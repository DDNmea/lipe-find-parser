pub mod ast;
mod find_parser;
mod permission_flags;
mod scheme;

pub use find_parser::parse;
pub use permission_flags::{Mode, SFlag};
pub use scheme::compile;

/// Convenience struct to collect the [ast::GlobalOption] passed on the command line.
#[derive(Debug)]
pub struct RunOptions {
    /// Perform the scan depth-first.
    pub depth: bool,
    //pub max_depth: u32,
    //pub min_depth: u32,
    /// The number on threads to used for scanning. This is on a per-device basis.
    pub threads: Option<u32>,
}

impl Default for RunOptions {
    fn default() -> Self {
        RunOptions {
            depth: false,
            //max_depth: u32::max_value(),
            //min_depth: u32::min_value(),
            threads: None,
        }
    }
}

impl RunOptions {
    /// Register a new value from the given [ast::GlobalOption].
    pub fn update(&mut self, option: &ast::GlobalOption) {
        match option {
            ast::GlobalOption::Depth => self.depth = true,
            //GlobalOption::MaxDepth(value) => self.max_depth = *value,
            //GlobalOption::MinDepth(value) => self.min_depth = *value,
            ast::GlobalOption::Threads(value) => self.threads = Some(*value),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
#[allow(unused_imports)]
mod parsing {
    use crate::ast::{
        Action, Comparison, Expression as Exp, GlobalOption, Operator as Ope, PositionalOption,
        Test, TimeSpec,
    };
    use crate::parse;
    use std::rc::Rc;

    #[test]
    fn test_parse_test() {
        let (_, exp) = parse("-amin 44").unwrap();
        assert_eq!(
            Exp::Test(Test::AccessTime(Comparison::Equal(TimeSpec::Minute(44)))),
            exp
        );

        let (_, exp) = parse("-true").unwrap();
        assert_eq!(Exp::Test(Test::True), exp);

        let (_, exp) = parse("-false").unwrap();
        assert_eq!(Exp::Test(Test::False), exp);

        let res = parse("-amin");
        assert!(res.is_err());

        let res = parse("-amin test");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_global_option() {
        let (opt, exp) = parse("-depth").unwrap();
        assert!(opt.depth);
        assert_eq!(Exp::Test(Test::True), exp);

        /* Disabled in LiPE
        let (opt, exp) = parse("-maxdepth 44").unwrap();
        assert_eq!(44u32, opt.max_depth);
        assert_eq!(Exp::Test(Test::True), exp);

        let (opt, exp) = parse("-mindepth 44").unwrap();
        assert_eq!(44u32, opt.min_depth);
        assert_eq!(Exp::Test(Test::True), exp);
        */

        let res = parse("-maxdepth -44");
        assert!(res.is_err());

        let res = parse("-mindepth -44");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_operator() {
        let (_, exp) = parse("! -true").unwrap();
        assert_eq!(Exp::Operator(Rc::new(Ope::Not(Exp::Test(Test::True)))), exp);

        let (_, exp) = parse("-true -o -false").unwrap();
        assert_eq!(
            Exp::Operator(Rc::new(Ope::Or(
                Exp::Test(Test::True),
                Exp::Test(Test::False)
            ))),
            exp
        );

        let (_, exp) = parse("-true -a -false").unwrap();
        assert_eq!(
            Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Test(Test::False)
            ))),
            exp
        );

        let (_, exp) = parse("-true -false").unwrap();
        assert_eq!(
            Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Test(Test::False)
            ))),
            exp
        );
    }

    #[test]
    fn test_parse_operator_precedence() {
        // and has a higher precedence than or, so we test this is reflected in the AST
        let (_, exp) = parse("-true -a -false -o -name test").unwrap();
        assert_eq!(
            Exp::Operator(Rc::new(Ope::Or(
                Exp::Operator(Rc::new(Ope::And(
                    Exp::Test(Test::True),
                    Exp::Test(Test::False)
                ))),
                Exp::Test(Test::Name(String::from("test")))
            ))),
            exp
        );

        let (_, exp) = parse("-true -o -false -a -name test").unwrap();
        assert_eq!(
            Exp::Operator(Rc::new(Ope::Or(
                Exp::Test(Test::True),
                Exp::Operator(Rc::new(Ope::And(
                    Exp::Test(Test::False),
                    Exp::Test(Test::Name(String::from("test"))),
                ))),
            ))),
            exp
        );

        let (_, exp) = parse("-true -a (-false -o -name test)").unwrap();
        assert_eq!(
            Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Operator(Rc::new(Ope::Or(
                    Exp::Test(Test::False),
                    Exp::Test(Test::Name(String::from("test"))),
                ))),
            ))),
            exp
        );

        let (_, exp) = parse("-true -a ! -false").unwrap();
        assert_eq!(
            Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Operator(Rc::new(Ope::Not(Exp::Test(Test::False)))),
            ))),
            exp
        );

        let (_, exp) = parse("! -true -o -false").unwrap();
        assert_eq!(
            Exp::Operator(Rc::new(Ope::Or(
                Exp::Operator(Rc::new(Ope::Not(Exp::Test(Test::True)))),
                Exp::Test(Test::False),
            ))),
            exp
        );

        let (_, exp) = parse("! ( -true -o -false )").unwrap();
        #[rustfmt::skip]
        assert_eq!(
            Exp::Operator(Rc::new(Ope::Not(
                Exp::Operator(Rc::new(Ope::Or(
                    Exp::Test(Test::True),
                    Exp::Test(Test::False)
            )))))),
            exp
        );
    }
}

#[cfg(test)]
/// Top level testing to ensure the compilation output is equal to LiPE find
/// Script to get the Scheme output from lipe_find3:
/// ```bash
/// lipe_find3 --debug / $@ 2>&1 | grep -A 200 -e scm_code | sed -s "s/.*'(use/(use/g" | head -n -3
/// ```
mod find_compilation {
    use crate::{
        ast::{Expression as Exp, Test},
        compile, parse, RunOptions,
    };

    fn parse_and_compile<S: AsRef<str>>(input: S) -> String {
        let mut clone = input.as_ref().to_string();
        let (opt, exp) =
            parse(&mut clone).unwrap_or((RunOptions::default(), Exp::Test(Test::False)));

        compile(&exp, &opt).unwrap()("/")
    }

    #[test]
    fn test_permission_check_basic_equal() {
        insta::assert_snapshot!(parse_and_compile("-perm 667"));
    }

    #[test]
    fn test_permission_check_basic_any() {
        insta::assert_snapshot!(parse_and_compile("-perm -244"));
    }

    #[test]
    fn test_permission_check_symbolic_equal_all_equal() {
        insta::assert_snapshot!(parse_and_compile("-perm a=x"));
    }

    #[test]
    fn test_permission_check_symbolic_equal_user_equal() {
        insta::assert_snapshot!(parse_and_compile("-perm u=w"));
    }

    #[test]
    fn test_permission_check_symbolic_equal_all_plus() {
        insta::assert_snapshot!(parse_and_compile("-perm a+x"));
    }

    #[test]
    fn test_permission_check_symbolic_equal_user_plus() {
        insta::assert_snapshot!(parse_and_compile("-perm u+w"));
    }

    #[test]
    fn test_permission_check_symbolic_equal_group_plus() {
        insta::assert_snapshot!(parse_and_compile("-perm g+w"));
    }

    #[test]
    fn test_permission_check_symbolic_equal_all_minus() {
        insta::assert_snapshot!(parse_and_compile("-perm a-x"));
    }

    #[test]
    fn test_permission_check_symbolic_equal_user_group_minus() {
        insta::assert_snapshot!(parse_and_compile("-perm ug-rw"));
    }

    #[test]
    fn test_permission_check_symbolic_at_least_user_plus() {
        insta::assert_snapshot!(parse_and_compile("-perm /u+w"));
    }

    #[test]
    fn test_print() {
        insta::assert_snapshot!(parse_and_compile("-print"));
    }

    #[test]
    fn test_print_null() {
        insta::assert_snapshot!(parse_and_compile("-print0"));
    }

    #[test]
    fn test_fprint() {
        insta::assert_snapshot!(parse_and_compile("-fprint filelist.out"));
    }

    #[test]
    fn test_printf() {
        insta::assert_snapshot!(parse_and_compile(
            "-printf \"%p,%U,%G,%m,%s,%A@,%C@,%T@,%{projid},%{fid}\\n\""
        ));
    }

    #[test]
    fn test_fprintf() {
        insta::assert_snapshot!(parse_and_compile(
            "-fprintf user_files.txt \"%p,%U,%{fid}\\n\""
        ));
    }
}
