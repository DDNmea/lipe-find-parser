mod ast;
mod nom_find;
mod winnow_find_tokenized;

pub use ast::compile;
pub use winnow_find_tokenized::parse;

#[cfg(test)]
mod parsing {
    use crate::ast::{
        Action, Comparison, Expression as Exp, GlobalOption, Operator as Ope, PositionalOption,
        Test,
    };
    use crate::parse;
    use std::rc::Rc;

    #[test]
    fn test_parse_test() {
        let (_, exp) = parse("-amin 44").unwrap();
        assert_eq!(Exp::Test(Test::AccessMin(Comparison::Equal(44))), exp);

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
        assert_eq!(vec![GlobalOption::Depth], opt);
        assert_eq!(Exp::Test(Test::True), exp);

        let (opt, exp) = parse("-maxdepth 44").unwrap();
        assert_eq!(vec![GlobalOption::MaxDepth(44)], opt);
        assert_eq!(Exp::Test(Test::True), exp);

        let (opt, exp) = parse("-mindepth 44").unwrap();
        assert_eq!(vec![GlobalOption::MinDepth(44)], opt);
        assert_eq!(Exp::Test(Test::True), exp);

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
