mod ast;
mod nom_find;
mod winnow_find_tokenized;

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
        let res = parse("-amin 44");
        assert_eq!(Ok(Exp::Test(Test::AccessMin(Comparison::Equal(44)))), res);

        let res = parse("-true");
        assert_eq!(Ok(Exp::Test(Test::True)), res);

        let res = parse("-false");
        assert_eq!(Ok(Exp::Test(Test::False)), res);

        let res = parse("-amin");
        assert!(res.is_err());

        let res = parse("-amin test");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_global_option() {
        let res = parse("-depth").unwrap();
        assert_eq!(Exp::Global(GlobalOption::Depth), res);

        let res = parse("-maxdepth 44").unwrap();
        assert_eq!(Exp::Global(GlobalOption::MaxDepth(44)), res);

        let res = parse("-mindepth 44").unwrap();
        assert_eq!(Exp::Global(GlobalOption::MinDepth(44)), res);

        let res = parse("-maxdepth -44");
        assert!(res.is_err());

        let res = parse("-mindepth -44");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_operator() {
        let res = parse("! -true");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::Not(Exp::Test(Test::True))))),
            res
        );

        let res = parse("-true -o -false");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::Or(
                Exp::Test(Test::True),
                Exp::Test(Test::False)
            )))),
            res
        );

        let res = parse("-true -a -false");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Test(Test::False)
            )))),
            res
        );

        let res = parse("-true -false");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Test(Test::False)
            )))),
            res
        );
    }

    #[test]
    fn test_parse_operator_precedence() {
        // and has a higher precedence than or, so we test this is reflected in the AST
        let res = parse("-true -a -false -o -name test");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::Or(
                Exp::Operator(Rc::new(Ope::And(
                    Exp::Test(Test::True),
                    Exp::Test(Test::False)
                ))),
                Exp::Test(Test::Name(String::from("test")))
            )))),
            res
        );

        let res = parse("-true -o -false -a -name test");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::Or(
                Exp::Test(Test::True),
                Exp::Operator(Rc::new(Ope::And(
                    Exp::Test(Test::False),
                    Exp::Test(Test::Name(String::from("test"))),
                ))),
            )))),
            res
        );

        let res = parse("-true -a (-false -o -name test)");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Operator(Rc::new(Ope::Or(
                    Exp::Test(Test::False),
                    Exp::Test(Test::Name(String::from("test"))),
                ))),
            )))),
            res
        );

        let res = parse("-true -a ! -false");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Operator(Rc::new(Ope::Not(Exp::Test(Test::False)))),
            )))),
            res
        );

        let res = parse("! -true -o -false");
        assert_eq!(
            Ok(Exp::Operator(Rc::new(Ope::Or(
                Exp::Operator(Rc::new(Ope::Not(Exp::Test(Test::True)))),
                Exp::Test(Test::False),
            )))),
            res
        );

        let res = parse("! ( -true -o -false )");
        #[rustfmt::skip]
    assert_eq!(
        Ok(Exp::Operator(Rc::new(Ope::Not(
            Exp::Operator(Rc::new(Ope::Or(
                Exp::Test(Test::True),
                Exp::Test(Test::False)
        ))))))),
        res
    );
    }
}
