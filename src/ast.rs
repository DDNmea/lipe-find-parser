#![allow(dead_code, unused_variables)]

use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Comparison {
    GreaterThan(u32),
    LesserThan(u32),
    Equal(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Size {}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {}

#[derive(Debug, Clone, PartialEq)]
pub enum Test {
    AccessMin(Comparison),
    AccessNewer(String),
    AccessTime(Comparison),
    ChangeMin(Comparison),
    ChangeNewer(String),
    ChangeTime(Comparison),
    Empty,
    Executable,
    False,
    FsType(String),
    GroupId(u32),
    Group(String),
    InsensitiveLinkName(String), //TODO Pattern
    InsensitiveName(String),     //TODO Pattern
    InodeNumber(Comparison),
    InsensitivePath(String),
    InsensitiveRegex(String),
    Hardlinks(u32),
    ModifyMin(Comparison),
    ModifyNewer(String),
    ModifyTime(Comparison),
    Name(String),
    //NewerXY(Timestamp, Timestamp, String) // A whole can of worms
    NoGroup,
    NoUser,
    Path(String),
    Perm(String),
    PermAtLeast(String),
    PermAny(String),
    Readable,
    Regex(String),
    Samefile(String),
    Size(String),
    True,
    Type(String),
    UserId(u32),
    User(String),
    Writable,
    //XType(Type)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Action {
    //Exec(Vec<String>),
    //ExecOnAll(Vec<String>)
    //ExecOnParent(Vec<String>)
    //ExecOnAllParent(Vec<String>)
    FileList(String),
    FilePrint(String),
    FilePrintNull(String),
    FilePrintFormatted(String, String),
    List,
    //Ask(Vec<String>)
    //AskDir(Vec<String>)
    Print,
    PrintNull,
    PrintFormatted(String),
    Prune,
    Quit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GlobalOption {
    Depth,
    MaxDepth(u32),
    MinDepth(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PositionalOption {
    XDev,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Precedence(Expression),
    Not(Expression),
    And(Expression, Expression),
    Or(Expression, Expression),
    List(Expression, Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Operator(Rc<Operator>),
    Test(Test),
    Action(Action),
    Global(GlobalOption),
    Positional(PositionalOption),
}

macro_rules! format_cmp {
    ($cmp:expr, $target:expr) => {
        match $cmp {
            Comparison::GreaterThan(n) => format!("(> ({}) {})", $target, n),
            Comparison::LesserThan(n) => format!("(< ({}) {})", $target, n),
            Comparison::Equal(n) => format!("(= ({}) {})", $target, n),
        }
    };
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SchemeManager {
    strings: Vec<String>,
}

impl SchemeManager {
    /// Record a string to compare to later in the program. The manager will keep track of the
    /// recorded strings and associate a unique index to each. They will correspond to match
    /// functions in the final LISP code.
    fn register_streq<S: AsRef<str>>(&mut self, cmp: S) -> usize {
        match self.strings.iter().position(|x| x == cmp.as_ref()) {
            Some(index) => index + 1,
            None => {
                self.strings.push(cmp.as_ref().to_string());
                self.strings.len()
            }
        }
    }
}

pub trait Scheme {
    fn compile(&self, buffer: &mut String, init: &mut SchemeManager);
}

impl Expression {
    pub fn str_comps<'a>(&'a self) -> Vec<&'a String> {
        let mut out = vec![];
        match self {
            Expression::Test(parameter) => {
                if let Test::AccessNewer(s)
                | Test::ChangeNewer(s)
                | Test::FsType(s)
                | Test::Group(s)
                | Test::InsensitiveLinkName(s)
                | Test::InsensitiveName(s)
                | Test::InsensitivePath(s)
                | Test::InsensitiveRegex(s)
                | Test::ModifyNewer(s)
                | Test::Name(s)
                | Test::Path(s)
                | Test::Perm(s)
                | Test::PermAtLeast(s)
                | Test::PermAny(s)
                | Test::Regex(s)
                | Test::Samefile(s)
                | Test::Size(s)
                | Test::Type(s)
                | Test::User(s) = parameter
                {
                    out.push(s)
                }
            }

            Expression::Operator(op) => match op.as_ref() {
                Operator::Precedence(e) | Operator::Not(e) => out.extend(e.str_comps()),
                Operator::And(e1, e2) | Operator::Or(e1, e2) | Operator::List(e1, e2) => {
                    out.extend(e1.str_comps());
                    out.extend(e2.str_comps());
                }
            },
            _ => (),
        }

        out
    }

    pub fn output_files<'a>(&'a self) -> Vec<&'a String> {
        let mut out = vec![];
        match self {
            Expression::Action(act) => match act {
                Action::FileList(f) | Action::FilePrint(f) | Action::FilePrintNull(f) => {
                    out.push(f)
                }
                Action::FilePrintFormatted(f, _) => out.push(f),
                _ => (),
            },

            Expression::Operator(op) => match op.as_ref() {
                Operator::Precedence(e) | Operator::Not(e) => out.extend(e.output_files()),
                Operator::And(e1, e2) | Operator::Or(e1, e2) | Operator::List(e1, e2) => {
                    out.extend(e1.output_files());
                    out.extend(e2.output_files());
                }
            },
            _ => (),
        }

        out
    }

    pub fn action(&self) -> bool {
        match self {
            Expression::Action(_) => true,

            Expression::Operator(op) => match op.as_ref() {
                Operator::Precedence(e) | Operator::Not(e) => e.action(),
                Operator::And(e1, e2) | Operator::Or(e1, e2) | Operator::List(e1, e2) => {
                    e1.action() || e2.action()
                }
            },
            _ => false,
        }
    }
}

impl Scheme for Test {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        match self {
            Test::False => buffer.push_str("#f"),
            Test::Name(s) => {
                let match_ref = ctx.register_streq(s);
                buffer.push_str(&format!("(call-with-name %lf3:match:{})", match_ref))
            }
            Test::AccessMin(cmp) => buffer.push_str(&format_cmp!(cmp, "uid")),
            _ => todo!(),
        }
    }
}

impl Scheme for Operator {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        match self {
            // This is technically an error but the List seems to be treated as an And in the
            // original lipe wrapper so that is what we are doing for now.
            Operator::And(lhs, rhs) | Operator::List(lhs, rhs) => {
                buffer.push_str("(and ");
                lhs.compile(buffer, ctx);
                rhs.compile(buffer, ctx);
                buffer.push_str(")");
            }
            Operator::Or(lhs, rhs) => {
                buffer.push_str("(or ");
                lhs.compile(buffer, ctx);
                rhs.compile(buffer, ctx);
                buffer.push_str(")");
            }
            Operator::Not(exp) => {
                buffer.push_str("(not ");
                exp.compile(buffer, ctx);
                buffer.push_str(")");
            }
            // We are not supposed to encounter explicit precendence in the AST
            Operator::Precedence(_) => unreachable!(),
        }
    }
}

impl Scheme for Action {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        todo!()
    }
}

impl Scheme for GlobalOption {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        todo!()
    }
}

impl Scheme for PositionalOption {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        todo!()
    }
}

impl Scheme for Expression {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        match self {
            Expression::Test(t) => t.compile(buffer, ctx),
            Expression::Operator(o) => o.as_ref().compile(buffer, ctx),
            _ => todo!(),
        }
    }
}

impl SchemeManager {
    fn compile(&self, buffer: &mut String) {
        for (index, string) in self.strings.iter().enumerate() {
            buffer.push_str(&format!(
                "(%lf3:match:{} (lambda (%lf3:str:0) (streq? \"{}\" %lf3:str:0)))",
                index, string
            ));
        }
    }
}

#[test]
fn test_scheme_manager_strings() {
    let mut m = SchemeManager::default();

    let index1 = m.register_streq("test*");
    let index2 = m.register_streq("*.txt");
    let index3 = m.register_streq("test*");

    assert!(index1 != index2);
    assert_eq!(index1, index3);
}
