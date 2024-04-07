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
