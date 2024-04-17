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
    GroupId(Comparison),
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
    UserId(Comparison),
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

/// Options affecting the behaviour of the scan. They are expected to be present before any other
/// options on the command line but will only generate a warning if improperly declared.
#[derive(Debug, Clone, PartialEq)]
pub enum GlobalOption {
    Depth,
    MaxDepth(u32),
    MinDepth(u32),

    /// Threads to use for each scan. This is a LiPE-specific option.
    Threads(u32),
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
