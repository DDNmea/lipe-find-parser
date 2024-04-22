#![allow(dead_code, unused_variables)]

use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum TimeRecord<T> {
    GreaterThan(T),
    LesserThan(T),
    Equal(T),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Size {
    Block(u32),
    Bytes(u32),
    Word(u32),
    KiloBytes(u32),
    MegaBytes(u32),
    GigaBytes(u32),
    TeraBytes(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comparison<T> {
    GreaterThan(T),
    LesserThan(T),
    Equal(T),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {}

#[derive(Debug, Clone, PartialEq)]
pub enum Test {
    AccessMin(Comparison<u32>),
    AccessNewer(String),
    AccessTime(Comparison<u32>),
    ChangeMin(Comparison<u32>),
    ChangeNewer(String),
    ChangeTime(Comparison<u32>),
    Empty,
    Executable,
    False,
    FsType(String),
    GroupId(Comparison<u32>),
    Group(String),
    InsensitiveLinkName(String), //TODO Pattern
    InsensitiveName(String),     //TODO Pattern
    InodeNumber(Comparison<u32>),
    InsensitivePath(String),
    InsensitiveRegex(String),
    Hardlinks(u32),
    ModifyMin(Comparison<u32>),
    ModifyNewer(String),
    ModifyTime(Comparison<u32>),
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
    Size(Comparison<Size>),
    True,
    Type(String),
    UserId(Comparison<u32>),
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
