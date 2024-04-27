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
    /// 1 byte. Called character in find.
    Byte(u32),
    /// 2 bytes
    Word(u32),
    /// 512 bytes
    Block(u32),
    /// 1024 bytes
    KiloByte(u32),
    MegaByte(u32),
    GigaByte(u32),
    TeraByte(u32),
}

/// Used to represent file sizes in tests
impl Size {
    /// The size unit multiplier
    pub fn mult(&self) -> u32 {
        match self {
            Size::Byte(_) => 1u32,
            Size::Word(_) => 2u32,
            Size::Block(_) => 512u32,
            Size::KiloByte(_) => 1024u32,
            Size::MegaByte(_) => 1024 * 1024u32,
            Size::GigaByte(_) => 1024 * 1024 * 1024u32,
            Size::TeraByte(_) => 1024 * 1024 * 1024 * 1024u32,
        }
    }

    /// The amount of bytes of the described size
    pub fn byte_size(&self) -> u32 {
        let (Size::Byte(s)
        | Size::Word(s)
        | Size::Block(s)
        | Size::KiloByte(s)
        | Size::MegaByte(s)
        | Size::GigaByte(s)
        | Size::TeraByte(s)) = self;

        s * self.mult()
    }
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
