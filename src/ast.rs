#![allow(dead_code, unused_variables)]

use std::rc::Rc;

type SizeType = u64;
#[derive(Debug, Clone, PartialEq)]
pub enum Size {
    /// 1 byte. Called character in find.
    Byte(SizeType),
    /// 2 bytes
    Word(SizeType),
    /// 512 bytes
    Block(SizeType),
    /// 1024 bytes
    KiloByte(SizeType),
    MegaByte(SizeType),
    GigaByte(SizeType),
    TeraByte(SizeType),
}

/// Used to represent file sizes in tests
impl Size {
    /// The size unit multiplier
    pub fn mult(&self) -> SizeType {
        match self {
            Size::Byte(_) => 1,
            Size::Word(_) => 2,
            Size::Block(_) => 512,
            Size::KiloByte(_) => 1024,
            Size::MegaByte(_) => 1024 * 1024,
            Size::GigaByte(_) => 1024 * 1024 * 1024,
            Size::TeraByte(_) => 1024 * 1024 * 1024 * 1024,
        }
    }

    /// The amount of bytes of the described size
    pub fn byte_size(&self) -> SizeType {
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
pub enum TimeSpec {
    Second(SizeType),
    Minute(SizeType),
    Hour(SizeType),
    Day(SizeType),
}

impl TimeSpec {
    pub fn secs(&self) -> SizeType {
        match self {
            TimeSpec::Second(_) => 1,
            TimeSpec::Minute(_) => 60,
            TimeSpec::Hour(_) => 60 * 60,
            TimeSpec::Day(_) => 24 * 60 * 60,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comparison<T> {
    GreaterThan(T),
    LesserThan(T),
    Equal(T),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FileType {
    /// b      block (buffered) special
    Block,
    /// c      character (unbuffered) special
    Character,
    /// d      directory
    Directory,
    /// p      named pipe (FIFO)
    Pipe,
    /// f      regular file
    File,
    /// l      symbolic link
    ///
    /// this is never true if the -L option or the -follow  option is in effect,
    /// unless the symbolic link is broken.  If you want to search for symbolic
    /// links when -L is in effect, use -xtype.
    Link,
    /// s      socket
    Socket,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Test {
    AccessMin(Comparison<TimeSpec>),
    AccessTime(Comparison<TimeSpec>),
    ChangeMin(Comparison<TimeSpec>),
    ChangeTime(Comparison<TimeSpec>),
    Empty,
    Executable,
    False,
    GroupId(Comparison<u32>),
    InsensitiveName(String),
    InodeNumber(Comparison<u32>),
    InsensitivePath(String),
    Links(Comparison<SizeType>),
    ModifyMin(Comparison<TimeSpec>),
    ModifyTime(Comparison<TimeSpec>),
    Name(String),
    //NewerXY(Timestamp, Timestamp, String) // A whole can of worms
    Path(String),
    Perm(String),
    PermAtLeast(String),
    PermAny(String),
    Readable,
    Size(Comparison<Size>),
    True,
    Type(Vec<FileType>),
    UserId(Comparison<u32>),
    Writable,
    //XType(Type)
    // The following are not supported in the final scheme output
    AccessNewer(String),
    ChangeNewer(String),
    FsType(String),
    Group(String),
    InsensitiveLinkName(String),
    InsensitiveRegex(String),
    LinkName(String),
    ModifyNewer(String),
    NoGroup,
    NoUser,
    Regex(String),
    Samefile(String),
    User(String),
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
