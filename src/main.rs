#![allow(dead_code)]
use std::rc::Weak;

enum Size {}

enum Type {}

enum Test {
    AccessMin(i32),
    AccessNewer(String),
    AccessTime(i32),
    ChangeMin(i32),
    ChangeNewer(String),
    ChangeTime(i32),
    Empty,
    Executable,
    False,
    FsType(String),
    GroupId(i32),
    Group(String),
    InsensitiveLinkName(String), //TODO Pattern
    InsensitiveName(String),     //TODO Pattern
    InodeNumber(i32),
    InsensitivePath(String),
    InsensitiveRegex(String),
    Hardlinks(i32),
    ModifyMin(i32),
    ModifyTime(i32),
    ModifyNewer(String),
    Name(String),
    //NewerXY(Timestamp, Timestamp, String) // A whole can of worms
    NoGroup,
    NoUser,
    Path(String),
    Perm(String),
    PermAtLeast(String),
    PermAny(String),
    //Readable // Should always be true as this is an admin tool
    Regex(String),
    Samefile(String),
    Size(Size),
    True,
    Type(Type),
    Uid(i32),
    User(String),
    //Writable
    //XType(Type)
}

enum Action {
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

enum GlobalOption {
    Depth,
    MaxDepth(u32),
    MinDepth(u32),
}

enum PositionalOption {}

enum Operator {
    Precedence(Expression),
    Not(Expression),
    And(Expression),
    Or(Expression),
    List(Expression),
}

enum Expression {
    Operator(Weak<Operator>),
    Test(Test),
    Action(Action),
    Global(GlobalOption),
    Positional(PositionalOption),
}

fn main() {
    println!("Hello, world!");
}
