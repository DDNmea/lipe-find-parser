#![allow(dead_code, unused_variables)]

use crate::{Mode, SFlag};
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

impl FileType {
    pub fn octal(&self) -> SFlag {
        match self {
            FileType::Directory => SFlag::S_IFDIR,
            FileType::Character => SFlag::S_IFCHR,
            FileType::Block => SFlag::S_IFBLK,
            FileType::File => SFlag::S_IFREG,
            FileType::Pipe => SFlag::S_IFIFO,
            FileType::Link => SFlag::S_IFLNK,
            FileType::Socket => SFlag::S_IFSOCK,
        }
    }
}

/// Permission parameter when testing file modes. We wrap around [nix::sys::stat::Mode] for the
/// ease of use and convenient display parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct Permission(pub Mode);

#[derive(Debug, Clone, PartialEq)]
pub enum PermCheck {
    AtLeast(Permission),
    Any(Permission),
    Equal(Permission),
}

#[derive(Clone, PartialEq, Debug)]
pub enum FormatSpecial {
    // A `\' character followed by any other character is treated as an ordinary character, so they both are printed.
    Alarm,          // \a Alarm bell.
    Backspace,      // \b Backspace.
    Clear,          // \c Stop printing from this format immediately and flush the output.
    Form,           // \f Form feed.
    Newline,        // \n Newline.
    CarriageReturn, // \r Carriage return.
    TabHorizontal,  // \t Horizontal tab.
    TabVertical,    // \v Vertical tab.
    Null,           // \0 ASCII NUL.
    Backslash,      // \\ A literal backslash (`\').
    Ascii(u16),     // \NNN The character whose ASCII code is NNN (octal).
}

#[derive(Clone, PartialEq, Debug)]
pub enum FormatField {
    Percent,                  // %% A literal percent sign.
    Access, // %a File's last access time in the format returned by the C `ctime' function.
    AccessFormatted(char), // %Ak File's last access time in the format specified by k,
    DiskSizeBlocks, // %b The amount of disk space used for this file in 512-byte blocks.
    Change, // %c File's last status change time in the format returned by the C `ctime' function.
    ChangeFormatted(char), // %Ck File's last status change time in the format specified by k, which is the same as for %A.
    Depth, // %d File's depth in the directory tree; 0 means the file is a starting-point.
    DeviceNumber, // %D The device number on which the file exists (the st_dev field of struct stat), in decimal.
    Basename,     // %f File's name with any leading directories removed (only the last element).
    FsType,       // %F Type of the filesystem the file is on; this value can be used for -fstype.
    Group,        // %g File's group name, or numeric group ID if the group has no name.
    GroupId,      // %G File's numeric group ID.
    Parents, // %h Leading directories of file's name (all but the last element). If the file is in the current directory the %h specifier expands to ".".
    StartingPoint, // %H Starting-point under which file was found.
    InodeDecimal, // %i File's inode number (in decimal).
    DiskSizeKilos, // %k The amount of disk space used for this file in 1K blocks
    SymbolicTarget, // %l Object of symbolic link (empty string if file is not a symbolic link).
    PermissionsOctal, // %m File's permission bits (in octal). This option uses the `traditional' numbers which most Unix implementations use, but if your particular implementation uses an unusual ordering of octal permissions bits, you will see a difference between the actual value of the file's mode and the output of %m. Normally you will want to have a leading zero on this number, and to do this, you should use the # flag (as in, for example, `%#m').
    PermissionsSymbolic, // %M File's permissions (in symbolic form, as for ls). This directive is supported in findutils 4.2.5 and later.
    Hardlinks,           // %n Number of hard links to file.
    Name,                // %p File's name.
    NameWithoutStartingPoint, // %P File's name with the name of the starting-point under which it was found removed.
    DiskSizeBytes,            // %s File's size in bytes.
    Sparseness, // %S File's sparseness. This is calculated as (BLOCK‐ SIZE*st_blocks / st_size). The exact value you will get for an ordinary file of a certain length is system-depen‐ dent. However, normally sparse files will have values less than 1.0, and files which use indirect blocks may have a value which is greater than 1.0. The value used for BLOCKSIZE is system-dependent, but is usually 512 bytes. If the file size is zero, the value printed is undefined. On systems which lack support for st_blocks, a file's sparseness is assumed to be 1.0.
    Modify, // %t File's last modification time in the format returned by the C `ctime' function.
    ModifyFormatted(char), // %Tk File's last modification time in the format specified by k, which is the same as for %A.
    User,                  // %u File's user name, or numeric user ID if the user has no name.
    UserId,                // %U File's numeric user ID.
    Type,                  // %y File's type (like in ls -l), U=unknown type (shouldn't happen)
    TypeSymlink,           // %Y File's type (like %y), plus follow symlinks: L=loop, N=nonexistent
    SecurityContext,       // %Z (SELinux only) file's security context.
    FileId,                // %{fid}, file FID
    ProjectId,             // %{projid} numerical project ID
    MirrorCount,           // %{mirror-count}, FLR mirror count
    StripeCount,           // %{stripe-count}, stripe count of last instantiated component
    StripeSize,            // %{stripe-size}, stripe size of last instantiated component
    XAttr(String),         // %{xattr:NAME} contents of NAME xattr as a string
}

#[derive(PartialEq, Debug, Clone)]
pub enum FormatElement {
    Literal(String),
    Field(FormatField),
    Special(FormatSpecial),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Test {
    AccessTime(Comparison<TimeSpec>),
    ChangeTime(Comparison<TimeSpec>),
    Empty,
    Executable,
    False,
    GroupId(Comparison<u32>),
    InsensitiveName(String),
    InodeNumber(Comparison<u32>),
    InsensitivePath(String),
    Links(Comparison<SizeType>),
    ModifyTime(Comparison<TimeSpec>),
    Name(String),
    //NewerXY(Timestamp, Timestamp, String) // A whole can of worms
    Path(String),
    Perm(PermCheck),
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
    PrintFormatted(Vec<FormatElement>),
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
