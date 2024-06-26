use crate::{Mode, SFlag};
use std::rc::Rc;

/// Size of the integer used for size storage
type SizeType = u64;

/// Size reference unit definitions
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

/// Time reference units
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

/// Generic comparison type
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

/// Permission parameter when testing file modes.
#[derive(Debug, Clone, PartialEq)]
pub struct Permission(pub Mode);

/// Type of permission checking
#[derive(Debug, Clone, PartialEq)]
pub enum PermCheck {
    AtLeast(Permission),
    Any(Permission),
    Equal(Permission),
}

/// Special characters supported by format strings
///
/// A `\` character followed by any other character is treated as an ordinary character, so they both are printed.
#[derive(Clone, PartialEq, Debug)]
pub enum FormatSpecial {
    /// `\a` Alarm bell.
    Alarm,
    /// `\b` Backspace.
    Backspace,
    /// `\c` Stop printing from this format immediately and flush the output.
    Clear,
    /// `\f` Form feed.
    Form,
    /// `\n` Newline.
    Newline,
    /// `\r` Carriage return.
    CarriageReturn,
    /// `\t` Horizontal tab.
    TabHorizontal,
    /// `\v` Vertical tab.
    TabVertical,
    /// `\0` ASCII NUL.
    Null,
    /// `\\` A literal backslash (`\').
    Backslash,
    /// `\NNN` The character whose ASCII code is NNN (octal).
    Ascii(u16),
}

/// Format fields supported by format strings
#[derive(Clone, PartialEq, Debug)]
pub enum FormatField {
    /// `%%` A literal percent sign.
    Percent,
    /// `%a` File's last access time in the format returned by the C `ctime' function.
    Access,
    /// `%Ak` File's last access time in the format specified by k,
    AccessFormatted(char),
    /// `%b` The amount of disk space used for this file in 512-byte blocks.
    DiskSizeBlocks,
    /// `%c` File's last status change time in the format returned by the C `ctime' function.
    Change,
    /// `%Ck` File's last status change time in the format specified by k, which is the same as for `%A`.
    ChangeFormatted(char),
    /// `%d` File's depth in the directory tree; 0 means the file is a starting-point.
    Depth,
    /// `%D` The device number on which the file exists (the `st_dev` field of `struct stat`), in decimal.
    DeviceNumber,
    /// `%f` File's name with any leading directories removed (only the last element).
    Basename,
    /// `%F` Type of the filesystem the file is on; this value can be used for `-fstype`.
    FsType,
    /// `%g` File's group name, or numeric group ID if the group has no name.
    Group,
    /// `%G` File's numeric group ID.
    GroupId,
    /// `%h` Leading directories of file's name (all but the last element). If the file is in the current directory the `%h` specifier expands to ".".
    Parents,
    /// `%H` Starting-point under which file was found.
    StartingPoint,
    /// `%i` File's inode number (in decimal).
    InodeDecimal,
    /// `%k` The amount of disk space used for this file in 1K blocks
    DiskSizeKilos,
    /// `%l` Object of symbolic link (empty string if file is not a symbolic link).
    SymbolicTarget,
    /// `%m` File's permission bits (in octal). This option uses the `traditional' numbers which most Unix implementations use, but if your particular implementation uses an unusual ordering of octal permissions bits, you will see a difference between the actual value of the file's mode and the output of `%m`. Normally you will want to have a leading zero on this number, and to do this, you should use the # flag (as in, for example, `%#m').
    PermissionsOctal,
    /// `%M` File's permissions (in symbolic form, as for ls). This directive is supported in findutils 4.2.5 and later.
    PermissionsSymbolic,
    /// `%n` Number of hard links to file.
    Hardlinks,
    /// `%p` File's name.
    Name,
    /// `%P` File's name with the name of the starting-point under which it was found removed.
    NameWithoutStartingPoint,
    /// `%s` File's size in bytes.
    DiskSizeBytes,
    /// `%S` File's sparseness. This is calculated as `(BLOCK‐SIZE*st_blocks / st_size)`. The exact value you will get for an ordinary file of a certain length is system-depen‐ dent. However, normally sparse files will have values less than 1.0, and files which use indirect blocks may have a value which is greater than 1.0. The value used for BLOCKSIZE is system-dependent, but is usually 512 bytes. If the file size is zero, the value printed is undefined. On systems which lack support for st_blocks, a file's sparseness is assumed to be 1.0.
    Sparseness,
    /// `%t` File's last modification time in the format returned by the C `ctime` function.
    Modify,
    /// `%Tk` File's last modification time in the format specified by k, which is the same as for `%A`.
    ModifyFormatted(char),
    /// `%u` File's user name, or numeric user ID if the user has no name.
    User,
    /// `%U` File's numeric user ID.
    UserId,
    /// `%y` File's type (like in ls -l), U=unknown type (shouldn't happen)
    Type,
    /// `%Y` File's type (like `%y`), plus follow symlinks: L=loop, N=nonexistent
    TypeSymlink,
    /// `%Z` (SELinux only) file's security context.
    SecurityContext,
    /// `%{fid}` file FID
    FileId,
    /// `%{projid}` numerical project ID
    ProjectId,
    /// `%{mirror-count}` FLR mirror count
    MirrorCount,
    /// `%{stripe-count}` stripe count of last instantiated component
    StripeCount,
    /// `%{stripe-size}` stripe size of last instantiated component
    StripeSize,
    /// `%{xattr:NAME}` contents of NAME xattr as a string
    XAttr(String),
}

/// Abstraction over the possible elements making up a format string
#[derive(PartialEq, Debug, Clone)]
pub enum FormatElement {
    /// A regular string
    Literal(String),
    /// A format field
    Field(FormatField),
    /// A special character
    Special(FormatSpecial),
}

#[derive(Debug, Clone, PartialEq)]
/// Tests handled by LiPE when matching files to run [Action]s on
pub enum Test {
    AccessTime(Comparison<TimeSpec>),
    ChangeTime(Comparison<TimeSpec>),
    Empty,
    Executable,
    False,
    GroupId(Comparison<u32>),
    InodeNumber(Comparison<u32>),
    InsensitiveName(String),
    InsensitivePath(String),
    Links(Comparison<SizeType>),
    MirrorCount(Comparison<u32>),
    ModifyTime(Comparison<TimeSpec>),
    Name(String),
    Path(String),
    Perm(PermCheck),
    Pool(String),
    Readable,
    Size(Comparison<Size>),
    StripeCount(Comparison<u32>),
    True,
    Type(Vec<FileType>),
    UserId(Comparison<u32>),
    Writable,
    Xattr(String),
    XattrMatch(String, String),
    //XType(Type)
    // The following are not supported in the final scheme output
    //NewerXY(Timestamp, Timestamp, String) // A whole can of worms
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
/// Actions supported by the program
pub enum Action {
    //Exec(Vec<String>),
    //ExecOnAll(Vec<String>)
    //ExecOnParent(Vec<String>)
    //ExecOnAllParent(Vec<String>)
    /// Call ls on the matching paths and print to a file
    FileList(String),
    /// Output matching paths to a file
    FilePrint(String),
    /// Output matching paths to a file, terminated by a null byte
    FilePrintNull(String),
    /// Output matching paths to a file, with the given FORMAT
    FilePrintFormatted(String, Vec<FormatElement>),
    /// Call ls on the matching paths
    List,
    //Ask(Vec<String>)
    //AskDir(Vec<String>)
    /// Output matching paths
    Print,
    /// Output matching paths, terminated by a null byte
    PrintNull,
    /// Output matching with the given FORMAT
    PrintFormatted(Vec<FormatElement>),
    PrintFid,
    Prune,
    Quit,

    #[deprecated]
    /// Implicit printing directive
    ///
    /// Although the actions are the same, the original code handles implicit and explicit print
    /// differently. This is only inserted in the ast when no other action is present to match the
    /// original behaviour.
    DefaultPrint,
}

/// Options affecting the behaviour of the scan
///
/// They are expected to be present before any other options on the command
/// line but will only generate a warning if improperly declared.
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

/// Operators composing [Expression]s
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Precedence(Expression),
    Not(Expression),
    And(Expression, Expression),
    Or(Expression, Expression),
    List(Expression, Expression),
}

/// Top level object of the AST
#[derive(Clone, PartialEq)]
pub enum Expression {
    Operator(Rc<Operator>),
    Test(Test),
    Action(Action),
    Global(GlobalOption),
    Positional(PositionalOption),
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Expression::Operator(o) => formatter.write_str(&format!("{o:#?}")),
            Expression::Test(t) => formatter.write_str(&format!("Test({t:?})")),
            Expression::Action(a) => formatter.write_str(&format!("Action({a:?})")),
            Expression::Global(a) => formatter.write_str(&format!("Global({a:?})")),
            Expression::Positional(a) => formatter.write_str(&format!("Positional({a:?})")),
        }?;

        Ok(())
    }
}

impl Expression {
    /// Returns true if this expression contains an action at any part of its structure
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

    /// Checks the expression and returns true if it requires special handling by outputting to
    /// files or outputting lines that do not terminate by a newline character
    pub fn complex_frames(&self) -> bool {
        match self {
            // Check the actions of the expression
            Expression::Action(act) => match act {
                Action::PrintNull
                | Action::FileList(_)
                | Action::FilePrint(_)
                | Action::FilePrintFormatted(_, _)
                | Action::FilePrintNull(_) => true,
                // Returns true if a printf does not end with \n
                Action::PrintFormatted(format) => {
                    format.last().is_some_and(|el: &FormatElement| {
                        !matches!(el, FormatElement::Special(FormatSpecial::Newline))
                    })
                }
                _ => false,
            },

            // Descend recursively
            Expression::Operator(op) => match op.as_ref() {
                Operator::Precedence(e) | Operator::Not(e) => e.complex_frames(),
                Operator::And(e1, e2) | Operator::Or(e1, e2) | Operator::List(e1, e2) => {
                    e1.complex_frames() || e2.complex_frames()
                }
            },

            // Any other component is safe
            _ => false,
        }
    }
}
