#![allow(dead_code, unused_variables, deprecated)]

use crate::ast::{
    Action, Comparison, Expression, FileType, FormatElement, FormatField, FormatSpecial, Operator,
    PermCheck, PositionalOption, Size, Test, TimeSpec,
};
use crate::Mode;
use crate::SFlag;
use std::rc::Rc;

#[cfg(target_arch = "wasm32")]
use instant::SystemTime;
#[cfg(not(target_arch = "wasm32"))]
use std::time::SystemTime;

macro_rules! format_cmp {
    ($cmp:expr, $target:expr) => {
        match $cmp {
            Comparison::GreaterThan(n) => format!("(> ({}) {})", $target, n),
            Comparison::LesserThan(n) => format!("(< ({}) {})", $target, n),
            Comparison::Equal(n) => format!("(= ({}) {})", $target, n),
        }
    };

    ($cmp:expr, $lhs:expr, $rhs:expr) => {
        match $cmp {
            Comparison::GreaterThan(n) => format!("(> ({}) {})", $lhs(n), $rhs(n)),
            Comparison::LesserThan(n) => format!("(< ({}) {})", $lhs(n), $rhs(n)),
            Comparison::Equal(n) => format!("(= ({}) {})", $lhs(n), $rhs(n)),
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
struct SchemeManager {
    init: Vec<String>,
    fini: Vec<String>,

    var_index: usize,
    vars: Vec<String>,
}

impl Default for SchemeManager {
    fn default() -> Self {
        SchemeManager {
            init: vec![],
            fini: vec![],
            var_index: 0usize,
            vars: vec![],
        }
    }
}

fn is_pattern(input: &str) -> bool {
    input.contains('?') | input.contains('*') | input.contains('[')
}

impl SchemeManager {
    /// This method will record a string matching operation and create a function for it in the
    /// initialization of the program. If the given string is detected to be a patter, the fnmatch
    /// function will be used to compare the strings, else the streq function will be used.
    fn register_strcmp<S: AsRef<str>>(&mut self, cmp: S) -> usize {
        let matcher = if is_pattern(cmp.as_ref()) {
            "fnmatch"
        } else {
            "streq"
        };

        self.register_str_match(matcher, cmp.as_ref())
    }

    /// This method operates the same way as the above for case insensitive matches, using either
    /// fnmatch-ci or streq-ci
    fn register_ci_strcmp<S: AsRef<str>>(&mut self, cmp: S) -> usize {
        let matcher = is_pattern(cmp.as_ref())
            .then(|| "fnmatch-ci")
            .unwrap_or("streq-ci");

        self.register_str_match(matcher, cmp.as_ref())
    }

    /// Internal function used by register_*strcmp
    fn register_str_match(&mut self, matcher: &str, string: &str) -> usize {
        self.vars.push(format!(
            "(%lf3:match:{} (lambda (%lf3:str:{}) ({matcher}? \"{string}\" %lf3:str:{})))",
            self.var_index + 1,
            self.var_index,
            self.var_index
        ));

        self.var_index += 2;
        return self.var_index - 1;
    }

    fn register_file<S, T>(&mut self, filename: S, terminator: T) -> usize
    where
        S: AsRef<str>,
        T: AsRef<str>,
    {
        self.vars.push(format!(
            "(%lf3:port:{} (open-file \"{}\" \"w\"))",
            self.var_index,
            filename.as_ref(),
        ));
        self.fini
            .push(format!("(close-port %lf3:port:{})", self.var_index));
        self.vars
            .push(format!("(%lf3:mutex:{} (make-mutex))", self.var_index + 1));
        self.vars.push(format!(
            "(%lf3:print:{} (make-printer %lf3:port:{} %lf3:mutex:{} {}))",
            self.var_index + 2,
            self.var_index,
            self.var_index + 1,
            terminator.as_ref()
        ));

        self.var_index += 3;
        return self.var_index - 1;
    }

    fn register_port<S: AsRef<str>>(&mut self, terminator: S) -> usize {
        self.vars.push(format!(
            "(%lf3:port:{} (current-output-port))",
            self.var_index
        ));
        self.vars
            .push(format!("(%lf3:mutex:{} (make-mutex))", self.var_index + 1));
        self.vars.push(format!(
            "(%lf3:print:{} (make-printer %lf3:port:{} %lf3:mutex:{} {}))",
            self.var_index + 2,
            self.var_index,
            self.var_index + 1,
            terminator.as_ref()
        ));

        self.var_index += 3;
        return self.var_index - 1;
    }

    fn register_init<S: AsRef<str>>(&mut self, step: S) {
        self.init.push(step.as_ref().to_string())
    }

    fn register_fini<S: AsRef<str>>(&mut self, step: S) {
        self.fini.push(step.as_ref().to_string())
    }

    fn vars(&self) -> String {
        self.vars.join(" ")
    }

    fn init(&self) -> String {
        if self.init.is_empty() {
            String::from("#t")
        } else {
            self.init.join(" ")
        }
    }

    fn fini(&self) -> String {
        if self.fini.is_empty() {
            String::from("#t")
        } else {
            self.fini.join(" ")
        }
    }
}

trait Scheme {
    fn compile(&self, buffer: &mut String, init: &mut SchemeManager);
}

impl Expression {
    fn action(&self) -> bool {
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

fn size_matching(size: &Size) -> String {
    match size {
        Size::Byte(_) => String::from("size"),
        Size::Word(_)
        | Size::Block(_)
        | Size::KiloByte(_)
        | Size::MegaByte(_)
        | Size::GigaByte(_)
        | Size::TeraByte(_) => format!("round-up-power-of-2 (size) {}", size.mult()),
    }
}

fn compile_size_comp(buffer: &mut String, comp: &Comparison<Size>) {
    buffer.push_str(&format_cmp!(comp, size_matching, Size::byte_size));
}

fn compile_time_comp(buffer: &mut String, field: &str, comp: &Comparison<TimeSpec>) {
    let secs = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let quotient = move |t: &TimeSpec| format!("quotient (- {secs} ({field})) {}", t.secs());
    let count = |t: &TimeSpec| {
        let (TimeSpec::Second(s) | TimeSpec::Minute(s) | TimeSpec::Hour(s) | TimeSpec::Day(s)) = t;
        s.clone()
    };

    buffer.push_str(&format_cmp!(comp, quotient, count));
}

static S_IFMT: SFlag = SFlag::S_IFMT;
fn compile_type_comp(buffer: &mut String, filetype: &FileType) {
    buffer.push_str(&format!(
        "(= (logand (mode) {}) {})",
        S_IFMT.bits(),
        filetype.octal().bits()
    ))
}

fn compile_type_list_comp(buffer: &mut String, filetypes: &Vec<FileType>) {
    let comps: Vec<String> = filetypes
        .iter()
        .map(|tp| {
            format!(
                "(= (logand (mode) {}) {})",
                S_IFMT.bits(),
                tp.octal().bits()
            )
        })
        .collect();

    match comps.len() {
        1 => buffer.push_str(comps.first().unwrap()),
        _ => buffer.push_str(&format!("(or {})", comps.join(" "))),
    }
}

fn compile_perm_check(buffer: &mut String, check: &PermCheck) {
    let (PermCheck::Any(p) | PermCheck::AtLeast(p) | PermCheck::Equal(p)) = check;

    let perm_mask = (Mode::S_ISUID
        | Mode::S_ISGID
        | Mode::S_ISVTX
        | Mode::S_IRWXU
        | Mode::S_IRWXG
        | Mode::S_IRWXO)
        .bits();

    let perm = p.0.bits();

    let code = match check {
        PermCheck::Equal(p) => format!("(= (logand (mode) {}) {})", perm_mask, perm),
        PermCheck::AtLeast(p) => format!("(= (logand (mode) {}) {})", perm, perm),
        PermCheck::Any(p) => format!("(not (= (logand (mode) {}) 0))", perm),
    };

    buffer.push_str(&code)
}

fn literal(special: &FormatSpecial) -> String {
    match special {
        FormatSpecial::Alarm => "\\a".to_string(),
        FormatSpecial::Ascii(val) => format!("{}", char::from_u32(*val as u32).unwrap_or('0')),
        FormatSpecial::Backslash => "\\".to_string(),
        FormatSpecial::Backspace => "\\b".to_string(),
        FormatSpecial::CarriageReturn => "\\r".to_string(),
        FormatSpecial::Clear => "\\c".to_string(),
        FormatSpecial::Form => "\\f".to_string(),
        FormatSpecial::Newline => "\\n".to_string(),
        FormatSpecial::Null => "\\0".to_string(),
        FormatSpecial::TabHorizontal => "\\t".to_string(),
        FormatSpecial::TabVertical => "\\v".to_string(),
    }
}

fn placeholder(field: &FormatField) -> &'static str {
    match field {
        FormatField::Access
        | FormatField::Basename
        | FormatField::Change
        | FormatField::FileId
        | FormatField::Group
        | FormatField::Modify
        | FormatField::Name
        | FormatField::NameWithoutStartingPoint
        | FormatField::Parents
        | FormatField::StartingPoint
        | FormatField::Type
        | FormatField::User
        | FormatField::XAttr(_) => "~a",

        FormatField::DiskSizeBlocks
        | FormatField::DiskSizeBytes
        | FormatField::DiskSizeKilos
        | FormatField::GroupId
        | FormatField::Hardlinks
        | FormatField::InodeDecimal
        | FormatField::MirrorCount
        | FormatField::ProjectId
        | FormatField::StripeCount
        | FormatField::StripeSize
        | FormatField::UserId => "~d",

        FormatField::PermissionsOctal => "~o",

        FormatField::Sparseness => "~f",

        FormatField::Percent => "%",

        FormatField::AccessFormatted(f)
        | FormatField::ChangeFormatted(f)
        | FormatField::ModifyFormatted(f) => match f {
            '@' => "~d",
            _ => "~a",
        },

        FormatField::Depth
        | FormatField::DeviceNumber
        | FormatField::FsType
        | FormatField::SymbolicTarget
        | FormatField::PermissionsSymbolic
        | FormatField::TypeSymlink
        | FormatField::SecurityContext => "x",
    }
}

fn snippet(field: &FormatField) -> Option<String> {
    let snippet = match field {
        FormatField::Percent => "".to_string(),
        FormatField::Access => "atime".to_string(),
        FormatField::Change => "ctime".to_string(),
        FormatField::Modify => "mtime".to_string(),
        FormatField::DiskSizeBlocks => "blocks".to_string(),
        FormatField::Basename => "name".to_string(),
        FormatField::Group => "group".to_string(),
        FormatField::GroupId => "gid".to_string(),
        FormatField::Parents => "call-with-relative-path dirname".to_string(),
        FormatField::StartingPoint => "lipe-scan-client-mount-path".to_string(),
        FormatField::InodeDecimal => "ino".to_string(),
        FormatField::DiskSizeKilos => "quotient (+ (blocks) 1) 2".to_string(),
        FormatField::PermissionsOctal => "logand (mode) #o07777".to_string(),
        FormatField::Hardlinks => "nlink".to_string(),
        FormatField::Name => "absolute-path".to_string(),
        FormatField::NameWithoutStartingPoint => "relative-path".to_string(),
        FormatField::DiskSizeBytes => "size".to_string(),
        FormatField::Sparseness => "/ (* 512 (blocks)) (size)".to_string(),
        FormatField::User => "user".to_string(),
        FormatField::UserId => "uid".to_string(),
        FormatField::Type => "type->char (type)".to_string(),
        FormatField::FileId => "file-fid".to_string(),
        FormatField::StripeSize => "lov-stripe-size".to_string(),
        FormatField::StripeCount => "lov-stripe-count".to_string(),
        FormatField::MirrorCount => "lov-mirror-count".to_string(),
        FormatField::ProjectId => "projid".to_string(),

        FormatField::AccessFormatted(f) => match f {
            '@' => "atime".to_string(),
            f => format!("strftime \"%{f}\" (localtime (atime))"),
        }
        .to_string(),

        FormatField::ChangeFormatted(f) => match f {
            '@' => "ctime".to_string(),
            f => format!("strftime \"%{f}\" (localtime (ctime))"),
        },

        FormatField::ModifyFormatted(f) => match f {
            '@' => "mtime".to_string(),
            f => format!("strftime \"%{f}\" (localtime (mtime))"),
        },

        FormatField::XAttr(attr) => format!("or (xattr-ref-string \"{attr}\") \"\"").to_owned(),

        FormatField::Depth
        | FormatField::DeviceNumber
        | FormatField::FsType
        | FormatField::SymbolicTarget
        | FormatField::PermissionsSymbolic
        | FormatField::TypeSymlink
        | FormatField::SecurityContext => "".to_string(),
    };

    (!snippet.is_empty()).then(move || format!("({})", snippet))
}

impl Scheme for Vec<FormatElement> {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        let template = self
            .iter()
            .map(|el| match el {
                FormatElement::Literal(s) => s.clone(),
                FormatElement::Field(f) => placeholder(f).to_string(),
                FormatElement::Special(v) => literal(v),
            })
            .collect::<Vec<String>>()
            .join("");

        let items = self
            .iter()
            .filter_map(|el| match el {
                FormatElement::Literal(s) => None,
                FormatElement::Field(f) => snippet(f),
                FormatElement::Special(v) => None,
            })
            .collect::<Vec<String>>()
            .join(" ");

        buffer.push_str(&format!("(format #f \"{template}\" {items})"));
    }
}

impl Scheme for Test {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        match self {
            Test::AccessTime(cmp) => compile_time_comp(buffer,"atime",&cmp),
            Test::ChangeTime(cmp) => compile_time_comp(buffer, "ctime", &cmp),
            Test::Empty => buffer.push_str("(empty)"),
            Test::Executable => buffer.push_str("(executable)"),
            Test::False => buffer.push_str("#f"),
            Test::GroupId(cmp) => buffer.push_str(&format_cmp!(cmp, "gid")),
            Test::InodeNumber(cmp) => buffer.push_str(&format_cmp!(cmp, "ino")),
            Test::InsensitiveName(s) => buffer.push_str(&format!("(call-with-name %lf3:match:{})", ctx.register_ci_strcmp(s))),
            Test::InsensitivePath(s) => buffer.push_str(&format!("(call-with-relative-path %lf3:match:{})", ctx.register_ci_strcmp(s))),
            Test::Links(cmp) => buffer.push_str(&format_cmp!(cmp, "nlink")),
            Test::ModifyTime(cmp) => compile_time_comp(buffer, "mtime", &cmp),
            Test::Name(s) => buffer.push_str(&format!("(call-with-name %lf3:match:{})", ctx.register_strcmp(s))),
            Test::Path(s) => buffer.push_str(&format!("(call-with-relative-path %lf3:match:{})", ctx.register_strcmp(s))),
            Test::Readable => buffer.push_str("(readable)"),
            Test::Size(cmp) => compile_size_comp(buffer, &cmp),
            Test::True => buffer.push_str("#t"),
            Test::Type(list) => compile_type_list_comp(buffer, list),
            Test::UserId(cmp) => buffer.push_str(&format_cmp!(cmp, "uid")),
            Test::Writable => buffer.push_str("(writable)"),
            Test::Perm(check) => compile_perm_check(buffer, check),

            // The following are tests defined by GNU find that are not supported either by LiPE or
            // exfind
            Test::AccessNewer(_) // LiPE support
            | Test::ChangeNewer(_) // LiPE support
            | Test::FsType(_) // LiPE support
            | Test::Group(_) // exfind - we need to figure out what the remote gid is
            | Test::InsensitiveLinkName(_) // LiPE support
            | Test::InsensitiveRegex(_) // LiPE support
            | Test::LinkName(_) // LiPE support
            | Test::ModifyNewer(_) // LiPE support
            | Test::NoGroup // LiPE support
            | Test::NoUser // LiPE support
            | Test::Regex(_) // LiPE support
            | Test::Samefile(_) // LiPE support
            | Test::User(_) // exfind - we need to figure out the remote uid

            => {
                log::error!("You have used a test that is not supported by this program.");
                todo!()
            }
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
                buffer.push_str(" ");
                rhs.compile(buffer, ctx);
                buffer.push_str(")");
            }
            Operator::Or(lhs, rhs) => {
                buffer.push_str("(or ");
                lhs.compile(buffer, ctx);
                buffer.push_str(" ");
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
        match self {
            Action::DefaultPrint => buffer.push_str("(print-relative-path)"),
            Action::Print => {
                let port = ctx.register_port("#\\x0a");
                buffer.push_str(&format!("(call-with-relative-path %lf3:print:{port})"));
            }

            Action::PrintNull => {
                let port = ctx.register_port("#\\x00");
                buffer.push_str(&format!("(call-with-relative-path %lf3:print:{port})"));
            }

            Action::FilePrint(dest) => {
                let var_id = ctx.register_file(dest, "#\\x0a");
                buffer.push_str(&format!("(call-with-relative-path %lf3:print:{})", var_id))
            }

            Action::FilePrintNull(dest) => {
                let var_id = ctx.register_file(dest, "#\\x00");
                buffer.push_str(&format!("(call-with-relative-path %lf3:print:{})", var_id))
            }

            Action::PrintFormatted(elements) => {
                let port = ctx.register_port("#f");
                buffer.push_str(&format!("(%lf3:print:{port} "));
                elements.compile(buffer, ctx);
                buffer.push_str(&format!(")"));
            }

            Action::FilePrintFormatted(dest, elements) => {
                let port = ctx.register_file(dest, "#f");
                buffer.push_str(&format!("(%lf3:print:{port} "));
                elements.compile(buffer, ctx);
                buffer.push_str(&format!(")"));
            }

            Action::Quit => buffer.push_str("(lipe-scan-break 0)"),

            Action::Prune | Action::List | Action::FileList(_) => log::error!("Unsupported action"),
        }
    }
}

impl Scheme for PositionalOption {
    fn compile(&self, buffer: &mut String, _: &mut SchemeManager) {
        match self {
            #[cfg(debug_assertions)]
            _ => buffer.push_str("(UNIMPLEMENTED)"),
            #[cfg(not(debug_assertions))]
            _ => todo!(),
        }
    }
}

impl Scheme for Expression {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        match self {
            Expression::Test(t) => t.compile(buffer, ctx),
            Expression::Action(a) => a.compile(buffer, ctx),
            Expression::Operator(o) => o.as_ref().compile(buffer, ctx),
            Expression::Positional(p) => p.compile(buffer, ctx),
            Expression::Global(_) => unreachable!(),
        }

        log::debug!("{:?} => {buffer}", self);
    }
}

/// Returns a closure that will return the code needed to parse an MDT when given a path towards
/// this MDT. This allows to compile an expression once and insert the MDT path after the fact
pub fn compile<S: AsRef<str>>(
    exp: &Expression,
    options: &crate::RunOptions,
) -> impl Fn(S) -> String {
    let mut buffer = String::new();
    let mut manager = SchemeManager::default();

    if !exp.action() {
        let wrapper = Expression::Operator(Rc::new(Operator::And(
            exp.clone(),
            Expression::Action(Action::DefaultPrint),
        )));

        log::debug!("Compiling {:?}", exp);
        wrapper.compile(&mut buffer, &mut manager);
    } else {
        exp.compile(&mut buffer, &mut manager);
    }

    let options = options
        .threads
        .and_then(|c| Some(c.to_string()))
        .unwrap_or(String::from("(lipe-getopt-thread-count)"));
    move |mdt: S| {
        format!(
            "(use-modules (lipe) (lipe find))

(let* ({})
  (dynamic-wind
    (lambda () {})
    (lambda () (lipe-scan
        \"{}\"
        (lipe-getopt-client-mount-path)
        (lambda () {})
        (lipe-getopt-required-attrs)
        {}))
    (lambda () {})))",
            manager.vars(),
            manager.init(),
            mdt.as_ref(),
            buffer,
            options,
            manager.fini(),
        )
    }
}
