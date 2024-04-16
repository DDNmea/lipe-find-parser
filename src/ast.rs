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
struct SchemeManager {
    init: Vec<String>,
    fini: Vec<String>,

    var_index: usize,
    vars: Vec<String>,
}

impl SchemeManager {
    /// Record a string to compare to later in the program. The manager will keep track of the
    /// recorded strings and associate a unique index to each. They will correspond to match
    /// functions in the final LISP code.
    fn register_streq<S: AsRef<str>>(&mut self, cmp: S) -> usize {
        self.vars.push(format!(
            "(%lf3:match:{} (lambda (%lf3:str:{}) (streq? \"{}\" %lf3:str:{})))",
            self.var_index + 1,
            self.var_index,
            cmp.as_ref(),
            self.var_index
        ));

        self.var_index += 2;
        return self.var_index - 1;
    }

    fn register_file<S: AsRef<str>>(&mut self, cmp: S) -> usize {
        self.vars.push(format!(
            "(%lf3:port:{} (open-file \"{}\" \"w\"))",
            self.var_index,
            cmp.as_ref(),
        ));
        self.vars
            .push(format!("(%lf3:mutex:{} (make-mutex))", self.var_index + 1));
        self.vars.push(format!(
            "(%lf3:print:{} (make-printer %lf3:port:{}, %lf3:mutex:{} #\\x0a))",
            self.var_index + 2,
            self.var_index,
            self.var_index + 1
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
    fn str_comps<'a>(&'a self) -> Vec<&'a String> {
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

    fn output_files<'a>(&'a self) -> Vec<&'a String> {
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

impl Scheme for Test {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        match self {
            Test::False => buffer.push_str("#f"),
            Test::True => buffer.push_str("#t"),
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
            Action::Print => buffer.push_str("(print-relative-path)"),
            Action::FilePrint(dest) => {
                let var_id = ctx.register_file(dest);
                buffer.push_str(&format!("(call-with-relative-path %lf3:print:{})", var_id))
            }
            _ => todo!(),
        }
    }
}

impl Scheme for GlobalOption {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        todo!()
    }
}

impl Scheme for PositionalOption {
    fn compile(&self, buffer: &mut String, _: &mut SchemeManager) {
        todo!()
    }
}

impl Scheme for Expression {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        match self {
            Expression::Test(t) => t.compile(buffer, ctx),
            Expression::Action(a) => a.compile(buffer, ctx),
            Expression::Operator(o) => o.as_ref().compile(buffer, ctx),
            _ => todo!(),
        }
    }
}

pub fn compile<S: AsRef<str>>(exp: &Expression, path: S) -> String {
    let mut buffer = String::new();
    let mut manager = SchemeManager::default();

    if !exp.action() {
        let wrapper = Expression::Operator(Rc::new(Operator::And(
            exp.clone(),
            Expression::Action(Action::Print),
        )));

        wrapper.compile(&mut buffer, &mut manager);
    } else {
        exp.compile(&mut buffer, &mut manager);
    }

    format!(
        "(let * ({})
  (dynamic-wind
    (lambda () {})
    (lambda () (lipe-scan
        \"{}\"
        (lipe-getopt-client-mount-path)
        (lambda () {})
        (lipe-getopt-required-attrs)
        (lipe-getopt-thread-count)))
    (lambda () {})))",
        manager.vars(),
        manager.init(),
        path.as_ref(),
        buffer,
        manager.fini(),
    )
}
