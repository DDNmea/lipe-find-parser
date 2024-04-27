#![allow(dead_code, unused_variables)]

use crate::ast::{Action, Comparison, Expression, Operator, PositionalOption, Size, Test};
use std::rc::Rc;
use std::time::Instant;

macro_rules! format_cmp {
    ($cmp:expr, $target:expr) => {
        match $cmp {
            Comparison::GreaterThan(n) => format!("(> ({}) {})", $target, n),
            Comparison::LesserThan(n) => format!("(< ({}) {})", $target, n),
            Comparison::Equal(n) => format!("(= ({}) {})", $target, n),
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
struct SchemeManager {
    invocation: Instant,

    init: Vec<String>,
    fini: Vec<String>,

    var_index: usize,
    vars: Vec<String>,
}

impl Default for SchemeManager {
    fn default() -> Self {
        SchemeManager {
            invocation: Instant::now(),
            init: vec![],
            fini: vec![],
            var_index: 0usize,
            vars: vec![],
        }
    }
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
        self.fini
            .push(format!("(close-port %lf3:port:{})", self.var_index));
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
    let (Comparison::GreaterThan(s) | Comparison::LesserThan(s) | Comparison::Equal(s)) = comp;

    let exp = match comp {
        Comparison::GreaterThan(n) => format!("(> ({}) {})", size_matching(n), n.byte_size()),
        Comparison::LesserThan(n) => format!("(< ({}) {})", size_matching(n), n.byte_size()),
        Comparison::Equal(n) => format!("(= ({}) {})", size_matching(n), n.byte_size()),
    };

    buffer.push_str(&exp);
}

impl Scheme for Test {
    fn compile(&self, buffer: &mut String, ctx: &mut SchemeManager) {
        match self {
            Test::False => buffer.push_str("#f"),
            Test::True => buffer.push_str("#t"),
            Test::Empty => buffer.push_str("(empty)"),
            Test::Writable => buffer.push_str("(writable)"),
            Test::Name(s) => {
                let match_ref = ctx.register_streq(s);
                buffer.push_str(&format!("(call-with-name %lf3:match:{})", match_ref))
            }
            Test::UserId(cmp) => buffer.push_str(&format_cmp!(cmp, "uid")),
            Test::Size(cmp) => compile_size_comp(buffer, &cmp),
            #[cfg(debug_assertions)]
            _ => buffer.push_str("(UNIMPLEMENTED)"),
            #[cfg(not(debug_assertions))]
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
            #[cfg(debug_assertions)]
            _ => buffer.push_str("(UNIMPLEMENTED)"),
            #[cfg(not(debug_assertions))]
            _ => todo!(),
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
            #[cfg(debug_assertions)]
            _ => buffer.push_str("(UNIMPLEMENTED)"),
            #[cfg(not(debug_assertions))]
            _ => todo!(),
        }
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
            Expression::Action(Action::Print),
        )));

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

(let * ({})
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
