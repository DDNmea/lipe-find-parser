use std::collections::HashMap;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct OpenPort {
    port: usize,
    mutex: usize,
}

/// Collection of methods the code generation uses to keep track of IDs and initialization
pub trait SchemeManager {
    fn get_printer(&mut self, terminator: &str) -> String;

    fn get_file_printer(&mut self, filename: &str, terminator: &str) -> String;

    fn get_matcher(&mut self, pattern: &str, insensitive: bool) -> String;
    fn definitions(&self) -> String;
    fn initialization(&self) -> String;
    fn terminate(&self) -> String;
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalSchemeManager {
    /// List of instructions to run before
    init: Vec<String>,
    fini: Vec<String>,

    var_index: usize,
    vars: Vec<String>,

    /// The default output port. Defaults to None when not initialized.
    default_port: Option<OpenPort>,

    /// List of opened files with their associated ports
    files: HashMap<String, OpenPort>,

    /// Registry of defined printers. The keys contain the port used along with the termination - a
    /// string to append to each printed content.
    printers: HashMap<(OpenPort, String), usize>,

    /// String matches. The map key contains the string to match and the case-insensitiveness (true
    /// => case insensitive). The integer value is the reference of the match function assigned if
    /// the pattern was previously encountered.
    matches: HashMap<(String, bool), usize>,
}

impl Default for LocalSchemeManager {
    fn default() -> Self {
        LocalSchemeManager {
            init: vec![],
            fini: vec![],
            var_index: 0usize,
            vars: vec![],

            default_port: None,
            files: HashMap::new(),
            printers: HashMap::new(),
            matches: HashMap::new(),
        }
    }
}

fn is_pattern(input: &str) -> bool {
    input.contains('?') | input.contains('*') | input.contains('[')
}

impl LocalSchemeManager {
    fn init_default_port(&mut self) -> OpenPort {
        if self.default_port.is_none() {
            self.vars.push(format!(
                "(%lf3:port:{} (current-output-port))",
                self.var_index
            ));

            self.vars
                .push(format!("(%lf3:mutex:{} (make-mutex))", self.var_index + 1));

            self.default_port = Some(OpenPort {
                port: self.var_index,
                mutex: self.var_index + 1,
            });

            self.var_index += 2;
        }

        self.default_port.as_ref().unwrap().clone()
    }

    fn register_printer(&mut self, port: OpenPort, terminator: String) -> usize {
        let key = (port.clone(), terminator.clone());

        if self.printers.get(&key).is_none() {
            self.vars.push(format!(
                "(%lf3:print:{} (make-printer %lf3:port:{} %lf3:mutex:{} {}))",
                self.var_index, port.port, port.mutex, terminator
            ));

            self.printers.insert(key.clone(), self.var_index);

            self.var_index += 1;
        }

        self.printers.get(&key).unwrap().clone()
    }

    fn init_file_port(&mut self, filename: String) -> OpenPort {
        if self.files.get(&filename).is_none() {
            self.vars.push(format!(
                "(%lf3:port:{} (open-file \"{}\" \"w\"))",
                self.var_index, filename,
            ));
            self.fini
                .push(format!("(close-port %lf3:port:{})", self.var_index));
            self.vars
                .push(format!("(%lf3:mutex:{} (make-mutex))", self.var_index + 1));

            self.files.insert(
                filename.clone(),
                OpenPort {
                    port: self.var_index,
                    mutex: self.var_index + 1,
                },
            );
            self.var_index += 2;
        }

        self.files.get(&filename).unwrap().clone()
    }

    /// Internal function used by get_matcher
    fn register_str_match(&mut self, pattern: &str, insensitive: bool) -> usize {
        let matcher = match (is_pattern(pattern), insensitive) {
            (true, true) => "fnmatch-ci",
            (false, true) => "streq-ci",
            (true, false) => "fnmatch",
            (false, false) => "streq",
        };

        if let Some(existing_id) = self.matches.get(&(pattern.to_string(), insensitive)) {
            return *existing_id;
        }

        self.vars.push(format!(
            "(%lf3:match:{} (lambda (%lf3:str:{}) ({matcher}? \"{pattern}\" %lf3:str:{})))",
            self.var_index + 1,
            self.var_index,
            self.var_index
        ));

        self.var_index += 2;
        self.matches
            .insert((pattern.to_string(), insensitive), self.var_index - 1);
        return self.var_index - 1;
    }
}

impl SchemeManager for LocalSchemeManager {
    fn get_printer(&mut self, terminator: &str) -> String {
        let port = self.init_default_port();
        let printer_index = self.register_printer(port, terminator.to_string());

        format!("%lf3:print:{printer_index}")
    }

    fn get_file_printer(&mut self, filename: &str, terminator: &str) -> String {
        let port = self.init_file_port(filename.to_string());
        let printer_index = self.register_printer(port, terminator.to_string());

        format!("%lf3:print:{printer_index}")
    }

    /// This method will record a string matching operation and create a function for it in the
    /// initialization of the program. If the given string is detected to be a patter, the fnmatch
    /// function will be used to compare the strings, else the streq function will be used.
    fn get_matcher(&mut self, pattern: &str, insensitive: bool) -> String {
        format!(
            "%lf3:match:{}",
            self.register_str_match(pattern, insensitive)
        )
    }

    fn definitions(&self) -> String {
        self.vars.join(" ")
    }

    fn initialization(&self) -> String {
        if self.init.is_empty() {
            String::from("#t")
        } else {
            self.init.join(" ")
        }
    }

    fn terminate(&self) -> String {
        if self.fini.is_empty() {
            String::from("#t")
        } else {
            self.fini.join(" ")
        }
    }
}
