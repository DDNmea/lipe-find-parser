use std::collections::HashMap;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct OpenPort {
    port: usize,
    mutex: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemeManager {
    init: Vec<String>,
    fini: Vec<String>,

    var_index: usize,
    vars: Vec<String>,

    default_port: Option<OpenPort>,
    files: HashMap<String, OpenPort>,
    printers: HashMap<(OpenPort, String), usize>,
    matches: HashMap<String, usize>,
}

impl Default for SchemeManager {
    fn default() -> Self {
        SchemeManager {
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

impl SchemeManager {
    pub fn get_printer<S>(&mut self, terminator: S) -> String
    where
        S: AsRef<str>,
    {
        let port = self.init_default_port();
        let printer_index = self.register_printer(port, terminator.as_ref().to_string());

        format!("%lf3:print:{printer_index}")
    }

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

    pub fn get_file_printer<S1, S2>(&mut self, filename: S1, terminator: S2) -> String
    where
        S1: AsRef<str>,
        S2: AsRef<str>,
    {
        let port = self.init_file_port(filename.as_ref().to_string());
        let printer_index = self.register_printer(port, terminator.as_ref().to_string());

        format!("%lf3:print:{printer_index}")
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

    /// This method will record a string matching operation and create a function for it in the
    /// initialization of the program. If the given string is detected to be a patter, the fnmatch
    /// function will be used to compare the strings, else the streq function will be used.
    pub fn register_strcmp<S: AsRef<str>>(&mut self, cmp: S) -> usize {
        let matcher = if is_pattern(cmp.as_ref()) {
            "fnmatch"
        } else {
            "streq"
        };

        self.register_str_match(matcher, cmp.as_ref())
    }

    /// This method operates the same way as the above for case insensitive matches, using either
    /// fnmatch-ci or streq-ci
    pub fn register_ci_strcmp<S: AsRef<str>>(&mut self, cmp: S) -> usize {
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

    pub fn vars(&self) -> String {
        self.vars.join(" ")
    }

    pub fn init(&self) -> String {
        if self.init.is_empty() {
            String::from("#t")
        } else {
            self.init.join(" ")
        }
    }

    pub fn fini(&self) -> String {
        if self.fini.is_empty() {
            String::from("#t")
        } else {
            self.fini.join(" ")
        }
    }
}
