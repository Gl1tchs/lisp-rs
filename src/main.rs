use std::collections::{HashMap, VecDeque};
use std::f32::consts::{E, PI};
use std::process::exit;
use std::rc::Rc;
use std::{error::Error, fmt};

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Sym(String),
    Num(f32),
    List(Vec<Expr>),
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match &self {
            Expr::Sym(sym) => sym.to_string(),
            Expr::Num(num) => format!("{}", num),
            Expr::List(list) => list
                .iter()
                .map(|expr| format!("{} ", expr.to_string()))
                .collect(),
        }
    }
}

type Proc = dyn Fn(Vec<Value>, &Env) -> Result<Value, String>;

#[derive(Clone)]
enum Value {
    Num(f32),
    Func(Rc<Proc>),
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match &self {
            Value::Num(num) => format!("{}", num),
            Value::Func(proc) => format!("{:p}", proc),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Clone)]
struct Env {
    outer: Option<Box<Env>>,
    map: HashMap<String, Value>,
}

impl Env {
    fn from(outer: Env) -> Self {
        Self {
            outer: Some(Box::new(outer)),
            map: HashMap::new(),
        }
    }

    fn get(&self, key: &str) -> Option<&Value> {
        if let Some(value) = self.map.get(key) {
            return Some(value);
        }

        match &self.outer {
            Some(outer) => outer.get(key),
            _ => None,
        }
    }

    fn set(&mut self, key: &str, value: Value) {
        self.map.insert(key.to_string(), value);
    }

    fn insert(&mut self, symbol: &str, value: Value) {
        self.map.insert(symbol.into(), value);
    }

    fn insert_func(&mut self, symbol: &str, func: Rc<Proc>) {
        self.map.insert(symbol.into(), Value::Func(func));
    }
}

#[derive(Debug)]
struct SyntaxError(String);

impl Error for SyntaxError {}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error while parsing the expression: {}", self.0)
    }
}

fn tokenize(input: &str) -> VecDeque<String> {
    input
        .replace('(', "( ")
        .replace(')', " ) ")
        .split(' ')
        .filter(|&s| !s.is_empty())
        .map(|s| s.to_string())
        .collect()
}

fn parse(input: &str) -> Result<Expr, SyntaxError> {
    read_from_tokens(&mut tokenize(input))
}

fn read_from_tokens(tokens: &mut VecDeque<String>) -> Result<Expr, SyntaxError> {
    if tokens.is_empty() {
        return Err(SyntaxError("Cannot parse from an empty token list.".into()));
    }

    let token = tokens.pop_front().unwrap(); // Safe to unwrap, as we checked for emptiness
    if &token == "(" {
        let mut list = Vec::new();
        while tokens.front().map(|t| t.as_str()) != Some(")") {
            list.push(read_from_tokens(tokens)?);
        }
        tokens.pop_front(); // Remove ')'
        Ok(Expr::List(list))
    } else if &token == ")" {
        Err(SyntaxError("Unexpected ')'".into()))
    } else {
        Ok(atom(&token))
    }
}

fn atom(token: &str) -> Expr {
    if let Ok(num) = token.parse::<f32>() {
        Expr::Num(num)
    } else {
        Expr::Sym(token.to_string())
    }
}

fn eval(x: Expr, env: &mut Env) -> Option<Value> {
    match x {
        Expr::Sym(sym) => env.get(&sym).cloned(),
        Expr::Num(num) => Some(Value::Num(num)),
        Expr::List(list) => {
            let op = if let Expr::Sym(sym) = &list[0] {
                Some(sym.as_str())
            } else {
                None
            }?;

            match op {
                "if" => {
                    let exp = if eval(list[1].clone(), env).unwrap() == Value::Num(1.0) {
                        list[2].clone()
                    } else {
                        list[3].clone()
                    };
                    return eval(exp, env);
                }
                "define" => {
                    if let Expr::Sym(sym) = &list[1] {
                        let res = eval(list[2].clone(), env)?;
                        env.set(sym, res);
                    }

                    None
                }
                "set!" => {
                    let symbol = if let Expr::Sym(sym) = &list[1] {
                        Some(sym.as_str())
                    } else {
                        None
                    }?;

                    // set the symbol if it can, do not do otherwise
                    let res = eval(list[2].clone(), env)?;
                    env.set(symbol, res);

                    None
                }
                "lambda" => {
                    let params = if let Expr::List(args) = list[1].clone() {
                        Some(args)
                    } else {
                        None
                    }?;

                    let proc: Rc<Proc> = Rc::new(move |args: Vec<Value>, env: &Env| {
                        if args.len() != params.len() {
                            return Err(
                                "Length of parameters are not suitable with the given ones."
                                    .to_string(),
                            );
                        }

                        let mut inner_env = Env::from(env.clone());
                        for (i, param) in params.iter().enumerate() {
                            if let Expr::Sym(sym) = param {
                                let arg = args[i].clone();
                                inner_env.insert(sym, arg);
                            } else {
                                return Err("parameters should be a type of symbol.".to_string());
                            }
                        }

                        // evaluate the procedure with body and arguments
                        if let Some(result) = eval(list[2].clone(), &mut inner_env) {
                            return Ok(result);
                        }

                        Err("Error while executing procedure".into())
                    });

                    Some(Value::Func(proc))
                }
                _ => {
                    // procedure call
                    let proc = eval(Expr::Sym(op.into()), env)?;
                    let vals: Vec<Value> = list
                        .iter()
                        .skip(1)
                        .map(|arg| eval(arg.clone(), env))
                        .filter_map(|arg| {
                            if arg.is_none() {
                                return None;
                            }

                            return Some(arg.unwrap());
                        })
                        .collect();

                    match proc {
                        Value::Func(proc) => Some(proc(vals, env).unwrap()),
                        _ => None,
                    }
                }
            }
        }
    }
}

fn main() -> rustyline::Result<()> {
    let mut reader = DefaultEditor::new()?;
    if let Err(_) = reader.load_history("rispy_history.txt") {
        println!("No previous history.");
    }

    let mut env = Env::default();

    loop {
        let readline = reader.readline("repl> ");

        match readline {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }

                match parse(&line) {
                    Ok(ast) => {
                        if let Some(result) = eval(ast, &mut env) {
                            println!("{}", result.to_string());
                        }
                    }
                    Err(err) => println!("{}", err),
                }
            }
            Err(ReadlineError::Interrupted) => println!("KeyboardInterrupt"),
            _ => break,
        }
    }

    Ok(())
}

impl Default for Env {
    fn default() -> Self {
        let mut env = Self {
            outer: None,
            map: HashMap::new(),
        };

        env.insert_func(
            "begin",
            Rc::new(|args, _| {
                if args.is_empty() {
                    return Err("begin requires at least one argument.".into());
                }
                // Return the last argument's result
                let last_val = &args[args.len() - 1];
                Ok(last_val.clone())
            }),
        );

        env.insert_func(
            "car",
            Rc::new(|args, _| {
                if args.is_empty() {
                    return Err("car expects at least one argument".into());
                }

                Ok(args[0].clone())
            }),
        );

        env.insert_func(
            "+",
            Rc::new(|args, _| {
                let sum = args.iter().try_fold(0.0, |acc, expr| match expr {
                    Value::Num(n) => Ok::<f32, String>(acc + *n as f32),
                    _ => Err("Addition requires numeric arguments.".into()),
                })?;
                Ok(Value::Num(sum))
            }),
        );
        env.insert_func(
            "-",
            Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err("Subtraction function expects two arguments.".into());
                }
                let a = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("Subtraction requires numeric arguments.".into()),
                };
                let b = match &args[1] {
                    Value::Num(n) => *n,
                    _ => return Err("Subtraction requires numeric arguments.".into()),
                };
                Ok(Value::Num(a - b))
            }),
        );
        env.insert_func(
            "*",
            Rc::new(|args, _| {
                let product = args.iter().try_fold(1.0, |acc, expr| match expr {
                    Value::Num(n) => Ok::<f32, String>(acc * n),
                    _ => Err("Multiplication requires numeric arguments.".into()),
                })?;
                Ok(Value::Num(product))
            }),
        );
        env.insert_func(
            "/",
            Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err("Division function expects two arguments.".into());
                }
                let a = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("Division requires numeric arguments.".into()),
                };
                let b = match &args[1] {
                    Value::Num(n) => *n,
                    _ => return Err("Division requires numeric arguments.".into()),
                };
                if b == 0.0 {
                    return Err("Division by zero.".into());
                }
                Ok(Value::Num(a / b))
            }),
        );
        env.insert_func(
            "mod",
            Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err("mod expects two arguments.".into());
                }
                let a = match &args[0] {
                    Value::Num(n) => *n as i32,
                    _ => return Err("mod requires numeric arguments.".into()),
                };
                let b = match &args[1] {
                    Value::Num(n) => *n as i32,
                    _ => return Err("mod requires numeric arguments.".into()),
                };
                Ok(Value::Num((a % b) as f32))
            }),
        );
        env.insert_func(
            "expt",
            Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err("expt expects two arguments.".into());
                }
                let base = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("expt requires numeric arguments.".into()),
                };
                let exp = match &args[1] {
                    Value::Num(n) => *n,
                    _ => return Err("expt requires numeric arguments.".into()),
                };
                Ok(Value::Num(base.powf(exp)))
            }),
        );
        env.insert_func(
            "sqrt",
            Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err("sqrt expects one argument.".into());
                }
                let x = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("sqrt expects a numeric argument.".into()),
                };
                Ok(Value::Num(x.sqrt()))
            }),
        );
        env.insert_func(
            "abs",
            Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err("abs expects one argument.".into());
                }
                let x = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("abs expects a numeric argument.".into()),
                };
                Ok(Value::Num(x.abs()))
            }),
        );

        env.insert_func(
            "sin",
            Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err("sin function expects one argument.".into());
                }
                if let Value::Num(n) = &args[0] {
                    Ok(Value::Num(n.sin()))
                } else {
                    Err("sin expects a float.".into())
                }
            }),
        );

        env.insert_func(
            "cos",
            Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err("cos function expects one argument.".into());
                }
                if let Value::Num(n) = &args[0] {
                    Ok(Value::Num(n.cos()))
                } else {
                    Err("cos expects a float.".into())
                }
            }),
        );

        env.insert("pi", Value::Num(PI));
        env.insert("e", Value::Num(E));

        env.insert_func(
            ">",
            Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err("gt expects two arguments.".into());
                }
                let a = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("gt expects numeric arguments.".into()),
                };
                let b = match &args[1] {
                    Value::Num(n) => *n,
                    _ => return Err("gt expects numeric arguments.".into()),
                };
                Ok(Value::Num(if a > b { 1.0 } else { 0.0 }))
            }),
        );
        env.insert_func(
            "<",
            Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err("lt expects two arguments.".into());
                }
                let a = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("lt expects numeric arguments.".into()),
                };
                let b = match &args[1] {
                    Value::Num(n) => *n,
                    _ => return Err("lt expects numeric arguments.".into()),
                };
                Ok(Value::Num(if a < b { 1.0 } else { 0.0 }))
            }),
        );
        env.insert_func(
            ">=",
            Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err("gte expects two arguments.".into());
                }
                let a = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("gte expects numeric arguments.".into()),
                };
                let b = match &args[1] {
                    Value::Num(n) => *n,
                    _ => return Err("gte expects numeric arguments.".into()),
                };
                Ok(Value::Num(if a >= b { 1.0 } else { 0.0 }))
            }),
        );
        env.insert_func(
            "<=",
            Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err("lte expects two arguments.".into());
                }
                let a = match &args[0] {
                    Value::Num(n) => *n,
                    _ => return Err("lte expects numeric arguments.".into()),
                };
                let b = match &args[1] {
                    Value::Num(n) => *n,
                    _ => return Err("lte expects numeric arguments.".into()),
                };
                Ok(Value::Num(if a <= b { 1.0 } else { 0.0 }))
            }),
        );

        env.insert_func(
            "exit",
            Rc::new(|_, _| {
                exit(0);
            }),
        );

        env
    }
}
