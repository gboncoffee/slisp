use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::parser;

#[derive(Debug, Clone)]
pub enum Expression {
    Application(Vec<Rc<Expression>>),
    Quote(Rc<Expression>),
    Name(String),
    Float(f64),
    Int(i64),
    Nil,
    T,
}

#[derive(Debug)]
pub enum EvaluationError {
    CompilationError(parser::ParserError),
    Undefined(String),
    NotAName,
    NotAFunction(String),
    WrongArity(String, usize, usize),
    NotANumber,
    NotAQuote,
    NullApplication,
    NotAnApplication,
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluationError::CompilationError(err) => write!(f, "{}", err),
            EvaluationError::NotAName => write!(f, "Not a name"),
            EvaluationError::Undefined(name) => write!(f, "Undefined: {}", name),
            EvaluationError::NotAFunction(name) => write!(f, "Not a function: {}", name),
            EvaluationError::WrongArity(function, expected, got) => {
                write!(f, "Wrong arity for {function}/{expected}: {got}")
            }
            EvaluationError::NotANumber => write!(f, "Not a number"),
            EvaluationError::NotAQuote => write!(f, "Not a quote"),
            EvaluationError::NullApplication => write!(f, "Null quote"),
            EvaluationError::NotAnApplication => write!(f, "Not an application"),
        }
    }
}

pub type Scope = HashMap<String, Rc<Expression>>;

#[derive(Debug)]
pub struct Lisp {
    toplevel: Scope,
}

impl Lisp {
    pub fn new() -> Self {
        Self {
            toplevel: HashMap::new(),
        }
    }

    pub fn eval(
        self: &mut Self,
        content: &str,
        scope: Option<&Scope>,
    ) -> Result<Rc<Expression>, EvaluationError> {
        match parser::parse(content) {
            Ok(Expression::Application(list)) => {
                let results = self.eval_expression_list(&list, scope)?;
                if results.is_empty() {
                    Ok(Rc::new(Expression::Nil))
                } else {
                    Ok(results[results.len() - 1].clone())
                }
            }
            Ok(expression) => Ok(Rc::new(expression)),
            Err(err) => Err(EvaluationError::CompilationError(err)),
        }
    }

    fn eval_expression_list(
        self: &mut Self,
        expressions: &[Rc<Expression>],
        scope: Option<&Scope>,
    ) -> Result<Vec<Rc<Expression>>, EvaluationError> {
        let mut results = Vec::new();
        for expression in expressions {
            results.push(self.eval_expression(expression.clone(), scope)?)
        }
        Ok(results)
    }

    fn eval_expression(
        self: &mut Self,
        expression: Rc<Expression>,
        scope: Option<&Scope>,
    ) -> Result<Rc<Expression>, EvaluationError> {
        match &*expression {
            Expression::Application(list) => self.apply(&list, scope),
            Expression::Name(name) => self.query(name, scope),
            _ => Ok(expression.clone()),
        }
    }

    fn apply(
        self: &mut Self,
        elem: &[Rc<Expression>],
        scope: Option<&Scope>,
    ) -> Result<Rc<Expression>, EvaluationError> {
        if elem.is_empty() {
            return Ok(Rc::new(Expression::Nil));
        }

        if let Expression::Name(name) = &*elem[0] {
            let args = self.eval_expression_list(&elem[1..], scope)?;
            self.apply_name(&name, &args, scope)
        } else {
            Err(EvaluationError::NotAName)
        }
    }

    fn apply_name(
        self: &mut Self,
        name: &str,
        args: &[Rc<Expression>],
        scope: Option<&Scope>,
    ) -> Result<Rc<Expression>, EvaluationError> {
        match name {
            "println" => Ok(Lisp::println(args)),
            "puts" => Ok(Lisp::puts(args)),
            "def" => self.def(args),
            "unquote" => self.unquote(args, scope),
            "if" => self.iff(args, scope),
            "let" => self.lett(args, scope),
            "cons" => Lisp::cons(args),
            "head" => Lisp::head(args),
            "tail" => Lisp::tail(args),
            ":" => Ok(args[args.len() - 1].clone()),
            "+" => Lisp::add(args),
            "-" => Lisp::sub(args),
            "*" => Lisp::mul(args),
            "=" => Lisp::eq(args),
            _ => self.run_function(name, args),
        }
    }

    pub fn println(args: &[Rc<Expression>]) -> Rc<Expression> {
        Lisp::print(args);
        println!();
        Rc::new(Expression::Nil)
    }

    pub fn print(args: &[Rc<Expression>]) {
        for arg in &args[..args.len() - 1] {
            Lisp::print_expression(arg);
            print!(" ")
        }
        Lisp::print_expression(&args[args.len() - 1])
    }

    fn print_expression(expression: &Expression) {
        match expression {
            Expression::Application(list) => {
                print!("(");
                if list.len() > 0 {
                    for elem in &list[..list.len() - 1] {
                        Lisp::print_expression(elem);
                        print!(" ");
                    }
                    Lisp::print_expression(&list[list.len() - 1]);
                }

                print!(")");
            }
            Expression::T => print!("t"),
            Expression::Nil => print!("nil"),
            Expression::Name(name) => print!("{name}"),
            Expression::Quote(expression) => Lisp::print_quote(&expression),
            Expression::Float(number) => print!("{}", number),
            Expression::Int(number) => print!("{}", number),
        }
    }

    fn print_quote(expression: &Expression) {
        print!("'");
        Lisp::print_expression(expression)
    }

    fn puts(args: &[Rc<Expression>]) -> Rc<Expression> {
        if args.len() > 0 {
            for arg in &args[..args.len() - 1] {
                Lisp::puts_arg(arg);
                print!(" ");
            }
        }
        Lisp::puts_arg(&args[args.len() - 1]);
        println!("");

        Rc::new(Expression::Nil)
    }

    fn puts_arg(arg: &Expression) {
        if let Expression::Quote(a) = arg {
            if let Some(string) = Lisp::quote_to_string(a) {
                print!("{string}");
            } else {
                Lisp::print_quote(a);
            }
        } else {
            Lisp::print_expression(arg);
        }
    }

    fn quote_to_string(quote: &Expression) -> Option<String> {
        if let Expression::Application(list) = &*quote {
            let mut string = String::new();
            for elem in list {
                if let Expression::Int(value) = &**elem {
                    match u32::try_from(*value) {
                        Ok(value) => {
                            if let Some(value) = char::from_u32(value) {
                                string.push(value);
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    };
                } else {
                    return None;
                }
            }
            Some(string)
        } else {
            None
        }
    }

    fn run_function(
        self: &mut Self,
        name: &str,
        args: &[Rc<Expression>],
    ) -> Result<Rc<Expression>, EvaluationError> {
        let err = Err(EvaluationError::NotAFunction(String::from(name)));

        let def = self.query(name, None)?;
        let def = if let Expression::Quote(def) = &*def {
            if let Expression::Application(def) = &**def {
                def
            } else {
                return err;
            }
        } else {
            return err;
        };

        if def.len() < 2 {
            return err;
        }
        let args_names = match &*def[0] {
            Expression::Application(list) => list,
            _ => return err,
        };

        if args_names.len() != args.len() {
            return Err(EvaluationError::WrongArity(
                String::from(name),
                args_names.len(),
                args.len(),
            ));
        }

        let mut scope = Scope::new();
        for (i, arg) in args_names.iter().enumerate() {
            if let Expression::Name(arg) = &**arg {
                scope.insert(arg.clone(), args[i].clone());
            } else {
                return err;
            }
        }

        self.eval_expression(def[1].clone(), Some(&scope))
    }

    fn query(
        self: &Self,
        name: &str,
        scope: Option<&Scope>,
    ) -> Result<Rc<Expression>, EvaluationError> {
        if let Some(scope) = scope {
            if let Some(value) = scope.get(name) {
                return Ok(value.clone());
            }
        }

        if let Some(value) = self.toplevel.get(name) {
            Ok(value.clone())
        } else {
            Err(EvaluationError::Undefined(String::from(name)))
        }
    }

    fn ensure_arity(
        f: &str,
        expected: usize,
        args: &[Rc<Expression>],
    ) -> Result<(), EvaluationError> {
        if args.len() != expected {
            Err(EvaluationError::WrongArity(
                String::from(f),
                expected,
                args.len(),
            ))
        } else {
            Ok(())
        }
    }

    fn def(self: &mut Self, args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("def", 2, args)?;

        if let Expression::Quote(expression) = &*args[0] {
            if let Expression::Name(name) = &**expression {
                self.toplevel.insert(name.clone(), args[1].clone());
                Ok(args[1].clone())
            } else {
                Err(EvaluationError::NotAName)
            }
        } else {
            Err(EvaluationError::NotAName)
        }
    }

    fn unquote(
        self: &mut Self,
        args: &[Rc<Expression>],
        scope: Option<&Scope>,
    ) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("unquote", 1, args)?;

        if let Expression::Quote(expression) = &*args[0] {
            self.eval_expression(expression.clone(), scope)
        } else {
            Ok(args[0].clone())
        }
    }

    fn truthy(expression: &Rc<Expression>) -> bool {
        match &**expression {
            Expression::Application(_) => true,
            Expression::Quote(_) => true,
            Expression::Name(_) => true,
            Expression::Float(number) => *number != 0.0,
            Expression::Int(number) => *number != 0,
            Expression::Nil => false,
            Expression::T => true,
        }
    }

    fn iff(
        self: &mut Self,
        args: &[Rc<Expression>],
        scope: Option<&Scope>,
    ) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("if", 3, args)?;
        if Lisp::truthy(&args[0]) {
            self.unquote(&args[1..2], scope)
        } else {
            self.unquote(&args[2..3], scope)
        }
    }

    fn lett(
        self: &mut Self,
        args: &[Rc<Expression>],
        scope: Option<&Scope>,
    ) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("let", 3, args)?;

        let mut new_scope = Scope::new();
        if let Some(scope) = scope {
            for (key, value) in scope.iter() {
                new_scope.insert(key.clone(), value.clone());
            }
        }

        let name = if let Expression::Quote(q) = &*args[0] {
            if let Expression::Name(name) = &**q {
                name
            } else {
                return Err(EvaluationError::NotAName);
            }
        } else {
            return Err(EvaluationError::NotAQuote);
        };

        new_scope.insert(name.clone(), args[1].clone());
        self.unquote(&args[2..3], Some(&new_scope))
    }

    fn cons(args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("cons", 2, args)?;

        match &*args[1] {
            Expression::Quote(q) => {
                if let Expression::Application(v) = &**q {
                    let mut new = v.clone();
                    new.insert(0, args[0].clone());
                    Ok(Rc::new(Expression::Quote(Rc::new(
                        Expression::Application(new),
                    ))))
                } else {
                    Err(EvaluationError::NotAnApplication)
                }
            }
            Expression::Nil => Ok(Rc::new(Expression::Quote(Rc::new(
                Expression::Application(Vec::from(&args[0..1])),
            )))),
            _ => Err(EvaluationError::NotAQuote),
        }
    }

    fn head(args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("head", 1, args)?;

        if let Expression::Quote(q) = &*args[0] {
            match &**q {
                Expression::Application(v) if v.len() > 0 => Ok(v[0].clone()),
                Expression::Application(_) => Err(EvaluationError::NullApplication),
                _ => Err(EvaluationError::NotAnApplication),
            }
        } else {
            Err(EvaluationError::NotAQuote)
        }
    }

    fn tail(args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("tail", 1, args)?;

        if let Expression::Quote(q) = &*args[0] {
            if let Expression::Application(v) = &**q {
                Ok(Rc::new(if v.len() == 0 {
                    Expression::Nil
                } else {
                    Expression::Quote(Rc::new(Expression::Application(Vec::from(&v[1..]))))
                }))
            } else {
                Err(EvaluationError::NotAnApplication)
            }
        } else {
            Err(EvaluationError::NotAQuote)
        }
    }

    fn add(args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("+", 2, args)?;

        match (&*args[0], &*args[1]) {
            (Expression::Int(a), Expression::Int(b)) => Ok(Rc::new(Expression::Int(a + b))),
            (Expression::Float(a), Expression::Float(b)) => Ok(Rc::new(Expression::Float(a + b))),
            _ => Err(EvaluationError::NotANumber),
        }
    }

    fn sub(args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("-", 2, args)?;

        match (&*args[0], &*args[1]) {
            (Expression::Int(a), Expression::Int(b)) => Ok(Rc::new(Expression::Int(a - b))),
            (Expression::Float(a), Expression::Float(b)) => Ok(Rc::new(Expression::Float(a - b))),
            _ => Err(EvaluationError::NotANumber),
        }
    }

    fn mul(args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("*", 2, args)?;

        match (&*args[0], &*args[1]) {
            (Expression::Int(a), Expression::Int(b)) => Ok(Rc::new(Expression::Int(a * b))),
            (Expression::Float(a), Expression::Float(b)) => Ok(Rc::new(Expression::Float(a * b))),
            _ => Err(EvaluationError::NotANumber),
        }
    }

    fn eq(args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("=", 2, args)?;
        Ok(Rc::new(Lisp::compare(&*args[0], &*args[1])))
    }

    fn compare(a: &Expression, b: &Expression) -> Expression {
        match (a, b) {
            (Expression::Application(va), Expression::Application(vb)) => {
                if va.len() != vb.len() {
                    Expression::Nil
                } else {
                    let mut result = Expression::T;
                    for (a, b) in va.iter().zip(vb.iter()) {
                        if let Expression::Nil = Lisp::compare(a, b) {
                            result = Expression::Nil;
                            break;
                        }
                    }
                    result
                }
            }
            (Expression::Int(a), Expression::Int(b)) => Lisp::bool_to_expression(*a == *b),
            (Expression::Float(a), Expression::Float(b)) => Lisp::bool_to_expression(*a == *b),
            (Expression::Float(a), Expression::Int(b))
            | (Expression::Int(b), Expression::Float(a)) => {
                Lisp::bool_to_expression(*a == (*b as f64))
            }
            (Expression::T, Expression::T) | (Expression::Nil, Expression::Nil) => Expression::T,
            (Expression::Quote(a), Expression::Quote(b)) => Lisp::compare(a, b),
            _ => Expression::Nil,
        }
    }

    fn bool_to_expression(v: bool) -> Expression {
        if v { Expression::T } else { Expression::Nil }
    }
}
