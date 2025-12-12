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
    Uncomparable,
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
            EvaluationError::Uncomparable => write!(f, "Uncomparable"),
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
            "println" => self.println(args),
            "def" => self.def(args),
            "unquote" => self.unquote(args, scope),
            "if" => self.iff(args, scope),
            ":" => Ok(args[args.len() - 1].clone()),
            "+" => self.add(args),
            "-" => self.sub(args),
            "*" => self.mul(args),
            "=" => self.eq(args),
            _ => self.run_function(name, args),
        }
    }

    pub fn println(
        self: &Self,
        args: &[Rc<Expression>],
    ) -> Result<Rc<Expression>, EvaluationError> {
        self.print(args)?;
        println!();
        Ok(Rc::new(Expression::Nil))
    }

    pub fn print(self: &Self, args: &[Rc<Expression>]) -> Result<(), EvaluationError> {
        for arg in &args[..args.len() - 1] {
            self.print_expression(arg.clone())?;
            print!(" ")
        }
        self.print_expression(args[args.len() - 1].clone())
    }

    fn print_expression(self: &Self, expression: Rc<Expression>) -> Result<(), EvaluationError> {
        match &*expression {
            Expression::Application(list) => {
                print!("(");
                for elem in &list[..list.len() - 1] {
                    self.print_expression(elem.clone())?;
                    print!(" ");
                }
                self.print_expression(list[list.len() - 1].clone())?;
                print!(")");
            }
            Expression::T => print!("t"),
            Expression::Nil => print!("nil"),
            Expression::Name(name) => print!("{name}"),
            Expression::Quote(expression) => self.print_quote(expression.clone())?,
            Expression::Float(number) => print!("{}", number),
            Expression::Int(number) => print!("{}", number),
        }
        Ok(())
    }

    fn print_quote(self: &Self, expression: Rc<Expression>) -> Result<(), EvaluationError> {
        if let Some(string) = Lisp::quote_to_string(expression.clone()) {
            print!("{}", string);
            Ok(())
        } else {
            print!("'");
            self.print_expression(expression)
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

        let mut scope = HashMap::new();
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

    fn quote_to_string(quote: Rc<Expression>) -> Option<String> {
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

    fn add(self: &mut Self, args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("+", 2, args)?;

        match (&*args[0], &*args[1]) {
            (Expression::Int(a), Expression::Int(b)) => Ok(Rc::new(Expression::Int(a + b))),
            (Expression::Float(a), Expression::Float(b)) => Ok(Rc::new(Expression::Float(a + b))),
            _ => Err(EvaluationError::NotANumber),
        }
    }

    fn sub(self: &mut Self, args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("-", 2, args)?;

        match (&*args[0], &*args[1]) {
            (Expression::Int(a), Expression::Int(b)) => Ok(Rc::new(Expression::Int(a - b))),
            (Expression::Float(a), Expression::Float(b)) => Ok(Rc::new(Expression::Float(a - b))),
            _ => Err(EvaluationError::NotANumber),
        }
    }

    fn mul(self: &mut Self, args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("*", 2, args)?;

        match (&*args[0], &*args[1]) {
            (Expression::Int(a), Expression::Int(b)) => Ok(Rc::new(Expression::Int(a * b))),
            (Expression::Float(a), Expression::Float(b)) => Ok(Rc::new(Expression::Float(a * b))),
            _ => Err(EvaluationError::NotANumber),
        }
    }

    fn eq(self: &mut Self, args: &[Rc<Expression>]) -> Result<Rc<Expression>, EvaluationError> {
        Lisp::ensure_arity("=", 2, args)?;

        match (&*args[0], &*args[1]) {
            (Expression::Int(a), Expression::Int(b)) => {
                Ok(Rc::new(Lisp::bool_to_expression(*a == *b)))
            }
            (Expression::Float(a), Expression::Float(b)) => {
                Ok(Rc::new(Lisp::bool_to_expression(*a == *b)))
            }
            (Expression::Float(a), Expression::Int(b))
            | (Expression::Int(b), Expression::Float(a)) => {
                Ok(Rc::new(Lisp::bool_to_expression(*a == (*b as f64))))
            }
            (Expression::T, Expression::T) => Ok(Rc::new(Expression::T)),
            (Expression::Nil, Expression::Nil) => Ok(Rc::new(Expression::Nil)),
            _ => Err(EvaluationError::Uncomparable),
        }
    }

    fn bool_to_expression(v: bool) -> Expression {
        if v { Expression::T } else { Expression::Nil }
    }
}
