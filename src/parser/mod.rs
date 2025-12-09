pub mod tokenizer;

use crate::vm::Expression;
use std::{fmt::Display, rc::Rc, slice::Iter};

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEOF,
    UnmatchedCloseParen,
    TokenizerError(tokenizer::TokenizerError),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedEOF => write!(f, "Unexpected EOF"),
            ParserError::UnmatchedCloseParen => write!(f, "Unmatched )"),
            ParserError::TokenizerError(err) => write!(f, "{}", err),
        }
    }
}

pub fn parse(content: &str) -> Result<Expression, ParserError> {
    match tokenizer::tokenize(content) {
        Ok(mut tokens) => parse_tokens(&mut tokens),
        Err(err) => Err(ParserError::TokenizerError(err)),
    }
}

pub fn parse_tokens(tokens: &mut Vec<tokenizer::Token>) -> Result<Expression, ParserError> {
    // Make everything a single application.
    tokens.insert(0, tokenizer::Token::OpenParen);
    tokens.push(tokenizer::Token::CloseParen);

    let mut rem = tokens.iter();
    Ok(parse_rec(rem.next(), &mut rem)?.unwrap())
}

fn parse_rec(
    token: Option<&tokenizer::Token>,
    rem: &mut Iter<'_, tokenizer::Token>,
) -> Result<Option<Expression>, ParserError> {
    if let Some(token) = token {
        match token {
            tokenizer::Token::OpenParen => Ok(Some(parse_application(rem.next(), rem)?)),
            tokenizer::Token::CloseParen => Err(ParserError::UnmatchedCloseParen),
            tokenizer::Token::Quote => Ok(Some(parse_quote(rem.next(), rem)?)),
            tokenizer::Token::Name(name) => Ok(Some(Expression::Name(name.clone()))),
            tokenizer::Token::String(string) => Ok(Some(parse_string(string))),
            tokenizer::Token::Float(number) => Ok(Some(Expression::Float(*number))),
            tokenizer::Token::Int(number) => Ok(Some(Expression::Int(*number))),
            tokenizer::Token::Nil => Ok(Some(Expression::Nil)),
            tokenizer::Token::T => Ok(Some(Expression::T)),
        }
    } else {
        Ok(None)
    }
}

fn parse_application(
    token: Option<&tokenizer::Token>,
    rem: &mut Iter<'_, tokenizer::Token>,
) -> Result<Expression, ParserError> {
    let mut application = Vec::new();
    parse_application_rec(&mut application, token, rem)?;
    Ok(Expression::Application(application))
}

fn parse_application_rec(
    application: &mut Vec<Rc<Expression>>,
    token: Option<&tokenizer::Token>,
    rem: &mut Iter<'_, tokenizer::Token>,
) -> Result<(), ParserError> {
    match token {
        Some(tokenizer::Token::CloseParen) => Ok(()),
        Some(token) => {
            if let Some(expression) = parse_rec(Some(token), rem)? {
                application.push(Rc::new(expression));
                parse_application_rec(application, rem.next(), rem)
            } else {
                Ok(())
            }
        }
        None => Err(ParserError::UnexpectedEOF),
    }
}

fn parse_quote(
    token: Option<&tokenizer::Token>,
    rem: &mut Iter<'_, tokenizer::Token>,
) -> Result<Expression, ParserError> {
    if let Some(expression) = parse_rec(token, rem)? {
        Ok(Expression::Quote(Rc::new(expression)))
    } else {
        Err(ParserError::UnexpectedEOF)
    }
}

fn parse_string(string: &String) -> Expression {
    let mut list = Vec::new();
    for char in string.chars() {
        list.push(Rc::new(Expression::Int(char as i64)));
    }
    Expression::Quote(Rc::new(Expression::Application(list)))
}
