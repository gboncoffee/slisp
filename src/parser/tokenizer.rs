use std::{fmt::Display, str::Chars};

#[derive(Debug)]
pub enum Token {
    OpenParen,
    CloseParen,
    Quote,
    Name(String),
    Float(f64),
    Int(i64),
    String(String),
    Nil,
    T,
}

#[derive(Debug)]
pub enum TokenizerError {
    NotANumber(String),
    UnexpectedChar(char),
    UnexpectedEOF,
}

impl Display for TokenizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenizerError::NotANumber(content) => write!(f, "Not a number: {}", content),
            TokenizerError::UnexpectedChar(content) => write!(f, "Unexpected char: {}", content),
            TokenizerError::UnexpectedEOF => write!(f, "Unexpected EOF"),
        }
    }
}

pub fn tokenize(content: &str) -> Result<Vec<Token>, TokenizerError> {
    let mut list = Vec::new();
    let mut chars = content.chars();
    tokenize_rec(&mut list, chars.next(), &mut chars)?;
    Ok(list)
}

fn tokenize_rec(
    tokens: &mut Vec<Token>,
    char: Option<char>,
    rem: &mut Chars,
) -> Result<(), TokenizerError> {
    if let Some(char) = char {
        tokenize_char(tokens, char, rem)
    } else {
        Ok(())
    }
}

fn tokenize_char(
    tokens: &mut Vec<Token>,
    char: char,
    rem: &mut Chars,
) -> Result<(), TokenizerError> {
    match char {
        _ if char.is_whitespace() => {}
        _ if char.is_numeric() => {
            return tokenize_number(tokens, String::from(char), rem.next(), rem);
        }
        _ if !char_is_reserved(char) => {
            return tokenize_name(tokens, String::from(char), rem.next(), rem);
        }
        '(' => tokens.push(Token::OpenParen),
        ')' => tokens.push(Token::CloseParen),
        '\'' => tokens.push(Token::Quote),
        '"' => tokenize_string(tokens, String::new(), rem.next(), rem)?,
        ';' => skip_comment(tokens, rem.next(), rem)?,
        _ => return Err(TokenizerError::UnexpectedChar(char)),
    }
    tokenize_rec(tokens, rem.next(), rem)
}

fn tokenize_number(
    tokens: &mut Vec<Token>,
    mut number: String,
    char: Option<char>,
    rem: &mut Chars,
) -> Result<(), TokenizerError> {
    match char {
        Some(char) if !char_is_reserved(char) && !char.is_whitespace() => {
            number.push(char);
            tokenize_number(tokens, number, rem.next(), rem)
        }
        Some(char) => {
            push_number(tokens, number)?;
            tokenize_char(tokens, char, rem)
        }
        None => push_number(tokens, number),
    }
}

fn push_number(tokens: &mut Vec<Token>, number: String) -> Result<(), TokenizerError> {
    if let Ok(num) = number.parse::<i64>() {
        tokens.push(Token::Int(num));
        Ok(())
    } else if let Ok(num) = number.parse::<f64>() {
        tokens.push(Token::Float(num));
        Ok(())
    } else {
        Err(TokenizerError::NotANumber(number))
    }
}

fn tokenize_name(
    tokens: &mut Vec<Token>,
    mut name: String,
    char: Option<char>,
    rem: &mut Chars,
) -> Result<(), TokenizerError> {
    match char {
        Some(char) if !char_is_reserved(char) && !char.is_whitespace() => {
            name.push(char);
            tokenize_name(tokens, name, rem.next(), rem)
        }
        Some(char) => {
            push_name(tokens, name);
            tokenize_char(tokens, char, rem)
        }
        None => {
            push_name(tokens, name);
            Ok(())
        }
    }
}

fn push_name(tokens: &mut Vec<Token>, name: String) {
    match &name[..] {
        "nil" => tokens.push(Token::Nil),
        "t" => tokens.push(Token::T),
        _ => tokens.push(Token::Name(name)),
    }
}

fn char_is_reserved(char: char) -> bool {
    match char {
        '(' | ')' | '\'' | '"' | ';' => true,
        _ => false,
    }
}

fn tokenize_string(
    tokens: &mut Vec<Token>,
    mut string: String,
    char: Option<char>,
    rem: &mut Chars,
) -> Result<(), TokenizerError> {
    match char {
        Some('"') => {
            tokens.push(Token::String(string));
            Ok(())
        }
        Some(char) => {
            string.push(char);
            tokenize_string(tokens, string, rem.next(), rem)
        }
        None => Err(TokenizerError::UnexpectedEOF),
    }
}

fn skip_comment(
    tokens: &mut Vec<Token>,
    char: Option<char>,
    rem: &mut Chars,
) -> Result<(), TokenizerError> {
    match char {
        Some('\n') | None => Ok(()),
        Some(_) => skip_comment(tokens, rem.next(), rem),
    }
}
