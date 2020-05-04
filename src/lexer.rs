extern crate plex;
use plex::lexer;

#[derive(Debug)]
pub enum Token {
    Int,
    If,
    Else,
    While,
    Void,
    Return,

    Plus,
    Minus,
    Multiply,
    Divide,

    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equals,
    NotEquals,

    Assign,
    Semicolon,
    Comma,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,

    Ident(String), 
    Integer(i64),

    Whitespace,
    Comment,
}

lexer! {
    // the prototype for the lexer function
    fn next_token(text: 'a) -> Token;

    // non-code tokens

    r#"[ \t\r\n]+"# => Token::Whitespace,
    // C-style comments (can't contain themselves)
    r#"/[*](~(.*[*]/.*))[*]/"# => Token::Comment,
    // C++-style comments
    r#"//[^\n]*"# => Token::Comment,

    // keywords

    "int" => Token::Int,
    "if" => Token::If,
    "else" => Token::Else,
    "while" => Token::While,
    "void" => Token::Void,
    "return" => Token::Return,

    // arithmetic operators

    "[+]" => Token::Plus,
    "-" => Token::Minus,
    "[*]" => Token::Multiply,
    r#"/"# => Token::Divide,

    // comparison operators

    "<" => Token::LessThan,
    ">" => Token::GreaterThan,
    "<=" => Token::LessThanEqual,
    ">=" => Token::GreaterThanEqual,
    "==" => Token::Equals,
    "!=" => Token::NotEquals,

    // assignment operator
    
    "=" => Token::Assign,

    // structural chars

    ";" => Token::Semicolon,
    "," => Token::Comma,
    r#"\("# => Token::LeftParen,
    r#"\)"# => Token::RightParen,
    r#"\["# => Token::LeftSquare,
    r#"\]"# => Token::RightSquare,
    r#"\{"# => Token::LeftCurly,
    r#"\}"# => Token::RightCurly,

    // identifier
    r#"[a-zA-Z_][a-zA-Z0-9_]*"# => Token::Ident(text.to_owned()),

    // number
    r#"[0-9]+"# => {
        if let Ok(i) = text.parse() {
            Token::Integer(i)
        } else {
            panic!("integer '{}' out of range", text)
        }
    }

    // panic if not already matched
    "." => panic!("unexpected char '{}'", text),
}

pub struct Lexer<'a> {
    original: &'a str,
    remaining: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        Lexer {
            original: s,
            remaining: s,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);
    fn next(&mut self) -> Option<(Token, Span)> {
        loop {
            let (tok, span) = if let Some((tok, new_remaining)) = next_token(self.remaining) {
                let lo = self.original.len() - self.remaining.len();
                let hi = self.original.len() - new_remaining.len();
                self.remaining = new_remaining;
                (tok, Span { hi, lo })
            } else {
                return None;
            };
            match tok {
                Token::Whitespace | Token::Comment => { continue; }
                tok => { return Some((tok, span)); }
            }
        }
    }
}

