use std::{iter::Peekable, str::CharIndices};

#[derive(Debug, Clone, Copy)]
pub enum Tok<'input> {
    Number(&'input str),
    String(&'input str),
    NewLine,
    Plus,
    Minus,
    Asterisk,
    And,
    Slash,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Id(&'input str),

    Fn,
    Return,
}
pub struct Lexer<'input> {
    chars: Peekable<CharIndices<'input>>,
    input: &'input str,
}
impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            chars: input.char_indices().peekable(),
            input,
        }
    }
    fn eat_whitespace(&mut self) {
        while let Some((_, ch)) = self.chars.peek() {
            if ch.is_whitespace() && *ch != '\n' {
                self.chars.next();
            } else {
                break;
            }
        }
    }
    fn eat_comment(&mut self) {
        while let Some((_, ch)) = self.chars.peek() {
            if *ch == '\n' {
                break;
            }
            self.chars.next();
        }
    }
    fn eat_number(&mut self, end: &mut usize) {
        while let Some((next_i, next_c)) = self.chars.peek() {
            if next_c.is_ascii_digit() {
                *end = *next_i + next_c.len_utf8();
                self.chars.next();
            } else {
                break;
            }
        }
    }
    fn eat_string(&mut self, end: &mut usize) {
        for (next_i, next_c) in self.chars.by_ref() {
            if next_c == '"' {
                break;
            }
            *end = next_i + next_c.len_utf8();
        }
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok<'input>, usize, String>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (i, c) = self.chars.next()?;
            match c {
                '+' => return Some(Ok((i, Tok::Plus, i + 1))),
                '-' => return Some(Ok((i, Tok::Minus, i + 1))),
                '*' => return Some(Ok((i, Tok::Asterisk, i + 1))),
                '&' => return Some(Ok((i, Tok::And, i + 1))),
                '/' => return Some(Ok((i, Tok::Slash, i + 1))),
                '(' => return Some(Ok((i, Tok::LParen, i + 1))),
                ')' => return Some(Ok((i, Tok::RParen, i + 1))),
                '{' => return Some(Ok((i, Tok::LBrace, i + 1))),
                '}' => return Some(Ok((i, Tok::RBrace, i + 1))),
                ',' => return Some(Ok((i, Tok::Comma, i + 1))),
                '\n' => return Some(Ok((i, Tok::NewLine, i + 1))),

                '0'..='9' => {
                    let start = i;
                    let mut end = i + c.len_utf8();
                    self.eat_number(&mut end);
                    return Some(Ok((start, Tok::Number(&self.input[start..end]), end)));
                }
                '"' => {
                    let start = i + c.len_utf8();
                    let mut end = i;
                    self.eat_string(&mut end);
                    return Some(Ok((start, Tok::String(&self.input[start..end]), end)));
                }

                ch if ch.is_whitespace() => {
                    self.eat_whitespace();
                }
                '#' => {
                    self.eat_comment();
                }
                ch if ch.is_alphabetic() || ch == '_' => {
                    let start = i;
                    let mut end = i + c.len_utf8();
                    while let Some((next_i, next_c)) = self.chars.peek() {
                        if next_c.is_alphanumeric() || *next_c == '_' {
                            end = *next_i + next_c.len_utf8();
                            self.chars.next();
                        } else {
                            break;
                        }
                    }
                    let id = &self.input[start..end];
                    match id {
                        "fn" => return Some(Ok((start, Tok::Fn, end))),
                        "return" => return Some(Ok((start, Tok::Return, end))),
                        _ => return Some(Ok((start, Tok::Id(id), end))),
                    }
                }
                _ => return Some(Err(format!("Unexpected character: {}", c))),
            }
        }
    }
}
