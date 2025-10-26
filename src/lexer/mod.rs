use std::{iter::Peekable, str::CharIndices};

#[derive(Debug, Clone, Copy)]
pub enum Tok<'input> {
    Number(&'input str),
    String(&'input str),
    NewLine,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Apersand,
    Equal,
    EqualEqual,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    DotDot,
    Id(&'input str),

    Fn,
    Return,
    Var,
    For,
    And,
    In,
    If,
    Else,
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
        while let Some((i, ch)) = self.chars.peek() {
            if ch.is_ascii_digit() {
                *end = *i + ch.len_utf8();
                self.chars.next();
            } else {
                break;
            }
        }
    }
    fn eat_string(&mut self, end: &mut usize) {
        for (i, ch) in self.chars.by_ref() {
            if ch == '"' {
                break;
            }
            *end = i + ch.len_utf8();
        }
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok<'input>, usize, String>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (i, ch) = self.chars.next()?;
            match ch {
                '+' => return Some(Ok((i, Tok::Plus, i + ch.len_utf8()))),
                '-' => return Some(Ok((i, Tok::Minus, i + ch.len_utf8()))),
                '*' => return Some(Ok((i, Tok::Asterisk, i + ch.len_utf8()))),
                '/' => return Some(Ok((i, Tok::Slash, i + ch.len_utf8()))),
                '&' => return Some(Ok((i, Tok::Apersand, i + ch.len_utf8()))),
                '%' => return Some(Ok((i, Tok::Percent, i + ch.len_utf8()))),
                '(' => return Some(Ok((i, Tok::LParen, i + ch.len_utf8()))),
                ')' => return Some(Ok((i, Tok::RParen, i + ch.len_utf8()))),
                '{' => return Some(Ok((i, Tok::LBrace, i + ch.len_utf8()))),
                '}' => return Some(Ok((i, Tok::RBrace, i + ch.len_utf8()))),
                ',' => return Some(Ok((i, Tok::Comma, i + ch.len_utf8()))),
                '\n' => return Some(Ok((i, Tok::NewLine, i + ch.len_utf8()))),
                '.' => {
                    if let Some((i, ch)) = self.chars.peek()
                        && *ch == '.'
                    {
                        let span = (*i, Tok::DotDot, i + ch.len_utf8());
                        self.chars.next();
                        return Some(Ok(span));
                    }
                    return Some(Ok((i, Tok::Dot, i + ch.len_utf8())));
                }
                '=' => {
                    if let Some((i, ch)) = self.chars.peek()
                        && *ch == '='
                    {
                        let span = (*i, Tok::EqualEqual, i + ch.len_utf8());
                        self.chars.next();
                        return Some(Ok(span));
                    }
                    return Some(Ok((i, Tok::Equal, i + ch.len_utf8())));
                }

                '0'..='9' => {
                    let start = i;
                    let mut end = i + ch.len_utf8();
                    self.eat_number(&mut end);
                    return Some(Ok((start, Tok::Number(&self.input[start..end]), end)));
                }
                '"' => {
                    let start = i + ch.len_utf8();
                    let mut end = start;
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
                    let mut end = i + ch.len_utf8();
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
                        "var" => return Some(Ok((start, Tok::Var, end))),
                        "for" => return Some(Ok((start, Tok::For, end))),
                        "and" => return Some(Ok((start, Tok::And, end))),
                        "in" => return Some(Ok((start, Tok::In, end))),
                        "if" => return Some(Ok((start, Tok::If, end))),
                        "else" => return Some(Ok((start, Tok::Else, end))),
                        _ => return Some(Ok((start, Tok::Id(id), end))),
                    }
                }
                _ => return Some(Err(format!("Unexpected character: {}", ch))),
            }
        }
    }
}
