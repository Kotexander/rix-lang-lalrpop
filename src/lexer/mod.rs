use std::{borrow::Cow, iter::Peekable, num::ParseIntError, ops::Range, str::CharIndices};

pub enum Error {
    UnterminatedString {
        span: Range<usize>,
    },
    UnknownCharacter {
        span: Range<usize>,
        character: char,
    },
    UnkownEscapeSequence {
        span: Range<usize>,
        esc_span: Range<usize>,
    },
    ParseInt {
        span: Range<usize>,
        err: ParseIntError,
    },
}

pub fn parse_string_literal<'a>(
    str: &'a str,
    start: usize,
    span: Range<usize>,
) -> Result<Cow<'a, str>, Error> {
    if !str.contains('\\') {
        return Ok(Cow::Borrowed(str));
    }
    let mut result = String::with_capacity(str.len());
    let mut iter = str.char_indices();
    while let Some((i, ch)) = iter.next() {
        if ch == '\\' {
            let (j, esc) = iter
                .next()
                .expect("EOF escape character should have been caught as unterminated string");
            match esc {
                'n' => result.push('\n'),
                '\\' => result.push('\\'),
                '"' => result.push('"'),
                other => {
                    return Err(Error::UnkownEscapeSequence {
                        span,
                        esc_span: start + i..(start + j + other.len_utf8()),
                    });
                }
            }
        } else {
            result.push(ch);
        }
    }
    Ok(Cow::Owned(result))
}
pub fn parse_number(str: &str, span: Range<usize>) -> Result<i32, Error> {
    match str.parse::<i32>() {
        Ok(n) => Ok(n),
        Err(e) => Err(Error::ParseInt { span, err: e }),
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Tok<'input> {
    Number(&'input str),
    String(usize, &'input str),
    NewLine,
    Colon,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Apersand,
    Equal,
    EqualEqual,
    LessThan,
    GreaterThan,
    /// =>
    EqualGreater,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Comma,
    Dot,
    DotDot,
    DotDotDot,
    Ident(&'input str),

    Fn,
    Return,
    Var,
    For,
    And,
    Or,
    In,
    If,
    Else,
    Struct,
    Union,
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
    fn eat_string(&mut self) -> Result<(usize, usize), Option<usize>> {
        let mut escaped = false;
        let mut end = None;
        for (i, ch) in self.chars.by_ref() {
            end = Some(i + ch.len_utf8());

            if ch == '"' && !escaped {
                return Ok((i, i + ch.len_utf8()));
            }
            escaped = ch == '\\' && !escaped;
        }

        Err(end)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok<'input>, usize, Error>;

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
                '<' => return Some(Ok((i, Tok::LessThan, i + ch.len_utf8()))),
                '>' => return Some(Ok((i, Tok::GreaterThan, i + ch.len_utf8()))),
                '(' => return Some(Ok((i, Tok::LParen, i + ch.len_utf8()))),
                ')' => return Some(Ok((i, Tok::RParen, i + ch.len_utf8()))),
                '{' => return Some(Ok((i, Tok::LBrace, i + ch.len_utf8()))),
                '}' => return Some(Ok((i, Tok::RBrace, i + ch.len_utf8()))),
                '[' => return Some(Ok((i, Tok::LBracket, i + ch.len_utf8()))),
                ']' => return Some(Ok((i, Tok::RBracket, i + ch.len_utf8()))),
                ',' => return Some(Ok((i, Tok::Comma, i + ch.len_utf8()))),
                '\n' => return Some(Ok((i, Tok::NewLine, i + ch.len_utf8()))),
                ':' => return Some(Ok((i, Tok::Colon, i + ch.len_utf8()))),
                '.' => {
                    let (tok, j) = if let Some((ni, nch)) = self.chars.peek()
                        && *nch == '.'
                    {
                        let ni = *ni;
                        let nch = *nch;
                        self.chars.next();

                        if let Some((nni, nnch)) = self.chars.peek()
                            && *nnch == '.'
                        {
                            let nni = *nni;
                            let nnch = *nnch;
                            self.chars.next();
                            (Tok::DotDotDot, nni + nnch.len_utf8())
                        } else {
                            (Tok::DotDot, ni + nch.len_utf8())
                        }
                    } else {
                        (Tok::Dot, i + ch.len_utf8())
                    };
                    return Some(Ok((i, tok, j)));
                }
                '=' => {
                    let (tok, j) = if let Some((ni, nch)) = self.chars.peek() {
                        let ni = *ni;
                        let nch = *nch;
                        match nch {
                            '>' => {
                                self.chars.next();
                                (Tok::EqualGreater, ni + nch.len_utf8())
                            }
                            '=' => {
                                self.chars.next();
                                (Tok::EqualEqual, ni + nch.len_utf8())
                            }
                            _ => (Tok::Equal, i + ch.len_utf8()),
                        }
                    } else {
                        (Tok::Equal, i + ch.len_utf8())
                    };
                    return Some(Ok((i, tok, j)));
                }

                '0'..='9' => {
                    let start = i;
                    let mut end = i + ch.len_utf8();
                    self.eat_number(&mut end);
                    return Some(Ok((start, Tok::Number(&self.input[start..end]), end)));
                }
                '"' => {
                    let start = i + ch.len_utf8();
                    match self.eat_string() {
                        Ok((end, j)) => {
                            return Some(Ok((i, Tok::String(start, &self.input[start..end]), j)));
                        }
                        Err(j) => {
                            return Some(Err(Error::UnterminatedString {
                                span: i..j.unwrap_or(i),
                            }));
                        }
                    }
                }

                ch if ch.is_whitespace() => {
                    self.eat_whitespace();
                }
                '#' => {
                    self.eat_comment();
                }
                ch if ch.is_alphabetic() || ch == '_' => {
                    let mut end = i + ch.len_utf8();
                    while let Some((next_i, next_c)) = self.chars.peek() {
                        if next_c.is_alphanumeric() || *next_c == '_' {
                            end = *next_i + next_c.len_utf8();
                            self.chars.next();
                        } else {
                            break;
                        }
                    }
                    let id = &self.input[i..end];
                    match id {
                        "fn" => return Some(Ok((i, Tok::Fn, end))),
                        "return" => return Some(Ok((i, Tok::Return, end))),
                        "var" => return Some(Ok((i, Tok::Var, end))),
                        "for" => return Some(Ok((i, Tok::For, end))),
                        "and" => return Some(Ok((i, Tok::And, end))),
                        "or" => return Some(Ok((i, Tok::Or, end))),
                        "in" => return Some(Ok((i, Tok::In, end))),
                        "if" => return Some(Ok((i, Tok::If, end))),
                        "else" => return Some(Ok((i, Tok::Else, end))),
                        "struct" => return Some(Ok((i, Tok::Struct, end))),
                        "union" => return Some(Ok((i, Tok::Union, end))),
                        _ => return Some(Ok((i, Tok::Ident(id), end))),
                    }
                }
                ch => {
                    return Some(Err(Error::UnknownCharacter {
                        span: i..(i + ch.len_utf8()),
                        character: ch,
                    }));
                }
            }
        }
    }
}
