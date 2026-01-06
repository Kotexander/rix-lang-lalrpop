use std::{borrow::Cow, iter::Peekable, num::ParseIntError, ops::Range, str::CharIndices};

#[derive(Debug, Clone)]
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
pub fn parse_number(str: &str, span: Range<usize>) -> Result<i64, Error> {
    match str.parse() {
        Ok(n) => Ok(n),
        Err(e) => Err(Error::ParseInt { span, err: e }),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tok<'input> {
    Number(&'input str),
    String(usize, &'input str),
    Ident(&'input str),

    Plus,
    Minus,
    Slash,
    Asterisk,
    Percent,
    Ampersand,
    Caret,
    Pipe,
    And,
    Or,

    NewLine,
    Colon,
    Semicolon,
    Dot,
    DotDot,
    DotDotDot,
    Comma,
    Bang,

    Equal,
    EqualEqual,

    /// <
    LessThan,
    /// >
    GreaterThan,
    /// <=
    LessEqual,
    /// <<
    LessLess,
    /// >=
    GreaterEqual,
    /// >>
    GreaterGreater,
    /// !=
    NotEqual,
    /// =>
    EqualGreater,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    While,
    For,
    In,
    Break,
    Continue,

    If,
    Else,

    Fn,
    Return,
    Var,

    Struct,
    Union,
}
impl std::fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::Number(n) => write!(f, "`{}`", n),
            Tok::String(_, s) => write!(f, "`{:?}`", s),
            Tok::Ident(i) => write!(f, "`{}`", i),
            Tok::NewLine => write!(f, "`\\n`"),
            Tok::Colon => write!(f, "`:`"),
            Tok::Semicolon => write!(f, "`;`"),
            Tok::Plus => write!(f, "`+`"),
            Tok::Minus => write!(f, "`-`"),
            Tok::Slash => write!(f, "`/`"),
            Tok::Percent => write!(f, "`%`"),
            Tok::Ampersand => write!(f, "`&`"),
            Tok::Bang => write!(f, "`!`"),
            Tok::Caret => write!(f, "`^`"),
            Tok::Pipe => write!(f, "`|`"),
            Tok::Asterisk => write!(f, "`*`"),
            Tok::Equal => write!(f, "`=`"),
            Tok::EqualEqual => write!(f, "`==`"),
            Tok::LessThan => write!(f, "`<`"),
            Tok::GreaterThan => write!(f, "`>`"),
            Tok::LessEqual => write!(f, "`<=`"),
            Tok::LessLess => write!(f, "`<<`"),
            Tok::GreaterEqual => write!(f, "`>=`"),
            Tok::GreaterGreater => write!(f, "`>>`"),
            Tok::NotEqual => write!(f, "`!=`"),
            Tok::EqualGreater => write!(f, "`=>`"),
            Tok::LParen => write!(f, "`(`"),
            Tok::RParen => write!(f, "`)`"),
            Tok::LBrace => write!(f, "`{{`"),
            Tok::RBrace => write!(f, "`}}`"),
            Tok::LBracket => write!(f, "`[`"),
            Tok::RBracket => write!(f, "`]`"),
            Tok::Comma => write!(f, "`,`"),
            Tok::Dot => write!(f, "`.`"),
            Tok::DotDot => write!(f, "`..`"),
            Tok::DotDotDot => write!(f, "`...`"),
            Tok::While => write!(f, "`while`"),
            Tok::Break => write!(f, "`break`"),
            Tok::Continue => write!(f, "`continue`"),
            Tok::Fn => write!(f, "`fn`"),
            Tok::Return => write!(f, "`return`"),
            Tok::Var => write!(f, "`var`"),
            Tok::For => write!(f, "`for`"),
            Tok::And => write!(f, "`and`"),
            Tok::Or => write!(f, "`or`"),
            Tok::In => write!(f, "`in`"),
            Tok::If => write!(f, "`if`"),
            Tok::Else => write!(f, "`else`"),
            Tok::Struct => write!(f, "`struct`"),
            Tok::Union => write!(f, "`union`"),
        }
    }
}

#[derive(Debug)]
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
                '&' => return Some(Ok((i, Tok::Ampersand, i + ch.len_utf8()))),
                '%' => return Some(Ok((i, Tok::Percent, i + ch.len_utf8()))),
                '^' => return Some(Ok((i, Tok::Caret, i + ch.len_utf8()))),
                '|' => return Some(Ok((i, Tok::Pipe, i + ch.len_utf8()))),
                '!' => {
                    let (tok, j) = if let Some((ni, nch)) = self.chars.peek() {
                        let ni = *ni;
                        let nch = *nch;
                        match nch {
                            '=' => {
                                self.chars.next();
                                (Tok::NotEqual, ni + nch.len_utf8())
                            }
                            _ => (Tok::Bang, i + ch.len_utf8()),
                        }
                    } else {
                        (Tok::Bang, i + ch.len_utf8())
                    };
                    return Some(Ok((i, tok, j)));
                }
                '<' => {
                    let (tok, j) = if let Some((ni, nch)) = self.chars.peek() {
                        let ni = *ni;
                        let nch = *nch;
                        match nch {
                            '=' => {
                                self.chars.next();
                                (Tok::LessEqual, ni + nch.len_utf8())
                            }
                            '<' => {
                                self.chars.next();
                                (Tok::LessLess, ni + nch.len_utf8())
                            }
                            _ => (Tok::LessThan, i + ch.len_utf8()),
                        }
                    } else {
                        (Tok::LessThan, i + ch.len_utf8())
                    };
                    return Some(Ok((i, tok, j)));
                }
                '>' => {
                    let (tok, j) = if let Some((ni, nch)) = self.chars.peek() {
                        let ni = *ni;
                        let nch = *nch;
                        match nch {
                            '=' => {
                                self.chars.next();
                                (Tok::GreaterEqual, ni + nch.len_utf8())
                            }
                            '>' => {
                                self.chars.next();
                                (Tok::GreaterGreater, ni + nch.len_utf8())
                            }
                            _ => (Tok::GreaterThan, i + ch.len_utf8()),
                        }
                    } else {
                        (Tok::GreaterThan, i + ch.len_utf8())
                    };
                    return Some(Ok((i, tok, j)));
                }
                '(' => return Some(Ok((i, Tok::LParen, i + ch.len_utf8()))),
                ')' => return Some(Ok((i, Tok::RParen, i + ch.len_utf8()))),
                '{' => return Some(Ok((i, Tok::LBrace, i + ch.len_utf8()))),
                '}' => return Some(Ok((i, Tok::RBrace, i + ch.len_utf8()))),
                '[' => return Some(Ok((i, Tok::LBracket, i + ch.len_utf8()))),
                ']' => return Some(Ok((i, Tok::RBracket, i + ch.len_utf8()))),
                ',' => return Some(Ok((i, Tok::Comma, i + ch.len_utf8()))),
                '\n' => return Some(Ok((i, Tok::NewLine, i + ch.len_utf8()))),
                ':' => return Some(Ok((i, Tok::Colon, i + ch.len_utf8()))),
                ';' => return Some(Ok((i, Tok::Semicolon, i + ch.len_utf8()))),
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

                '#' => {
                    self.eat_comment();
                }

                ch if ch.is_whitespace() => {
                    self.eat_whitespace();
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
                        "while" => return Some(Ok((i, Tok::While, end))),
                        "break" => return Some(Ok((i, Tok::Break, end))),
                        "continue" => return Some(Ok((i, Tok::Continue, end))),
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
