use crate::diagnostic::*;
use std::fmt;

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub enum TokenKind {
    Identifier { text: String },
    IntLiteral { value: usize },
    CharLiteral { value: char },
    StringLiteral { value: String },
    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    Percent,
    OpenAngle,
    CloseAngle,
    OpenAngleEquals,
    CloseAngleEquals,
    Dot,
    Colon,
    PrintKeyword,
    PopKeyword,
    SwapKeyword,
    OverKeyword,
    RotKeyword,
    DupKeyword,
    TrueKeyword,
    FalseKeyword,
    AndKeyword,
    OrKeyword,
    NotKeyword,
    LenKeyword,
    ConsKeyword,
    RangeKeyword,
    ConcatKeyword,
    AppendKeyword,
    MapKeyword,
    FilterKeyword,
    FoldKeyword,
    ForeachKeyword,
    CallKeyword,
    ZipKeyword,
    PickKeyword,
    SliceKeyword,
    SplitKeyword,
    OpenSquare,
    CloseSquare,
    OpenParen,
    CloseParen,
    LetKeyword,
    SetKeyword,
    IfKeyword,
    PartialKeyword,
    ReturnKeyword,
    Arrow,
    ArgsKeyword,
    Whitespace,
    EOF,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Loc {
    path: String,
    row: usize,
    col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}:{}:{}", self.path, self.row, self.col);
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: Loc,
}

//TODO: stream the file
pub struct Lexer {
    path: String,
    lines: Vec<String>,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(path: String, lines: Vec<String>) -> Lexer {
        return Lexer {
            path,
            lines,
            line: 0,
            col: 0,
        };
    }
    pub fn next_token(&mut self) -> Result<Token, Diagnostic> {
        if self.line >= self.lines.len() {
            return Ok(Token {
                kind: TokenKind::EOF,
                text: "\0".to_string(),
                loc: Loc {
                    path: self.path.clone(),
                    row: self.line + 1,
                    col: self.col + 1,
                },
            });
        }
        let mut line = self.lines[self.line].as_bytes();

        while line.len() == 0 && self.line < self.lines.len() {
            self.line += 1;
            if self.line >= self.lines.len() {
                return Ok(Token {
                    kind: TokenKind::EOF,
                    text: "\0".to_string(),
                    loc: Loc {
                        path: self.path.clone(),
                        row: self.line + 1,
                        col: self.col + 1,
                    },
                });
            }
            line = self.lines[self.line].as_bytes();
        }

        let c = line[self.col] as char;

        let (token, bytes_read) = match c {
            n if n.is_numeric() => {
                let (text, bytes_read) = self.take_while(line, |c| c.is_numeric())?;

                (
                    Token {
                        kind: TokenKind::IntLiteral {
                            value: text.parse().unwrap(),
                        },
                        text,
                        loc: Loc {
                            path: self.path.clone(),
                            row: self.line + 1,
                            col: self.col + 1,
                        },
                    },
                    bytes_read,
                )
            }
            w if w.is_whitespace() => {
                let (text, bytes_read) = self.take_while(line, |c| c.is_whitespace())?;

                (
                    Token {
                        kind: TokenKind::Whitespace,
                        text,
                        loc: Loc {
                            path: self.path.clone(),
                            row: self.line + 1,
                            col: self.col + 1,
                        },
                    },
                    bytes_read,
                )
            }
            q if q == '\"' => {
                self.col += 1;
                let (text, bytes_read) = self.take_while(line, |c| c != '\"')?;

                (
                    Token {
                        kind: TokenKind::StringLiteral {
                            value: text.clone(),
                        },
                        text,
                        loc: Loc {
                            path: self.path.clone(),
                            row: self.line + 1,
                            col: self.col,
                        },
                    },
                    bytes_read + 1,
                )
            }
            x if x.is_alphabetic() => {
                let (text, bytes_read) = self.take_while(line, |c| c.is_alphabetic())?;

                let kind = match text.as_str() {
                    "print" => TokenKind::PrintKeyword,
                    "pop" => TokenKind::PopKeyword,
                    "swap" => TokenKind::SwapKeyword,
                    "rot" => TokenKind::RotKeyword,
                    "dup" => TokenKind::DupKeyword,
                    "over" => TokenKind::OverKeyword,
                    "true" => TokenKind::TrueKeyword,
                    "false" => TokenKind::FalseKeyword,
                    "and" => TokenKind::AndKeyword,
                    "or" => TokenKind::OrKeyword,
                    "not" => TokenKind::NotKeyword,
                    "len" => TokenKind::LenKeyword,
                    "cons" => TokenKind::ConsKeyword,
                    "range" => TokenKind::RangeKeyword,
                    "concat" => TokenKind::ConcatKeyword,
                    "append" => TokenKind::AppendKeyword,
                    "map" => TokenKind::MapKeyword,
                    "filter" => TokenKind::FilterKeyword,
                    "fold" => TokenKind::FoldKeyword,
                    "foreach" => TokenKind::ForeachKeyword,
                    "call" => TokenKind::CallKeyword,
                    "zip" => TokenKind::ZipKeyword,
                    "pick" => TokenKind::PickKeyword,
                    "slice" => TokenKind::SliceKeyword,
                    "split" => TokenKind::SplitKeyword,
                    "let" => TokenKind::LetKeyword,
                    "set" => TokenKind::SetKeyword,
                    "partial" => TokenKind::PartialKeyword,
                    "if" => TokenKind::IfKeyword,
                    "return" => TokenKind::ReturnKeyword,
                    "args" => TokenKind::ArgsKeyword,
                    _ => TokenKind::Identifier { text: text.clone() },
                };

                (
                    Token {
                        kind,
                        text,
                        loc: Loc {
                            path: self.path.clone(),
                            row: self.line + 1,
                            col: self.col + 1,
                        },
                    },
                    bytes_read,
                )
            }
            '+' => self.lex_token(TokenKind::Plus, "+".to_owned()),
            '-' => {
                if self.peek(line) == '>' {
                    self.lex_token(TokenKind::Arrow, "->".to_string())
                } else {
                    self.lex_token(TokenKind::Minus, "-".to_string())
                }
            }
            '*' => self.lex_token(TokenKind::Star, "*".to_owned()),
            '/' => self.lex_token(TokenKind::Slash, "/".to_owned()),
            '%' => self.lex_token(TokenKind::Percent, "%".to_owned()),
            '.' => self.lex_token(TokenKind::Dot, ".".to_owned()),
            ':' => self.lex_token(TokenKind::Colon, ":".to_owned()),
            '=' => self.lex_token(TokenKind::Equals, "=".to_owned()),
            '[' => self.lex_token(TokenKind::OpenSquare, "[".to_owned()),
            ']' => self.lex_token(TokenKind::CloseSquare, "]".to_owned()),
            '(' => self.lex_token(TokenKind::OpenParen, "(".to_owned()),
            ')' => self.lex_token(TokenKind::CloseParen, ")".to_owned()),
            '<' => {
                if self.peek(line) == '=' {
                    self.lex_token(TokenKind::OpenAngleEquals, "<=".to_string())
                } else {
                    self.lex_token(TokenKind::OpenAngle, "<".to_string())
                }
            }
            '>' => {
                if self.peek(line) == '=' {
                    self.lex_token(TokenKind::CloseAngleEquals, ">=".to_string())
                } else {
                    self.lex_token(TokenKind::CloseAngle, ">".to_string())
                }
            }
            '\'' => {
                self.col += 1;
                let char_literal = line[self.col] as char;
                (
                    Token {
                        kind: TokenKind::CharLiteral {
                            value: char_literal,
                        },
                        text: char_literal.to_string(),
                        loc: Loc {
                            path: self.path.clone(),
                            row: self.line + 1,
                            col: self.col + 1,
                        },
                    },
                    1,
                )
            }
            _ => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    loc: Loc {
                        path: self.path.clone(),
                        row: self.line + 1,
                        col: self.col + 1,
                    },
                    message: format!("Unexpected token {}", c),
                    hint: None,
                })
            }
        };
        self.col += bytes_read;

        if self.col >= line.len() {
            self.col = 0;
            self.line += 1;
        }

        Ok(token)
    }

    fn peek(&self, line: &[u8]) -> char {
        if self.col + 1 >= line.len() {
            return '\0';
        } else {
            return line[self.col + 1] as char;
        }
    }

    fn lex_token(&self, kind: TokenKind, text: String) -> (Token, usize) {
        let len = text.len();
        (
            Token {
                kind: kind,
                text: text,
                loc: Loc {
                    path: self.path.clone(),
                    row: self.line + 1,
                    col: self.col + 1,
                },
            },
            len,
        )
    }

    fn take_while(
        &self,
        line: &[u8],
        predicate: fn(char) -> bool,
    ) -> Result<(String, usize), Diagnostic> {
        let mut c = line[self.col] as char;

        let mut acc: Vec<char> = vec![];
        let mut len = 0;
        while predicate(c) {
            acc.push(c);
            len += 1;
            if self.col + len >= line.len() {
                break;
            }
            c = line[self.col + len] as char;
        }
        let text = String::from_iter(acc);
        return Ok((text, len));
    }
}
