use crate::{
    diagnostic::{Diagnostic, Severity},
    lexer::{Loc, Token, TokenKind},
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Operand {
    Bool { value: bool },
    Int { value: usize },
    Char { value: char },
    Array { values: Vec<Operand> },
    Seq { ops: Vec<Operation> },
    String { value: String },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Function {
    pub name: String,
    pub ins: Vec<TypeExpression>,
    pub outs: Vec<TypeExpression>,
    pub body: Operand,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum OperationKind {
    Nop,
    Push { operand: Operand },
    Pop,
    Swap,
    Dup,
    Over,
    Rot,
    FunctionDefinition { function: Function },
    FunctionBaseCaseDefinition { function: Function },
    FunctionCallOrVariable { name: String },
    Binding { bindings: Vec<Token>, body: Operand },
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Print,
    Identity,
    Len,
    Cons,
    Concat,
    Append,
    Map,
    Filter,
    Fold,
    Foreach,
    Do,
    Range,
    Zip,
    Pick,
    Slice,
    Split,
    Partial,
    Cast,
    If,
    Return,
    Args,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypePatternKind {
    Literal { value: usize }, //TODO: What about the other types?
                              //Array
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeExpressionKind {
    Identifier {
        text: String,
    },
    Pattern {
        kind: TypePatternKind,
    },
    Function {
        ins: Vec<TypeExpression>,
        outs: Vec<TypeExpression>,
    }, //Array { }
    Generic {
        identifier: String,
    },
    Variable {
        identifier: String,
        expression: Box<TypeExpression>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TypeExpression {
    pub kind: TypeExpressionKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Operation {
    pub kind: OperationKind,
    pub loc: Loc,
}

pub struct Parser {
    tokens: Vec<Token>,
    pub cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        return Parser { tokens, cursor: 0 };
    }

    pub fn parse_next(&mut self) -> Result<Operation, Diagnostic> {
        let next = &self.tokens[self.cursor].clone();
        self.cursor += 1;
        match next.kind.clone() {
            TokenKind::Identifier { text } => Ok(Operation {
                kind: OperationKind::FunctionCallOrVariable { name: text },
                loc: next.loc.clone(),
            }),
            TokenKind::IntLiteral { value } => Ok(Operation {
                kind: OperationKind::Push {
                    operand: Operand::Int { value: value },
                },
                loc: next.loc.clone(),
            }),
            TokenKind::CharLiteral { value } => Ok(Operation {
                kind: OperationKind::Push {
                    operand: Operand::Char { value: value },
                },
                loc: next.loc.clone(),
            }),
            TokenKind::StringLiteral { value } => Ok(Operation {
                kind: OperationKind::Push {
                    operand: Operand::String { value: value },
                },
                loc: next.loc.clone(),
            }),
            TokenKind::TrueKeyword => Ok(Operation {
                kind: OperationKind::Push {
                    operand: Operand::Bool { value: true },
                },
                loc: next.loc.clone(),
            }),
            TokenKind::FalseKeyword => Ok(Operation {
                kind: OperationKind::Push {
                    operand: Operand::Bool { value: false },
                },
                loc: next.loc.clone(),
            }),
            TokenKind::Plus => Ok(Operation {
                kind: OperationKind::Add,
                loc: next.loc.clone(),
            }),
            TokenKind::Minus => Ok(Operation {
                kind: OperationKind::Sub,
                loc: next.loc.clone(),
            }),
            TokenKind::Star => Ok(Operation {
                kind: OperationKind::Mul,
                loc: next.loc.clone(),
            }),
            TokenKind::Slash => Ok(Operation {
                kind: OperationKind::Div,
                loc: next.loc.clone(),
            }),
            TokenKind::Percent => Ok(Operation {
                kind: OperationKind::Mod,
                loc: next.loc.clone(),
            }),
            TokenKind::Dot => Ok(Operation {
                kind: OperationKind::Identity,
                loc: next.loc.clone(),
            }),
            TokenKind::Equals => Ok(Operation {
                kind: OperationKind::Eq,
                loc: next.loc.clone(),
            }),
            TokenKind::OpenAngle => Ok(Operation {
                kind: OperationKind::Lt,
                loc: next.loc.clone(),
            }),
            TokenKind::CloseAngle => Ok(Operation {
                kind: OperationKind::Gt,
                loc: next.loc.clone(),
            }),
            TokenKind::OpenAngleEquals => Ok(Operation {
                kind: OperationKind::LtEq,
                loc: next.loc.clone(),
            }),
            TokenKind::CloseAngleEquals => Ok(Operation {
                kind: OperationKind::GtEq,
                loc: next.loc.clone(),
            }),
            TokenKind::AndKeyword => Ok(Operation {
                kind: OperationKind::LogicalAnd,
                loc: next.loc.clone(),
            }),
            TokenKind::OrKeyword => Ok(Operation {
                kind: OperationKind::LogicalOr,
                loc: next.loc.clone(),
            }),
            TokenKind::NotKeyword => Ok(Operation {
                kind: OperationKind::LogicalNot,
                loc: next.loc.clone(),
            }),
            TokenKind::PrintKeyword => Ok(Operation {
                kind: OperationKind::Print,
                loc: next.loc.clone(),
            }),
            TokenKind::PopKeyword => Ok(Operation {
                kind: OperationKind::Pop,
                loc: next.loc.clone(),
            }),
            TokenKind::SwapKeyword => Ok(Operation {
                kind: OperationKind::Swap,
                loc: next.loc.clone(),
            }),
            TokenKind::DupKeyword => Ok(Operation {
                kind: OperationKind::Dup,
                loc: next.loc.clone(),
            }),
            TokenKind::OverKeyword => Ok(Operation {
                kind: OperationKind::Over,
                loc: next.loc.clone(),
            }),
            TokenKind::RotKeyword => Ok(Operation {
                kind: OperationKind::Rot,
                loc: next.loc.clone(),
            }),
            TokenKind::LenKeyword => Ok(Operation {
                kind: OperationKind::Len,
                loc: next.loc.clone(),
            }),
            TokenKind::PartialKeyword => Ok(Operation {
                kind: OperationKind::Partial,
                loc: next.loc.clone(),
            }),
            TokenKind::CastKeyword => Ok(Operation {
                kind: OperationKind::Cast,
                loc: next.loc.clone(),
            }),
            TokenKind::ConsKeyword => Ok(Operation {
                kind: OperationKind::Cons,
                loc: next.loc.clone(),
            }),
            TokenKind::ConcatKeyword => Ok(Operation {
                kind: OperationKind::Concat,
                loc: next.loc.clone(),
            }),
            TokenKind::AppendKeyword => Ok(Operation {
                kind: OperationKind::Append,
                loc: next.loc.clone(),
            }),
            TokenKind::MapKeyword => Ok(Operation {
                kind: OperationKind::Map,
                loc: next.loc.clone(),
            }),
            TokenKind::FilterKeyword => Ok(Operation {
                kind: OperationKind::Filter,
                loc: next.loc.clone(),
            }),
            TokenKind::FoldKeyword => Ok(Operation {
                kind: OperationKind::Fold,
                loc: next.loc.clone(),
            }),
            TokenKind::ForeachKeyword => Ok(Operation {
                kind: OperationKind::Foreach,
                loc: next.loc.clone(),
            }),
            TokenKind::DoKeyword => Ok(Operation {
                kind: OperationKind::Do,
                loc: next.loc.clone(),
            }),
            TokenKind::RangeKeyword => Ok(Operation {
                kind: OperationKind::Range,
                loc: next.loc.clone(),
            }),
            TokenKind::ZipKeyword => Ok(Operation {
                kind: OperationKind::Zip,
                loc: next.loc.clone(),
            }),
            TokenKind::PickKeyword => Ok(Operation {
                kind: OperationKind::Pick,
                loc: next.loc.clone(),
            }),
            TokenKind::SliceKeyword => Ok(Operation {
                kind: OperationKind::Slice,
                loc: next.loc.clone(),
            }),
            TokenKind::SplitKeyword => Ok(Operation {
                kind: OperationKind::Split,
                loc: next.loc.clone(),
            }),
            TokenKind::OpenSquare => Ok(Operation {
                kind: OperationKind::Push {
                    operand: self.parse_array()?,
                },
                loc: next.loc.clone(),
            }),
            TokenKind::OpenParen => Ok(Operation {
                kind: OperationKind::Push {
                    operand: self.parse_seq()?,
                },
                loc: next.loc.clone(),
            }),
            TokenKind::LetKeyword => {
                let actual = &self.tokens[self.cursor].clone();
                if let TokenKind::Identifier { text } = &actual.kind {
                    let function_name = text;
                    self.cursor += 1;

                    let mut ins: Vec<TypeExpression> = vec![];
                    let mut outs: Vec<TypeExpression> = vec![];

                    let mut is_base_case_definition = false;

                    //Parse ins
                    while self.cursor < self.tokens.len()
                        && &self.tokens[self.cursor].kind != &TokenKind::Arrow
                    {
                        let type_expression = self.parse_type_expression()?;
                        if let TypeExpressionKind::Pattern { .. } = type_expression.kind {
                            is_base_case_definition = true;
                        }
                        ins.push(type_expression);
                    }
                    self.expect_token(TokenKind::Arrow)?;

                    //Parse outs
                    while self.cursor < self.tokens.len()
                        && &self.tokens[self.cursor].kind != &TokenKind::OpenParen
                    {
                        let type_expression = self.parse_type_expression()?;
                        outs.push(type_expression);
                    }
                    self.expect_token(TokenKind::OpenParen)?;

                    //Parse body
                    let body = self.parse_seq()?;

                    if is_base_case_definition {
                        return Ok(Operation {
                            kind: OperationKind::FunctionBaseCaseDefinition {
                                function: Function {
                                    name: function_name.clone(),
                                    ins,
                                    outs,
                                    body,
                                },
                            },
                            loc: next.loc.clone(),
                        });
                    }

                    return Ok(Operation {
                        kind: OperationKind::FunctionDefinition {
                            function: Function {
                                name: function_name.clone(),
                                ins,
                                outs,
                                body,
                            },
                        },
                        loc: next.loc.clone(),
                    });
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: actual.loc.clone(),
                        message: format!("Expected identifier but got `{:?}", actual.kind),
                        hint: None,
                    });
                }
            }
            TokenKind::SetKeyword => {
                let mut bindings: Vec<Token> = vec![];

                while self.cursor < self.tokens.len()
                    && &self.tokens[self.cursor].kind != &TokenKind::OpenParen
                {
                    let actual = &self.tokens[self.cursor].clone();
                    if let TokenKind::Identifier { text } = &actual.kind {
                        bindings.push(actual.clone());
                        self.cursor += 1;
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: actual.loc.clone(),
                            message: format!("Expected identifier but got `{:?}", actual.kind),
                            hint: None,
                        });
                    }
                }
                self.cursor += 1;

                //Parse body
                let body = self.parse_seq()?;

                return Ok(Operation {
                    kind: OperationKind::Binding { bindings, body },
                    loc: next.loc.clone(),
                });
            }
            TokenKind::IfKeyword => Ok(Operation {
                kind: OperationKind::If,
                loc: next.loc.clone(),
            }),
            TokenKind::ReturnKeyword => Ok(Operation {
                kind: OperationKind::Return,
                loc: next.loc.clone(),
            }),
            TokenKind::ArgsKeyword => Ok(Operation {
                kind: OperationKind::Args,
                loc: next.loc.clone(),
            }),
            TokenKind::Comment { .. } => Ok(Operation {
                kind: OperationKind::Nop,
                loc: next.loc.clone(),
            }),
            TokenKind::CloseSquare
            | TokenKind::CloseParen
            | TokenKind::Whitespace
            | TokenKind::Arrow
            | TokenKind::Colon
            | TokenKind::PrimeLiteral { .. }
            | TokenKind::EOF => Err(Diagnostic {
                severity: Severity::Error,
                loc: next.loc.clone(),
                message: format!("Unexpected token: `{:?}`", next.kind),
                hint: None,
            }),
        }
    }

    fn parse_type_expression(&mut self) -> Result<TypeExpression, Diagnostic> {
        let tok = &self.tokens[self.cursor].clone();
        match &tok.kind {
            TokenKind::Identifier { .. } => {
                if self.cursor + 1 < self.tokens.len()
                    && &self.tokens[self.cursor + 1].kind == &TokenKind::Colon
                {
                    let identifier = tok.text.clone();
                    self.cursor += 1;

                    self.expect_token(TokenKind::Colon)?;

                    let expression = self.parse_type_expression()?;

                    if let TypeExpressionKind::Variable { .. } = expression.kind {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: tok.loc.clone(),
                            message: format!(
                                "Multiple variable bindings per argument is not supported"
                            ),
                            hint: None,
                        });
                    }
                    return Ok(TypeExpression {
                        kind: TypeExpressionKind::Variable {
                            identifier,
                            expression: Box::new(expression),
                        },
                        loc: tok.loc.clone(),
                    });
                }

                let type_expression = TypeExpression {
                    kind: TypeExpressionKind::Identifier {
                        text: tok.text.clone(),
                    },
                    loc: tok.loc.clone(),
                };
                self.cursor += 1;
                return Ok(type_expression);
            }
            TokenKind::IntLiteral { value } => {
                let type_expression = TypeExpression {
                    kind: TypeExpressionKind::Pattern {
                        kind: TypePatternKind::Literal { value: *value },
                    },
                    loc: tok.loc.clone(),
                };
                self.cursor += 1;
                return Ok(type_expression);
            }
            TokenKind::OpenParen => {
                self.cursor += 1;
                let mut ins: Vec<TypeExpression> = vec![];
                let mut outs: Vec<TypeExpression> = vec![];

                //Parse ins
                while self.cursor < self.tokens.len()
                    && &self.tokens[self.cursor].kind != &TokenKind::Arrow
                {
                    let type_expression = self.parse_type_expression()?;
                    ins.push(type_expression);
                }
                self.expect_token(TokenKind::Arrow)?;

                //Parse outs
                while self.cursor < self.tokens.len()
                    && &self.tokens[self.cursor].kind != &TokenKind::CloseParen
                {
                    let type_expression = self.parse_type_expression()?;
                    outs.push(type_expression);
                }
                self.expect_token(TokenKind::CloseParen)?;

                return Ok(TypeExpression {
                    kind: TypeExpressionKind::Function { ins, outs },
                    loc: tok.loc.clone(),
                });
            }
            TokenKind::PrimeLiteral { value } => {
                let type_expression = TypeExpression {
                    kind: TypeExpressionKind::Generic {
                        identifier: value.to_string(),
                    },
                    loc: tok.loc.clone(),
                };
                self.cursor += 1;
                return Ok(type_expression);
            }
            _ => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    loc: tok.loc.clone(),
                    message: format!("Unexpected Token `{:?}`", tok.kind),
                    hint: None,
                });
            }
        }
    }

    fn parse_next_operand(&mut self) -> Result<Operand, Diagnostic> {
        let next = &self.tokens[self.cursor];
        self.cursor += 1;

        match &next.kind {
            TokenKind::IntLiteral { value } => Ok(Operand::Int { value: *value }),
            TokenKind::CharLiteral { value } => Ok(Operand::Char { value: *value }),
            TokenKind::TrueKeyword => Ok(Operand::Bool { value: true }),
            TokenKind::FalseKeyword => Ok(Operand::Bool { value: false }),
            TokenKind::StringLiteral { value } => Ok(Operand::String {
                value: value.to_string(),
            }),
            TokenKind::OpenSquare => self.parse_array(),
            TokenKind::OpenParen => self.parse_seq(),
            _ => Err(Diagnostic {
                severity: Severity::Error,
                loc: next.loc.clone(),
                message: format!("Unsupported value in array literal `{:?}`", next.kind),
                hint: None,
            }),
        }
    }

    fn parse_array(&mut self) -> Result<Operand, Diagnostic> {
        let mut operands: Vec<Operand> = vec![];
        while self.cursor < self.tokens.len()
            && &self.tokens[self.cursor].kind != &TokenKind::CloseSquare
        {
            operands.push(self.parse_next_operand()?);
        }

        if self.cursor >= self.tokens.len() {
            return Err(Diagnostic {
                severity: Severity::Error,
                loc: self.tokens[self.tokens.len() - 1].loc.clone(),
                message: "Unterminated array literal".to_string(),
                hint: None,
            });
        }
        self.expect_token(TokenKind::CloseSquare)?;
        Ok(Operand::Array { values: operands })
    }

    fn parse_seq(&mut self) -> Result<Operand, Diagnostic> {
        let mut ops: Vec<Operation> = vec![];
        while self.cursor < self.tokens.len()
            && &self.tokens[self.cursor].kind != &TokenKind::CloseParen
        {
            ops.push(self.parse_next()?);
        }

        if self.cursor >= self.tokens.len() {
            return Err(Diagnostic {
                severity: Severity::Error,
                loc: self.tokens[self.tokens.len() - 1].loc.clone(),
                message: "Unterminated sequence literal".to_string(),
                hint: None,
            });
        }
        self.expect_token(TokenKind::CloseParen)?;
        Ok(Operand::Seq { ops })
    }

    fn expect_token(&mut self, expected: TokenKind) -> Result<TokenKind, Diagnostic> {
        let actual = &self.tokens[self.cursor];
        self.cursor += 1;

        if actual.kind != expected {
            return Err(Diagnostic {
                severity: Severity::Error,
                loc: actual.loc.clone(),
                message: format!("Expected `{:?}` but got `{:?}", expected, actual),
                hint: None,
            });
        }

        return Ok(actual.kind.clone());
    }
}
