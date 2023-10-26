use crate::{
    diagnostic::{Diagnostic, Severity},
    lexer::{Loc, Token, TokenKind},
    parser::{Operand, Operation, OperationKind},
};

use std::{collections::HashMap, fmt};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Bool,
    Int,
    Char,
    Array {
        el_type: Box<TypeKind>,
    },
    Function {
        ins: Vec<TypeKind>,
        outs: Vec<TypeKind>,
    },
}

#[derive(Clone, Debug)]
enum CheckedOpKind {
    Push,
    Pop,
    PopArray,
}

#[derive(Clone, Debug)]
struct CheckedOperation {
    kind: CheckedOpKind,
    ins: Vec<TypeKind>,
    outs: Vec<TypeKind>,
}

#[derive(Clone, Debug)]
pub struct TypeToken {
    pub kind: TypeKind,
    pub introduced_at: Loc,
}

impl fmt::Display for TypeToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "`{}` introduced at {}", self.kind, self.introduced_at)
    }
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Int => write!(f, "int"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::Array { el_type } => write!(f, "{} array", el_type),
            TypeKind::Function { ins, outs } => {
                let ins_list = ins
                    .into_iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                let outs_list = outs
                    .into_iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                return write!(f, "[{}] -- [{}]", ins_list, outs_list);
            }
        }
    }
}

#[derive(Debug, Clone)]
struct CheckedFunction {
    name: String,
    ins: Vec<TypeToken>,
    outs: Vec<TypeToken>,
    body: Vec<Operation>,
}

pub struct TypeChecker {
    type_stack: Vec<TypeToken>,
    functions: HashMap<String, CheckedFunction>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        return TypeChecker {
            type_stack: vec![],
            functions: HashMap::new(),
        };
    }

    pub fn check(&mut self, op: Operation) -> Result<(), Diagnostic> {
        match op.kind {
            OperationKind::Push { operand } => {
                let operand_type = self.check_operand(operand, op.loc.clone())?;
                self.type_stack.push(TypeToken {
                    kind: operand_type,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Add
            | OperationKind::Sub
            | OperationKind::Mul
            | OperationKind::Div
            | OperationKind::Mod => {
                self.expect_type(TypeKind::Int, op.loc.clone())?;
                self.expect_type(TypeKind::Int, op.loc.clone())?;
                self.type_stack.push(TypeToken {
                    kind: TypeKind::Int,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Identity => {
                let v = self.expect_any(op.loc.clone())?;
                self.type_stack.push(TypeToken {
                    kind: v.kind,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Lt | OperationKind::Gt | OperationKind::LtEq | OperationKind::GtEq => {
                let top = self.expect_types(vec![TypeKind::Int, TypeKind::Char], op.loc.clone())?;
                self.expect_type(top, op.loc.clone())?;
                self.type_stack.push(TypeToken {
                    kind: TypeKind::Bool,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::LogicalAnd | OperationKind::LogicalOr => {
                self.expect_type(TypeKind::Bool, op.loc.clone())?;
                self.expect_type(TypeKind::Bool, op.loc.clone())?;
                self.type_stack.push(TypeToken {
                    kind: TypeKind::Bool,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::LogicalNot => {
                self.expect_type(TypeKind::Bool, op.loc.clone())?;
                self.type_stack.push(TypeToken {
                    kind: TypeKind::Bool,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Print | OperationKind::Pop => match self.type_stack.pop() {
                Some(_kind) => Ok(()),
                None => {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!("Expected any but the type stack was empty"),
                        hint: None,
                    })
                }
            },
            OperationKind::Swap => {
                let a = self.expect_any(op.loc.clone())?;
                let b = self.expect_any(op.loc.clone())?;

                self.type_stack.push(TypeToken {
                    kind: a.kind,
                    introduced_at: op.loc.clone(),
                });
                self.type_stack.push(TypeToken {
                    kind: b.kind,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Dup => {
                let v = self.expect_any(op.loc.clone())?;
                self.type_stack.push(TypeToken {
                    kind: v.kind.clone(),
                    introduced_at: op.loc.clone(),
                });
                self.type_stack.push(TypeToken {
                    kind: v.kind,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Over => {
                let a = self.expect_any(op.loc.clone())?;
                let b = self.expect_any(op.loc.clone())?;

                self.type_stack.push(TypeToken {
                    kind: b.kind.clone(),
                    introduced_at: op.loc.clone(),
                });
                self.type_stack.push(TypeToken {
                    kind: a.kind,
                    introduced_at: op.loc.clone(),
                });
                self.type_stack.push(TypeToken {
                    kind: b.kind,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Rot => {
                let c = self.expect_any(op.loc.clone())?;
                let b = self.expect_any(op.loc.clone())?;
                let a = self.expect_any(op.loc.clone())?;

                self.type_stack.push(TypeToken {
                    kind: b.kind,
                    introduced_at: op.loc.clone(),
                });
                self.type_stack.push(TypeToken {
                    kind: c.kind,
                    introduced_at: op.loc.clone(),
                });
                self.type_stack.push(TypeToken {
                    kind: a.kind,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Eq => {
                let a = self.expect_any(op.loc.clone())?;
                self.expect_type(a.kind, op.loc.clone())?;

                self.type_stack.push(TypeToken {
                    kind: TypeKind::Bool,
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Cons => {
                let a = self.expect_any(op.loc.clone())?;
                let b = self.expect_type(a.kind, op.loc.clone())?;

                self.type_stack.push(TypeToken {
                    kind: TypeKind::Array {
                        el_type: Box::new(b),
                    },
                    introduced_at: op.loc,
                });
                Ok(())
            }
            OperationKind::Len => {
                let array = self.expect_any(op.loc.clone())?;
                match array.kind {
                    TypeKind::Array { .. } => {
                        self.type_stack.push(TypeToken {
                            kind: array.kind,
                            introduced_at: op.loc.clone(),
                        });
                        self.type_stack.push(TypeToken {
                            kind: TypeKind::Int,
                            introduced_at: op.loc,
                        });
                    }
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc,
                            message: format!(
                                "Expected array type but top of type stack was {}",
                                array.kind
                            ),
                            hint: Some(format!("Type stack is: {:#?}", self.type_stack)),
                        });
                    }
                }

                Ok(())
            }
            OperationKind::Concat => {
                let left = self.expect_any(op.loc.clone())?.kind;
                match left.clone() {
                    TypeKind::Array { el_type } => {
                        let left_type = el_type.clone();
                        let right = self.expect_any(op.loc.clone())?.kind;
                        match right.clone() {
                            TypeKind::Array { el_type } => {
                                if left_type != el_type {
                                    return Err(Diagnostic {
                                        severity: Severity::Error,
                                        loc: op.loc,
                                        message: format!(
                                            "Expected `{}` type but top of type stack was `{}`",
                                            left, right
                                        ),
                                        hint: Some(format!(
                                            "Type stack is: {:#?}",
                                            self.type_stack
                                        )),
                                    });
                                }
                                self.type_stack.push(TypeToken {
                                    kind: TypeKind::Array { el_type },
                                    introduced_at: op.loc,
                                });
                                return Ok(());
                            }
                            _ => {
                                return Err(Diagnostic {
                                    severity: Severity::Error,
                                    loc: op.loc,
                                    message: format!(
                                        "Expected array type but top of type stack was {}",
                                        left
                                    ),
                                    hint: Some(format!("Type stack is: {:#?}", self.type_stack)),
                                });
                            }
                        }
                    }
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc,
                            message: format!(
                                "Expected array type but top of type stack was {}",
                                left
                            ),
                            hint: Some(format!("Type stack is: {:#?}", self.type_stack)),
                        });
                    }
                }
            }
            OperationKind::Map => {
                let seq = self.expect_any(op.loc.clone())?;
                if let TypeKind::Function { ins, outs } = seq.kind {
                    let array = self.expect_any(op.loc.clone())?;
                    if let TypeKind::Array { el_type } = array.kind {
                        //Map requires [ T array, fn(T -- U) ]
                        if ins.len() != 1 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function input length mismatch, expected 1 but was {}",
                                    ins.len()
                                ),
                                hint: Some(format!("Inputs are: {:#?}", ins)),
                            });
                        }
                        if outs.len() != 1 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function output length mismatch, expected 1 but was {}",
                                    outs.len()
                                ),
                                hint: Some(format!("Inputs are: {:#?}", outs)),
                            });
                        }
                        //Make sure fn input and array element types match
                        if ins[0] != *el_type {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected `{} array` but got `{} array`",
                                    ins[0], el_type
                                ),
                                hint: Some(format!(
                                    "`{} array` introduced at {}",
                                    el_type, array.introduced_at
                                )),
                            });
                        }
                        //All good, place an array of out type onto the stack
                        self.type_stack.push(TypeToken {
                            kind: TypeKind::Array {
                                el_type: Box::new(outs[0].clone()),
                            },
                            introduced_at: op.loc,
                        });
                        return Ok(());
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc,
                            message: format!(
                                "Expected array type but top of type stack was {}",
                                array.kind
                            ),
                            hint: Some(format!(
                                "`{}` introduced at {}",
                                array.kind, array.introduced_at
                            )),
                        });
                    }
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected function type but top of type stack was {}",
                            seq.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            seq.kind, seq.introduced_at
                        )),
                    });
                }
            }
            OperationKind::Filter => {
                let seq = self.expect_any(op.loc.clone())?;
                if let TypeKind::Function { ins, outs } = seq.kind.clone() {
                    let array = self.expect_any(op.loc.clone())?;
                    if let TypeKind::Array { el_type } = array.kind {
                        //Filter requires [ T array, fn(T -- bool) ]
                        if ins.len() != 1 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function input length mismatch, expected 1 but was {}",
                                    ins.len()
                                ),
                                hint: Some(format!("Inputs are: {:#?}", ins)),
                            });
                        }
                        if outs.len() != 1 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function output length mismatch, expected 1 but was {}",
                                    outs.len()
                                ),
                                hint: Some(format!("Outputs are: {:#?}", outs)),
                            });
                        }
                        //Make sure fn input and array element types match
                        if ins[0] != *el_type {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected `{} array` but got `{} array`",
                                    ins[0], el_type
                                ),
                                hint: Some(format!(
                                    "`{} array` introduced at {}",
                                    el_type, array.introduced_at
                                )),
                            });
                        }
                        if outs[0] != TypeKind::Bool {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected function returning bool but got `fn {}`",
                                    seq.kind
                                ),
                                hint: Some(format!(
                                    "`{} array` introduced at {}",
                                    el_type, array.introduced_at
                                )),
                            });
                        }
                        //All good, place an array of in type onto the stack
                        self.type_stack.push(TypeToken {
                            kind: TypeKind::Array {
                                el_type: Box::new(ins[0].clone()),
                            },
                            introduced_at: op.loc,
                        });
                        return Ok(());
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc,
                            message: format!(
                                "Expected array type but top of type stack was {}",
                                array.kind
                            ),
                            hint: Some(format!(
                                "`{}` introduced at {}",
                                array.kind, array.introduced_at
                            )),
                        });
                    }
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected function type but top of type stack was {}",
                            seq.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            seq.kind, seq.introduced_at
                        )),
                    });
                }
            }
            OperationKind::Foreach => {
                let seq = self.expect_any(op.loc.clone())?;
                if let TypeKind::Function { ins, outs } = seq.kind {
                    let array = self.expect_any(op.loc.clone())?;
                    if let TypeKind::Array { el_type } = array.kind {
                        //Map requires [ T array, fn(T -> ) ]
                        if ins.len() != 1 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function input length mismatch, expected 1 but was {}",
                                    ins.len()
                                ),
                                hint: Some(format!("Inputs are: {:#?}", ins)),
                            });
                        }
                        if outs.len() != 0 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function output length mismatch, expected 0 but was {}",
                                    outs.len()
                                ),
                                hint: Some(format!("Outputs are: {:#?}", outs)),
                            });
                        }
                        //Make sure fn input and array element types match
                        if ins[0] != *el_type {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected `{} array` but got `{} array`",
                                    ins[0], el_type
                                ),
                                hint: Some(format!(
                                    "`{} array` introduced at {}",
                                    el_type, array.introduced_at
                                )),
                            });
                        }
                        //All good
                        return Ok(());
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc,
                            message: format!(
                                "Expected array type but top of type stack was {}",
                                array.kind
                            ),
                            hint: Some(format!(
                                "`{}` introduced at {}",
                                array.kind, array.introduced_at
                            )),
                        });
                    }
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected function type but top of type stack was {}",
                            seq.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            seq.kind, seq.introduced_at
                        )),
                    });
                }
            }
            OperationKind::Call => {
                let seq = self.expect_any(op.loc.clone())?;
                if let TypeKind::Function { ins, outs } = seq.kind {
                    for in_type in ins {
                        self.expect_type(in_type, op.loc.clone())?;
                    }
                    for out_type in outs {
                        self.type_stack.push(TypeToken {
                            kind: out_type,
                            introduced_at: op.loc.clone(),
                        });
                    }
                    Ok(())
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected function type but top of type stack was {}",
                            seq.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            seq.kind, seq.introduced_at
                        )),
                    });
                }
            }
            OperationKind::Zip => {
                let left = self.expect_any(op.loc.clone())?;
                if let TypeKind::Array { el_type } = left.kind {
                    let left_el_type = *el_type;
                    let right = self.expect_any(op.loc.clone())?;
                    let right_kind = right.kind;
                    if let TypeKind::Array { el_type } = right_kind.clone() {
                        if left_el_type != *el_type {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected `{}` but top of type stack was `{}`",
                                    left_el_type, el_type
                                ),
                                hint: Some(format!(
                                    "`{}` introduced at {}",
                                    right_kind, right.introduced_at
                                )),
                            });
                        }
                        self.type_stack.push(TypeToken {
                            kind: TypeKind::Array {
                                el_type: Box::new(TypeKind::Array { el_type }),
                            },
                            introduced_at: op.loc,
                        });
                        Ok(())
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc,
                            message: format!(
                                "Expected array type but top of type stack was {}",
                                right_kind
                            ),
                            hint: Some(format!(
                                "`{}` introduced at {}",
                                right_kind, right.introduced_at
                            )),
                        });
                    }
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected array type but top of type stack was {}",
                            left.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            left.kind, left.introduced_at
                        )),
                    });
                }
            }
            OperationKind::Pick => {
                self.expect_type(TypeKind::Int, op.loc.clone())?;
                let array = self.expect_any(op.loc.clone())?;
                if let TypeKind::Array { el_type } = array.kind.clone() {
                    self.type_stack.push(array);
                    self.type_stack.push(TypeToken {
                        kind: *el_type,
                        introduced_at: op.loc,
                    });
                    Ok(())
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected array type but top of type stack was {}",
                            array.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            array.kind, array.introduced_at
                        )),
                    });
                }
            }
            OperationKind::Slice => {
                self.expect_type(TypeKind::Int, op.loc.clone())?;
                self.expect_type(TypeKind::Int, op.loc.clone())?;
                let array = self.expect_any(op.loc.clone())?;
                if let TypeKind::Array { .. } = array.kind.clone() {
                    self.type_stack.push(array);
                    Ok(())
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected array type but top of type stack was {}",
                            array.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            array.kind, array.introduced_at
                        )),
                    });
                }
            }
            OperationKind::Split => {
                let seq = self.expect_any(op.loc.clone())?;
                if let TypeKind::Function { ins, outs } = seq.kind.clone() {
                    let array = self.expect_any(op.loc.clone())?;
                    if let TypeKind::Array { el_type } = array.kind {
                        //stack must be [a] (a -> bool)
                        if ins.len() != 1 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function input length mismatch, expected 1 but was {}",
                                    ins.len()
                                ),
                                hint: Some(format!("Inputs are: {:#?}", ins)),
                            });
                        }
                        if outs.len() != 1 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function output length mismatch, expected 1 but was {}",
                                    outs.len()
                                ),
                                hint: Some(format!("Outputs are: {:#?}", outs)),
                            });
                        }
                        //Make sure fn input and array element types match
                        if ins[0] != *el_type {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected `{} array` but got `{} array`",
                                    ins[0], el_type
                                ),
                                hint: Some(format!(
                                    "`{} array` introduced at {}",
                                    el_type, array.introduced_at
                                )),
                            });
                        }
                        //Make sure fn output is bool
                        if outs[0] != TypeKind::Bool {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected function to output `bool` but got `{}`",
                                    outs[0]
                                ),
                                hint: Some(format!("`{}` introduced at {}", outs[0], outs[0])),
                            });
                        }

                        //All good, place two arrays of in type onto the stack
                        self.type_stack.push(TypeToken {
                            kind: TypeKind::Array {
                                el_type: Box::new(ins[0].clone()),
                            },
                            introduced_at: op.loc.clone(),
                        });
                        self.type_stack.push(TypeToken {
                            kind: TypeKind::Array {
                                el_type: Box::new(ins[0].clone()),
                            },
                            introduced_at: op.loc,
                        });
                        return Ok(());
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc,
                            message: format!(
                                "Expected array type but top of type stack was {}",
                                array.kind
                            ),
                            hint: Some(format!(
                                "`{}` introduced at {}",
                                array.kind, array.introduced_at
                            )),
                        });
                    }
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected function type but top of type stack was {}",
                            seq.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            seq.kind, seq.introduced_at
                        )),
                    });
                }
            }
            OperationKind::Fold => {
                //fold needs [T array, U, fn([T T -- U])]
                let seq = self.expect_any(op.loc.clone())?;
                if let TypeKind::Function { ins, outs } = seq.kind.clone() {
                    let accumulator = self.expect_any(op.loc.clone())?;
                    let array = self.expect_any(op.loc.clone())?;
                    if let TypeKind::Array { el_type } = array.kind {
                        if ins.len() != 2 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function input length mismatch, expected 2 but was {}",
                                    ins.len()
                                ),
                                hint: Some(format!("Inputs are: {:#?}", ins)),
                            });
                        }
                        if outs.len() != 1 {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Function output length mismatch, expected 1 but was {}",
                                    outs.len()
                                ),
                                hint: Some(format!("Outputs are: {:#?}", outs)),
                            });
                        }
                        //Make sure fn input and array element types match
                        if ins[0] != *el_type {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected `{} array` but got `{} array`",
                                    ins[0], el_type
                                ),
                                hint: Some(format!(
                                    "`{} array` introduced at {}",
                                    el_type, array.introduced_at
                                )),
                            });
                        }
                        if outs[0] != accumulator.kind {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc,
                                message: format!(
                                    "Expected function returning `{}` but got `fn {}`",
                                    accumulator.kind, seq.kind
                                ),
                                hint: Some(format!(
                                    "`{}` introduced at {}",
                                    accumulator.kind, accumulator.introduced_at
                                )),
                            });
                        }
                        //All good, place an accumulator type onto the stack
                        self.type_stack.push(TypeToken {
                            kind: accumulator.kind,
                            introduced_at: op.loc,
                        });
                        return Ok(());
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc,
                            message: format!(
                                "Expected array type but top of type stack was {}",
                                array.kind
                            ),
                            hint: Some(format!(
                                "`{}` introduced at {}",
                                array.kind, array.introduced_at
                            )),
                        });
                    }
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected function type but top of type stack was {}",
                            seq.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            seq.kind, seq.introduced_at
                        )),
                    });
                }
            }
            OperationKind::FunctionDefinition { function } => {
                if self.functions.contains_key(&function.name) {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!("Function `{}` is already defined", function.name),
                        hint: None,
                    });
                }

                let mut sub_type_checker = TypeChecker {
                    type_stack: vec![],
                    functions: self.functions.clone(),
                };

                for input in function.ins.clone() {
                    let typ = Self::get_type(&input, &mut sub_type_checker.type_stack)?;
                    sub_type_checker.type_stack.push(TypeToken {
                        kind: typ.kind,
                        introduced_at: input.loc,
                    });
                }
                let ins = sub_type_checker
                    .type_stack
                    .clone()
                    .into_iter()
                    .rev()
                    .collect::<Vec<TypeToken>>();

                let mut outs: Vec<TypeToken> = vec![];
                for output in function.outs.clone() {
                    let typ = Self::get_type(&output, &mut outs)?;
                    outs.push(typ);
                }

                let checked_function = CheckedFunction {
                    name: function.name.clone(),
                    ins: ins.clone(),
                    outs: outs.clone(),
                    body: vec![],
                };

                //Insert an empty function definition in case the function is recursive
                sub_type_checker
                    .functions
                    .insert(checked_function.name.clone(), checked_function.clone());

                //Check body
                if let Operand::Seq { ops } = function.body.clone() {
                    for op in ops.clone() {
                        sub_type_checker.check(op)?
                    }

                    //Check outputs
                    for output_type in outs.clone().into_iter().rev() {
                        sub_type_checker
                            .expect_type(output_type.kind, output_type.introduced_at)?;
                    }

                    if !sub_type_checker.type_stack.is_empty() {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: ops[ops.len() - 1].loc.clone(),
                            message: format!(
                                "Type stack at end of `{}` does not match signature `[{}] -> [{}]`",
                                function.name,
                                checked_function
                                    .ins
                                    .into_iter()
                                    .map(|s| s.kind.to_string())
                                    .collect::<Vec<String>>()
                                    .join(" "),
                                checked_function
                                    .outs
                                    .into_iter()
                                    .map(|s| s.kind.to_string())
                                    .collect::<Vec<String>>()
                                    .join(" "),
                            ),
                            hint: Some(format!(
                                "Type stack at the end of execution was:\n\t{}",
                                sub_type_checker
                                    .type_stack()
                                    .into_iter()
                                    .rev()
                                    .map(|s| s.to_string())
                                    .collect::<Vec<String>>()
                                    .join("\n\t")
                            )),
                        });
                    }

                    let checked_function = CheckedFunction {
                        name: function.name,
                        ins: ins.clone(),
                        outs: outs,
                        body: ops,
                    };

                    self.functions
                        .insert(checked_function.name.clone(), checked_function);
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            OperationKind::FunctionCall { name } => {
                if self.functions.contains_key(&name) {
                    let function = self.functions.get(&name).unwrap();
                    let ins = &function.ins.clone();
                    let outs = &function.outs.clone();
                    for input in ins.clone() {
                        self.expect_type(input.kind, input.introduced_at)?;
                    }
                    for output in outs.clone() {
                        self.type_stack.push(output);
                    }
                    Ok(())
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!("Function `{}` is not defined", name),
                        hint: None,
                    });
                }
            }
            OperationKind::If => {
                let false_branch = self.expect_any(op.loc.clone())?;
                if let TypeKind::Function {
                    ins: false_ins,
                    outs: false_outs,
                } = false_branch.kind
                {
                    let true_branch = self.expect_any(op.loc.clone())?;
                    if let TypeKind::Function {
                        ins: true_ins,
                        outs: true_outs,
                    } = true_branch.kind
                    {
                        self.expect_type(TypeKind::Bool, op.loc.clone())?;
                        if false_ins != true_ins || false_outs != true_outs {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc.clone(),
                                message: format!(
                                    "Branches in `if` function must have the same signature.\n\ttrue:  `{}`\n\tfalse: `{}`",
                                    format!("fn [{}] -> [{}]", 
                                        true_ins.into_iter().map(|i| i.to_string()).collect::<Vec<String>>().join(" "), 
                                        true_outs.into_iter().map(|o| o.to_string()).collect::<Vec<String>>().join(" ")
                                    ),
                                    format!("fn [{}] -> [{}]", 
                                        false_ins.into_iter().map(|i| i.to_string()).collect::<Vec<String>>().join(" "), 
                                        false_outs.into_iter().map(|o| o.to_string()).collect::<Vec<String>>().join(" ")
                                    ),
                                ),
                                hint: None,
                            });
                        }
                        for input in true_ins {
                            self.expect_type(input, op.loc.clone())?;
                        }
                        for output in true_outs {
                            self.type_stack.push(TypeToken {
                                kind: output,
                                introduced_at: op.loc.clone(),
                            })
                        }
                        Ok(())
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc.clone(),
                            message: format!(
                                "Expected function type but top of type stack was {}",
                                true_branch.kind
                            ),
                            hint: Some(format!(
                                "`{}` introduced at {}",
                                true_branch.kind, true_branch.introduced_at
                            )),
                        });
                    }
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc,
                        message: format!(
                            "Expected function type but top of type stack was {}",
                            false_branch.kind
                        ),
                        hint: Some(format!(
                            "`{}` introduced at {}",
                            false_branch.kind, false_branch.introduced_at
                        )),
                    });
                }
            },
            OperationKind::Return => {
                todo!(); //need to know function context
            }
        }
    }

    pub fn type_stack(&self) -> &Vec<TypeToken> {
        return &self.type_stack;
    }

    fn check_operand(&mut self, operand: Operand, loc: Loc) -> Result<TypeKind, Diagnostic> {
        match operand {
            Operand::Bool { .. } => Ok(TypeKind::Bool),
            Operand::Int { .. } => Ok(TypeKind::Int),
            Operand::Char { .. } => Ok(TypeKind::Char),
            Operand::String { .. } => Ok(TypeKind::Array {
                el_type: Box::new(TypeKind::Char),
            }),
            Operand::Array { values } => {
                if values.is_empty() {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: loc.clone(),
                        message: "Empty arrays cannot be type checked yet".to_string(),
                        hint: None,
                    });
                }
                let values_type = self.check_operand(values[0].clone(), loc.clone())?;
                for el in values {
                    let el_type = self.check_operand(el, loc.clone())?;
                    if el_type != values_type {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: loc.clone(),
                            message: format!(
                                "Expected `{:?}` but got `{:?}`",
                                values_type, el_type
                            ),
                            hint: None,
                        });
                    }
                }
                return Ok(TypeKind::Array {
                    el_type: Box::new(values_type),
                });
            }
            Operand::Seq { ops } => {
                let mut inferred_ins: Vec<TypeKind> = vec![];
                let mut inferred_outs: Vec<TypeKind> = vec![];
                for op in ops {
                    //TODO: can check() be refactored to return this info, so that I don't need to implement it all twice?
                    match op.kind.clone() {
                        OperationKind::Push { operand } => {
                            let op_type = self.check_operand(operand, op.loc)?;
                            inferred_outs.push(op_type);
                        }
                        OperationKind::Add => {
                            let inputs = vec![TypeKind::Int, TypeKind::Int];
                            let outputs = vec![TypeKind::Int];

                            Self::infer_operation(
                                inputs,
                                &mut inferred_outs,
                                &op,
                                &mut inferred_ins,
                                outputs,
                            )?
                        }
                        OperationKind::Sub
                        | OperationKind::Mul
                        | OperationKind::Div
                        | OperationKind::Mod => {
                            let inputs = vec![TypeKind::Int, TypeKind::Int];
                            let outputs = vec![TypeKind::Int];

                            Self::infer_operation(
                                inputs,
                                &mut inferred_outs,
                                &op,
                                &mut inferred_ins,
                                outputs,
                            )?;
                        }
                        OperationKind::LogicalNot => {
                            let inputs = vec![TypeKind::Bool];
                            let outputs = vec![TypeKind::Bool];

                            Self::infer_operation(
                                inputs,
                                &mut inferred_outs,
                                &op,
                                &mut inferred_ins,
                                outputs,
                            )?;
                        }
                        OperationKind::LogicalAnd | OperationKind::LogicalOr => {
                            let inputs = vec![TypeKind::Bool, TypeKind::Bool];
                            let outputs = vec![TypeKind::Bool];

                            Self::infer_operation(
                                inputs,
                                &mut inferred_outs,
                                &op,
                                &mut inferred_ins,
                                outputs,
                            )?;
                        }
                        OperationKind::Gt | OperationKind::Lt => {
                            let inputs = vec![TypeKind::Int, TypeKind::Int];
                            let outputs = vec![TypeKind::Bool];

                            Self::infer_operation(
                                inputs,
                                &mut inferred_outs,
                                &op,
                                &mut inferred_ins,
                                outputs,
                            )?;
                        }
                        OperationKind::FunctionCall { name } => {
                            if self.functions.contains_key(&name) {
                                let function = self.functions.get(&name).unwrap();
                                let ins = &function.ins.clone();
                                let outs = &function.outs.clone();

                                for input in ins.clone() {
                                    //TODO This can be reused for each op
                                    match inferred_outs.pop() {
                                        Some(top) => {
                                            Self::check_types_equal(
                                                top,
                                                input.kind,
                                                input.introduced_at,
                                            )?;
                                        }
                                        None => match inferred_ins.pop() {
                                            Some(top) => {
                                                Self::check_types_equal(
                                                    top,
                                                    input.kind,
                                                    input.introduced_at,
                                                )?;
                                            }
                                            None => inferred_ins.push(input.kind),
                                        },
                                    }
                                }
                                for output in outs.clone() {
                                    match inferred_outs.pop() {
                                        Some(top) => {
                                            Self::check_types_equal(
                                                top,
                                                output.kind.clone(),
                                                output.introduced_at,
                                            )?;
                                            inferred_outs.push(output.kind);
                                        }
                                        None => match inferred_ins.pop() {
                                            Some(top) => {
                                                inferred_ins.push(top.clone());
                                                inferred_outs.push(output.kind);
                                            }
                                            None => inferred_ins.push(output.kind),
                                        },
                                    }
                                }
                            } else {
                                return Err(Diagnostic {
                                    severity: Severity::Error,
                                    loc: op.loc,
                                    message: format!("Function `{}` is not defined", name),
                                    hint: None,
                                });
                            }
                        }
                        _ => todo!("{:?}", op),
                    }
                    // println!("op: {:?}", op.kind);
                    // println!("Inferred ins: {:?}", inferred_ins);
                    // println!("Inferred outs: {:?}", inferred_outs);
                    // println!("\n");
                }
                return Ok(TypeKind::Function {
                    ins: inferred_ins,
                    outs: inferred_outs,
                });
            }
        }
    }

    fn check_types_equal(actual: TypeKind, expected: TypeKind, loc: Loc) -> Result<(), Diagnostic> {
        if actual != expected {
            return Err(Diagnostic {
                severity: Severity::Error,
                loc: loc.clone(),
                message: format!(
                    "Expected `{}` type but top of type stack was `{}`",
                    expected, actual
                ),
                hint: Some(format!("{} expected at {}", expected, loc)),
            });
        }
        Ok(())
    }

    fn get_type(token: &Token, type_stack: &mut Vec<TypeToken>) -> Result<TypeToken, Diagnostic> {
        if let TokenKind::Identifier { text } = &token.kind {
            match text.as_str() {
                "int" => {
                    return Ok(TypeToken {
                        kind: TypeKind::Int,
                        introduced_at: token.loc.clone(),
                    })
                }
                "bool" => {
                    return Ok(TypeToken {
                        kind: TypeKind::Bool,
                        introduced_at: token.loc.clone(),
                    })
                }
                "char" => {
                    return Ok(TypeToken {
                        kind: TypeKind::Char,
                        introduced_at: token.loc.clone(),
                    })
                }
                "string" => {
                    return Ok(TypeToken {
                        kind: TypeKind::Array {
                            el_type: Box::new(TypeKind::Char),
                        },
                        introduced_at: token.loc.clone(),
                    })
                }
                "array" => match type_stack.pop() {
                    Some(type_token) => {
                        return Ok(TypeToken {
                            kind: TypeKind::Array {
                                el_type: Box::new(type_token.kind),
                            },
                            introduced_at: token.loc.clone(),
                        });
                    }
                    None => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: token.loc.clone(),
                            message: format!("Expected type but type stack was empty"),
                            hint: Some("Declare array types as `<element type> array`".to_owned()),
                        })
                    }
                },
                _ => {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: token.loc.clone(),
                        message: format!("Type `{:?}` is not defined", text),
                        hint: None,
                    })
                }
            }
        } else {
            return Err(Diagnostic {
                severity: Severity::Error,
                loc: token.loc.clone(),
                message: format!("Expected identifier but got `{:?}`", token.kind),
                hint: None,
            });
        }
    }

    fn expect_any(&mut self, loc: Loc) -> Result<TypeToken, Diagnostic> {
        match self.type_stack.pop() {
            Some(type_token) => return Ok(type_token),
            None => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    loc: loc,
                    message: format!("Expected any but the type stack was empty"),
                    hint: None,
                })
            }
        }
    }

    fn expect_type(&mut self, expected: TypeKind, loc: Loc) -> Result<TypeKind, Diagnostic> {
        match self.type_stack.pop() {
            Some(type_token) => {
                if type_token.kind == expected {
                    return Ok(type_token.kind);
                }
                return Err(Diagnostic {
                    severity: Severity::Error,
                    loc: loc,
                    message: format!(
                        "Expected `{}` but top of type stack was `{}`",
                        expected, type_token.kind
                    ),
                    hint: Some(format!(
                        "`{}` introduced at {}",
                        type_token.kind, type_token.introduced_at
                    )),
                });
            }
            None => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    loc: loc,
                    message: format!("Expected `{}` but the type stack was empty", expected),
                    hint: None,
                })
            }
        }
    }
    fn expect_types(&mut self, expected: Vec<TypeKind>, loc: Loc) -> Result<TypeKind, Diagnostic> {
        match self.type_stack.pop() {
            Some(type_token) => {
                if expected.contains(&type_token.kind) {
                    return Ok(type_token.kind);
                }
                return Err(Diagnostic {
                    severity: Severity::Error,
                    loc: loc,
                    message: format!(
                        "Expected one of `{:?}` but top of type stack was `{}`",
                        expected, type_token.kind
                    ),
                    hint: Some(format!(
                        "`{}` introduced at {}",
                        type_token.kind, type_token.introduced_at
                    )),
                });
            }
            None => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    loc: loc,
                    message: format!(
                        "Expected one of `{:?}` but the type stack was empty",
                        expected
                    ),
                    hint: None,
                })
            }
        }
    }

    fn infer_operation(
        inputs: Vec<TypeKind>,
        inferred_outs: &mut Vec<TypeKind>,
        op: &Operation,
        inferred_ins: &mut Vec<TypeKind>,
        outputs: Vec<TypeKind>,
    ) -> Result<(), Diagnostic> {
        for input in inputs {
            match inferred_outs.pop() {
                Some(top) => {
                    Self::check_types_equal(top, input, op.loc.clone())?;
                }
                None => match inferred_ins.pop() {
                    Some(top) => {
                        Self::check_types_equal(top, input.clone(), op.loc.clone())?;
                        inferred_ins.push(input.clone()); //put back
                        inferred_ins.push(input); //add another
                    }
                    None => inferred_ins.push(input),
                },
            }
        }
        Ok(for output in outputs {
            match inferred_outs.pop() {
                Some(top) => {
                    Self::check_types_equal(top, output.clone(), op.loc.clone())?;
                    inferred_outs.push(output);
                }
                None => match inferred_ins.pop() {
                    Some(top) => {
                        inferred_ins.push(top.clone());
                        inferred_outs.push(output);
                    }
                    None => {
                        inferred_ins.push(output);
                    }
                },
            }
        })
    }
}
