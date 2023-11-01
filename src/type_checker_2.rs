use std::{collections::HashMap, fmt};

use crate::{
    diagnostic::{Diagnostic, Severity},
    lexer::{Loc, Token, TokenKind},
    parser::{Operand, Operation, OperationKind},
};

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
    Generic {
        id: usize,
    },
}

#[derive(Clone, Debug, PartialEq)]
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
                return write!(f, "fn [{}] -> [{}]", ins_list, outs_list);
            }
            TypeKind::Generic { id } => write!(f, "<{}>", id),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CheckedOperation {
    kind: OperationKind,
    loc: Loc,
    ins: Vec<TypeKind>,
    outs: Vec<TypeKind>,
}

#[derive(Debug, Clone)]
struct CheckedFunction {
    name: String,
    ins: Vec<TypeToken>,
    outs: Vec<TypeToken>,
    body: Vec<CheckedOperation>,
}

pub struct TypeChecker2 {
    type_stack: Vec<TypeToken>,
    functions: HashMap<String, CheckedFunction>,
    generics: HashMap<usize, TypeKind>,
    generic_index: usize,
}

impl TypeChecker2 {
    pub fn new() -> TypeChecker2 {
        return TypeChecker2 {
            type_stack: vec![],
            functions: HashMap::new(),
            generics: HashMap::new(),
            generic_index: 0,
        };
    }

    pub fn check_program(
        &mut self,
        ops: &Vec<Operation>,
    ) -> Result<Vec<CheckedOperation>, Diagnostic> {
        let mut checked_ops: Vec<CheckedOperation> = vec![];
        for op in ops {
            //println!("op: {:?}", op);
            let checked_op = self.check_op(op)?;

            //manipulate stack
            for input in &checked_op.ins {
                self.expect_type(input, op.loc.clone())?;
            }
            for output in &checked_op.outs {
                self.type_stack.push(TypeToken {
                    kind: output.clone(),
                    introduced_at: op.loc.clone(),
                });
            }
            // println!("checked op: {:?}", checked_op);
            // println!("stack: {:?}", self.type_stack);
            // println!("------------");
            checked_ops.push(checked_op);
        }
        Ok(checked_ops)
    }

    fn check_op(&mut self, op: &Operation) -> Result<CheckedOperation, Diagnostic> {
        match &op.kind {
            OperationKind::Dup => {
                let generic = self.next_generic();

                Ok(CheckedOperation {
                    kind: OperationKind::Dup,
                    loc: op.loc.clone(),
                    ins: vec![generic.clone()],
                    outs: vec![generic.clone(), generic],
                })
            }
            OperationKind::Push { operand } => {
                let push_type = self.check_operand(operand)?;

                Ok(CheckedOperation {
                    kind: op.kind.clone(),
                    loc: op.loc.clone(),
                    ins: vec![],
                    outs: vec![push_type],
                })
            }
            OperationKind::Pop => {
                let generic = self.next_generic();

                Ok(CheckedOperation {
                    kind: op.kind.clone(),
                    loc: op.loc.clone(),
                    ins: vec![generic],
                    outs: vec![],
                })
            }
            OperationKind::Swap => {
                let a = self.next_generic();
                let b = self.next_generic();

                Ok(CheckedOperation {
                    kind: op.kind.clone(),
                    loc: op.loc.clone(),
                    ins: vec![a.clone(), b.clone()],
                    outs: vec![b, a],
                })
            }
            OperationKind::Add
            | OperationKind::Sub
            | OperationKind::Mul
            | OperationKind::Div
            | OperationKind::Mod => Ok(CheckedOperation {
                kind: op.kind.clone(),
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Int],
            }),
            OperationKind::Gt | OperationKind::GtEq | OperationKind::Lt | OperationKind::LtEq => {
                Ok(CheckedOperation {
                    kind: op.kind.clone(),
                    loc: op.loc.clone(),
                    ins: vec![TypeKind::Int, TypeKind::Int],
                    outs: vec![TypeKind::Bool],
                })
            }
            OperationKind::Eq => {
                let generic = self.next_generic();
                Ok(CheckedOperation {
                    kind: op.kind.clone(),
                    loc: op.loc.clone(),
                    ins: vec![generic.clone(), generic],
                    outs: vec![TypeKind::Bool],
                })
            }
            OperationKind::LogicalAnd | OperationKind::LogicalOr => Ok(CheckedOperation {
                kind: op.kind.clone(),
                loc: op.loc.clone(),
                ins: vec![TypeKind::Bool, TypeKind::Bool],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::LogicalNot => Ok(CheckedOperation {
                kind: op.kind.clone(),
                loc: op.loc.clone(),
                ins: vec![TypeKind::Bool],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::Print => Ok(CheckedOperation {
                kind: op.kind.clone(),
                loc: op.loc.clone(),
                ins: vec![self.next_generic()],
                outs: vec![],
            }),

            OperationKind::FunctionDefinition { function } => {
                if self.functions.contains_key(&function.name) {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!("Function `{}` is already defined", function.name),
                        hint: None,
                    });
                }
                let mut function_type_checker = TypeChecker2::new();
                function_type_checker.functions = self.functions.clone();

                let mut inputs: Vec<TypeToken> = vec![];
                for input in &function.ins {
                    let typ = Self::get_type(input, &mut function_type_checker.type_stack)?;
                    inputs.push(typ.clone());
                    function_type_checker.type_stack.push(TypeToken {
                        kind: typ.kind,
                        introduced_at: input.loc.clone(),
                    });
                }
                let mut outputs: Vec<TypeToken> = vec![];
                for output in &function.outs {
                    let typ = Self::get_type(output, &mut function_type_checker.type_stack)?;
                    outputs.push(typ);
                }

                //For recursion
                function_type_checker.functions.insert(
                    function.name.clone(),
                    CheckedFunction {
                        name: function.name.clone(),
                        ins: inputs.clone(),
                        outs: outputs.clone(),
                        body: vec![],
                    },
                );
                if let Operand::Seq { ops } = &function.body {
                    let checked_body = function_type_checker.check_program(ops)?;

                    self.functions.insert(
                        function.name.clone(),
                        CheckedFunction {
                            name: function.name.clone(),
                            ins: inputs,
                            outs: outputs,
                            body: checked_body,
                        },
                    );
                } else {
                    unreachable!("Function body must be a sequence")
                }

                return Ok(CheckedOperation {
                    kind: op.kind.clone(),
                    loc: op.loc.clone(),
                    ins: vec![],
                    outs: vec![],
                });
            }
            OperationKind::FunctionCall { name } => {
                if self.functions.contains_key(name) {
                    let function = self.functions.get(name).unwrap();
                    let ins = &function.ins.clone();
                    let outs = &function.outs.clone();

                    let mut inputs = vec![];
                    for input in ins {
                        inputs.push(input.kind.clone());
                    }
                    let mut outputs = vec![];
                    for output in outs.clone() {
                        outputs.push(output.kind.clone());
                        self.type_stack.push(output);
                    }
                    Ok(CheckedOperation {
                        kind: op.kind.clone(),
                        loc: op.loc.clone(),
                        ins: inputs,
                        outs: outputs,
                    })
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!("Function `{}` is not defined", name),
                        hint: None,
                    });
                }
            }
            OperationKind::Map => {
                let t = self.next_generic();
                let u = self.next_generic();

                Ok(CheckedOperation {
                    kind: OperationKind::Map,
                    loc: op.loc.clone(),
                    ins: vec![
                        TypeKind::Function {
                            ins: vec![t.clone()],
                            outs: vec![u.clone()],
                        },
                        TypeKind::Array {
                            el_type: Box::new(t),
                        },
                    ],
                    outs: vec![TypeKind::Array {
                        el_type: Box::new(u),
                    }],
                })
            }
            OperationKind::Filter => {
                let t = self.next_generic();

                Ok(CheckedOperation {
                    kind: OperationKind::Map,
                    loc: op.loc.clone(),
                    ins: vec![
                        TypeKind::Function {
                            ins: vec![t.clone()],
                            outs: vec![TypeKind::Bool],
                        },
                        TypeKind::Array {
                            el_type: Box::new(t.clone()),
                        },
                    ],
                    outs: vec![TypeKind::Array {
                        el_type: Box::new(t),
                    }],
                })
            }
            OperationKind::Foreach => {
                let t = self.next_generic();

                Ok(CheckedOperation {
                    kind: OperationKind::Foreach,
                    loc: op.loc.clone(),
                    ins: vec![
                        TypeKind::Function {
                            ins: vec![t.clone()],
                            outs: vec![],
                        },
                        TypeKind::Array {
                            el_type: Box::new(t),
                        },
                    ],
                    outs: vec![],
                })
            }
            OperationKind::Fold => {
                let t = self.next_generic();
                let u = self.next_generic();

                Ok(CheckedOperation {
                    kind: OperationKind::Map,
                    loc: op.loc.clone(),
                    ins: vec![
                        TypeKind::Function {
                            ins: vec![t.clone(), t.clone()],
                            outs: vec![u.clone()],
                        },
                        u.clone(),
                        TypeKind::Array {
                            el_type: Box::new(t),
                        },
                    ],
                    outs: vec![u],
                })
            }
            OperationKind::Concat => {
                let t = self.next_generic();

                Ok(CheckedOperation {
                    kind: OperationKind::Concat,
                    loc: op.loc.clone(),
                    ins: vec![
                        TypeKind::Array {
                            el_type: Box::new(t.clone()),
                        },
                        TypeKind::Array {
                            el_type: Box::new(t.clone()),
                        },
                    ],
                    outs: vec![TypeKind::Array {
                        el_type: Box::new(t),
                    }],
                })
            }
            _ => todo!("{:?}", op.kind),
        }
    }

    fn next_generic(&mut self) -> TypeKind {
        let id = self.generic_index;
        self.generic_index += 1;
        return TypeKind::Generic { id };
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

    fn expect_type(&mut self, expected: &TypeKind, loc: Loc) -> Result<TypeToken, Diagnostic> {
        match self.type_stack.pop() {
            Some(type_token) => {
                self.types_equal(expected, &type_token.kind, loc)?;
                return Ok(type_token);
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

    fn types_equal(
        &mut self,
        left: &TypeKind,
        right: &TypeKind,
        loc: Loc,
    ) -> Result<(), Diagnostic> {
        match (left, right) {
            (TypeKind::Generic { id: left_id }, TypeKind::Generic { id: right_id }) => {
                match (self.generics.get(left_id), self.generics.get(right_id)) {
                    (Some(left_erasure), Some(right_erasure)) => {
                        return self.types_equal(
                            &left_erasure.clone(),
                            &right_erasure.clone(),
                            loc,
                        );
                    }
                    _ => {
                        if let Some(left_erasure) = self.generics.get(left_id) {
                            self.generics.insert(*right_id, left_erasure.clone());
                        }
                        if let Some(right_erasure) = self.generics.get(right_id) {
                            self.generics.insert(*left_id, right_erasure.clone());
                        }
                        return Ok(());
                    }
                }
            }
            (
                TypeKind::Function {
                    ins: left_ins,
                    outs: left_outs,
                },
                TypeKind::Function {
                    ins: right_ins,
                    outs: right_outs,
                },
            ) => {
                if left_ins.len() != right_ins.len() || left_outs.len() != right_outs.len() {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: loc.clone(),
                        message: format!(
                            "Args length mismatch. Expected `{}` but top of type stack was `{}`",
                            left, right
                        ),
                        hint: Some(format!(
                            "{} introduced at {}", //TODO Take actual as a TypeToken and use its loc here
                            right, loc
                        )),
                    });
                }
                for i in 0..left_ins.len() {
                    self.types_equal(&left_ins[i], &right_ins[i], loc.clone())?;
                }
                for i in 0..left_outs.len() {
                    self.types_equal(&left_outs[i], &right_outs[i], loc.clone())?;
                }
                return Ok(());
            }
            (
                TypeKind::Array {
                    el_type: left_el_type,
                },
                TypeKind::Array {
                    el_type: right_el_type,
                },
            ) => {
                self.types_equal(left_el_type, right_el_type, loc)?;
                return Ok(());
            }
            _ => {
                if let TypeKind::Generic { id } = left {
                    match self.generics.get(id) {
                        Some(type_kind) => {
                            return self.types_equal(&type_kind.clone(), right, loc);
                        }
                        None => {
                            self.generics.insert(*id, right.clone());
                            return Ok(());
                        }
                    }
                }
                if let TypeKind::Generic { id } = right {
                    match self.generics.get(id) {
                        Some(type_kind) => {
                            return self.types_equal(left, &type_kind.clone(), loc);
                        }
                        None => {
                            self.generics.insert(*id, left.clone());
                            return Ok(());
                        }
                    }
                }
                if let TypeKind::Function { ins, outs } = left {
                    todo!()
                }
                if let TypeKind::Function { ins, outs } = right {
                    todo!()
                }
                if left != right {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: loc.clone(),
                        message: format!(
                            "Expected `{}` but top of type stack was `{}`",
                            left, right
                        ),
                        hint: Some(format!(
                            "{} introduced at {}", //TODO Take actual as a TypeToken and use its loc here
                            right, loc
                        )),
                    });
                }
            }
        }
        Ok(())
    }

    fn check_operand(&mut self, operand: &Operand) -> Result<TypeKind, Diagnostic> {
        let push_type = match operand {
            Operand::Bool { .. } => TypeKind::Bool,
            Operand::Int { .. } => TypeKind::Int,
            Operand::Char { .. } => TypeKind::Char,
            Operand::Array { values } => {
                if values.len() == 0 {
                    return Ok(TypeKind::Array {
                        el_type: Box::new(self.next_generic()),
                    });
                }
                TypeKind::Array {
                    el_type: Box::new(self.check_operand(&values[0])?),
                }
            }
            Operand::Seq { ops } => {
                let mut sub_type_checker = TypeChecker2::new();
                sub_type_checker.functions = self.functions.clone();

                let mut inferred_ins: Vec<TypeKind> = vec![];
                let mut inferred_outs: Vec<TypeKind> = vec![];
                for op in ops {
                    let checked_op = sub_type_checker.check_op(op)?;

                    for input in checked_op.ins {
                        //Try to take this op's input from the previous output
                        if inferred_outs.is_empty()
                            || !self
                                .types_equal(
                                    &inferred_outs[inferred_outs.len() - 1],
                                    &input,
                                    op.loc.clone(),
                                )
                                .is_ok()
                        {
                            //If we can't, then infer it as an input to the function
                            inferred_ins.push(input);
                        } else {
                            //otherwise try to consume it from the inferred outputs
                            self.types_equal(
                                &input,
                                &inferred_outs.pop().unwrap(),
                                op.loc.clone(),
                            )?;
                        }
                    }
                    for output in checked_op.outs {
                        inferred_outs.push(output);
                    }
                }

                TypeKind::Function {
                    ins: inferred_ins,
                    outs: inferred_outs,
                }
            }
            Operand::String { .. } => todo!(),
        };
        Ok(push_type)
    }
}
