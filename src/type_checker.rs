use std::{collections::HashMap, fmt};

use crate::{
    diagnostic::{Diagnostic, Severity},
    lexer::{Loc, Token, TokenKind},
    parser::{
        Operand, Operation, OperationKind, TypeExpression, TypeExpressionKind, TypePatternKind,
    },
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CheckedOperation {
    pub kind: CheckedOpKind,
    loc: Loc,
    ins: Vec<TypeKind>,
    outs: Vec<TypeKind>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CheckedFunction {
    pub name: String,
    ins: Vec<TypeToken>,
    outs: Vec<TypeToken>,
    pub body: Vec<CheckedOperation>,
    pub base_cases: Vec<BaseCase>, //TODO: This is hella wonkwards
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BaseCase {
    pub function_name: String,
    pub input: usize,
    pub body: Vec<CheckedOperation>,
}

#[derive(Debug, Clone)]
pub struct TypeChecker {
    type_stack: Vec<TypeToken>,
    functions: HashMap<String, CheckedFunction>,
    base_cases: HashMap<String, Vec<BaseCase>>,
    generics: HashMap<usize, TypeKind>,
    generic_index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum CheckedOpKind {
    Push { operand: Operand },
    Pop,
    Swap,
    Dup,
    Over,
    Rot,
    FunctionDefinition { function: CheckedFunction },
    FunctionBaseCaseDefinition { base_case: BaseCase },
    FunctionCall { name: String },
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
    Call,
    Range,
    Zip,
    Pick,
    Slice,
    Split,
    Partial,
    If,
    Return,
    Args,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        return TypeChecker {
            type_stack: vec![],
            functions: HashMap::new(),
            generics: HashMap::new(),
            generic_index: 0,
            base_cases: HashMap::new(),
        };
    }

    pub fn check_program(
        &mut self,
        ops: &Vec<Operation>,
    ) -> Result<Vec<CheckedOperation>, Diagnostic> {
        let mut checked_ops: Vec<CheckedOperation> = vec![];
        for op in ops {
            // println!("stack: {:?}", self.type_stack);
            // println!("op: {:?}", op);
            let checked_op = self.check_op(op)?;
            // println!("checked op: {:?}", checked_op);

            //manipulate stack
            for input in &checked_op.ins {
                // println!("checking input: {:?}", input);
                // println!("stack: {:?}", self.type_stack);
                self.expect_type(input, op.loc.clone())?;
            }
            for output in &checked_op.outs {
                self.type_stack.push(TypeToken {
                    kind: output.clone(),
                    introduced_at: op.loc.clone(),
                });
            }
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
                    kind: CheckedOpKind::Dup,
                    loc: op.loc.clone(),
                    ins: vec![generic.clone()],
                    outs: vec![generic.clone(), generic],
                })
            }
            OperationKind::Push { operand } => {
                let push_type = self.check_operand(operand)?;

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Push {
                        operand: operand.clone(),
                    },
                    loc: op.loc.clone(),
                    ins: vec![],
                    outs: vec![push_type],
                })
            }
            OperationKind::Pop => {
                let generic = self.next_generic();

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Pop,
                    loc: op.loc.clone(),
                    ins: vec![generic],
                    outs: vec![],
                })
            }
            OperationKind::Swap => {
                let a = self.next_generic();
                let b = self.next_generic();

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Swap,
                    loc: op.loc.clone(),
                    ins: vec![a.clone(), b.clone()],
                    outs: vec![a, b],
                })
            }
            OperationKind::Rot => {
                let a = self.next_generic();
                let b = self.next_generic();
                let c = self.next_generic();

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Rot,
                    loc: op.loc.clone(),
                    ins: vec![a.clone(), b.clone(), c.clone()],
                    outs: vec![b, a, c],
                })
            }
            OperationKind::Over => {
                let a = self.next_generic();
                let b = self.next_generic();

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Over,
                    loc: op.loc.clone(),
                    ins: vec![a.clone(), b.clone()],
                    outs: vec![b.clone(), a, b],
                })
            }
            OperationKind::Add => Ok(CheckedOperation {
                kind: CheckedOpKind::Add,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Int],
            }),
            OperationKind::Sub => Ok(CheckedOperation {
                kind: CheckedOpKind::Sub,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Int],
            }),
            OperationKind::Mul => Ok(CheckedOperation {
                kind: CheckedOpKind::Mul,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Int],
            }),
            OperationKind::Div => Ok(CheckedOperation {
                kind: CheckedOpKind::Div,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Int],
            }),
            OperationKind::Mod => Ok(CheckedOperation {
                kind: CheckedOpKind::Mod,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Int],
            }),
            OperationKind::Gt => Ok(CheckedOperation {
                kind: CheckedOpKind::Gt,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::GtEq => Ok(CheckedOperation {
                kind: CheckedOpKind::GtEq,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::Lt => Ok(CheckedOperation {
                kind: CheckedOpKind::Lt,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::LtEq => Ok(CheckedOperation {
                kind: CheckedOpKind::LtEq,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Int, TypeKind::Int],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::Eq => {
                let generic = self.next_generic();
                Ok(CheckedOperation {
                    kind: CheckedOpKind::Eq,
                    loc: op.loc.clone(),
                    ins: vec![generic.clone(), generic],
                    outs: vec![TypeKind::Bool],
                })
            }
            OperationKind::LogicalAnd => Ok(CheckedOperation {
                kind: CheckedOpKind::LogicalAnd,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Bool, TypeKind::Bool],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::LogicalOr => Ok(CheckedOperation {
                kind: CheckedOpKind::LogicalOr,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Bool, TypeKind::Bool],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::LogicalNot => Ok(CheckedOperation {
                kind: CheckedOpKind::LogicalNot,
                loc: op.loc.clone(),
                ins: vec![TypeKind::Bool],
                outs: vec![TypeKind::Bool],
            }),
            OperationKind::Print => Ok(CheckedOperation {
                kind: CheckedOpKind::Print,
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
                let mut function_type_checker = TypeChecker::new();
                function_type_checker.functions = self.functions.clone();

                let mut inputs: Vec<TypeToken> = vec![];
                for input in &function.ins {
                    let typ = Self::get_type(input, &mut inputs)?;
                    inputs.push(typ);
                }
                for input in &inputs {
                    function_type_checker.type_stack.push(input.clone());
                }
                let mut outputs: Vec<TypeToken> = vec![];
                for output in &function.outs {
                    let typ = Self::get_type(output, &mut outputs)?;
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
                        base_cases: vec![],
                    },
                );
                if let Operand::Seq { ops } = &function.body {
                    let checked_body = function_type_checker.check_program(ops)?;

                    let end_type_stack = function_type_checker.type_stack.clone();
                    for output_type in outputs.clone().into_iter().rev() {
                        function_type_checker
                            .expect_type(&output_type.kind, output_type.introduced_at)?;
                    }

                    if !function_type_checker.type_stack.is_empty() && !outputs.is_empty() {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: outputs[outputs.len() - 1].introduced_at.clone(),
                            message: format!(
                                "Type stack at the end of `{}` does not match signature `[{}] -> [{}]`",
                                function.name,
                                inputs
                                    .into_iter()
                                    .map(|s| s.kind.to_string())
                                    .collect::<Vec<String>>()
                                    .join(" "),
                                outputs
                                    .into_iter()
                                    .map(|s| s.kind.to_string())
                                    .collect::<Vec<String>>()
                                    .join(" "),
                            ),
                            hint: Some(format!(
                                "Type stack at the end of execution was:\n\t{}",
                                end_type_stack
                                    .into_iter()
                                    .rev()
                                    .map(|s| s.to_string())
                                    .collect::<Vec<String>>()
                                    .join("\n\t")
                            )),
                        });
                    }

                    let base_cases: Vec<BaseCase> = match self.base_cases.get(&function.name) {
                        Some(base_cases) => base_cases.to_vec(),
                        None => vec![],
                    };
                    let checked_function = CheckedFunction {
                        name: function.name.clone(),
                        ins: inputs,
                        outs: outputs,
                        body: checked_body,
                        base_cases,
                    };
                    self.functions
                        .insert(function.name.clone(), checked_function.clone());

                    return Ok(CheckedOperation {
                        kind: CheckedOpKind::FunctionDefinition {
                            function: checked_function,
                        },
                        loc: op.loc.clone(),
                        ins: vec![],
                        outs: vec![],
                    });
                } else {
                    unreachable!("Function body must be a sequence")
                }
            }
            OperationKind::FunctionCall { name } => {
                if self.functions.contains_key(name) {
                    let function = self.functions.get(name).unwrap();

                    let ins = &function.ins.clone();
                    let outs = &function.outs.clone();

                    let mut inputs = vec![];
                    //Reverse the inputs because stack I guess?
                    for input in ins.into_iter().rev() {
                        inputs.push(input.kind.clone());
                    }
                    let mut outputs = vec![];
                    for output in outs.clone() {
                        outputs.push(output.kind.clone());
                    }

                    Ok(CheckedOperation {
                        kind: CheckedOpKind::FunctionCall {
                            name: name.to_string(),
                        },
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
                    kind: CheckedOpKind::Map,
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
                    kind: CheckedOpKind::Map,
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
                    kind: CheckedOpKind::Foreach,
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
                    kind: CheckedOpKind::Map,
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
                    kind: CheckedOpKind::Concat,
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
            OperationKind::Pick => {
                let t = self.next_generic();

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Pick,
                    loc: op.loc.clone(),
                    ins: vec![
                        TypeKind::Int,
                        TypeKind::Array {
                            el_type: Box::new(t.clone()),
                        },
                    ],
                    outs: vec![
                        TypeKind::Array {
                            el_type: Box::new(t.clone()),
                        },
                        t,
                    ],
                })
            }
            OperationKind::Len => {
                let t = self.next_generic();

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Len,
                    loc: op.loc.clone(),
                    ins: vec![TypeKind::Array {
                        el_type: Box::new(t.clone()),
                    }],
                    outs: vec![
                        TypeKind::Array {
                            el_type: Box::new(t.clone()),
                        },
                        TypeKind::Int,
                    ],
                })
            }
            OperationKind::Partial => {
                //peek behind at the function before
                if self.type_stack.is_empty() {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!("Expected function but the type stack was empty",),
                        hint: None,
                    });
                }
                if let TypeKind::Function { ins, outs } =
                    self.type_stack[self.type_stack.len() - 1].kind.clone()
                {
                    if self.type_stack.len() == 1 {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc.clone(),
                            message: format!("Expected value but the type stack was empty",),
                            hint: None,
                        });
                    }
                    let value = &self.type_stack[self.type_stack.len() - 2].kind.clone();
                    if ins.len() > 0 {
                        self.types_equal(&ins[0].clone(), &value.clone(), op.loc.clone())?;
                        return Ok(CheckedOperation {
                            kind: CheckedOpKind::Len,
                            loc: op.loc.clone(),
                            ins: vec![
                                TypeKind::Function {
                                    ins: ins.to_vec(),
                                    outs: outs.to_vec(),
                                },
                                value.clone(),
                            ],
                            outs: vec![TypeKind::Function {
                                ins: ins[1..].to_vec(),
                                outs: outs.to_vec(),
                            }],
                        });
                    }
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!("Value of type `{}` is not applicable to partial application of function `{}`",
                        value, self.type_stack[self.type_stack.len() - 1].kind
                    ),
                        hint: None,
                    });
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!("Expected function but the type stack was empty",),
                        hint: None,
                    });
                }
            }
            OperationKind::If => {
                //peek behind at the function before
                if self.type_stack.is_empty() {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!("Expected function but the type stack was empty",),
                        hint: None,
                    });
                }
                if let TypeKind::Function {
                    ins: true_ins,
                    outs: true_outs,
                } = self.type_stack[self.type_stack.len() - 1].kind.clone()
                {
                    if self.type_stack.len() == 1 {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc.clone(),
                            message: format!("Expected value but the type stack was empty",),
                            hint: None,
                        });
                    }
                    if let TypeKind::Function {
                        ins: false_ins,
                        outs: false_outs,
                    } = self.type_stack[self.type_stack.len() - 2].kind.clone()
                    {
                        if true_ins.len() != false_ins.len() || true_outs.len() != false_outs.len()
                        {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                loc: op.loc.clone(),
                                message: format!(
                                    "Branches in `if` function must have the same signature.\n\ttrue:  `{}`\n\tfalse: `{}`",
                                    format!("fn [{}] -> [{}]", 
                                        true_ins.into_iter().map(|i| i.to_string()).collect::<Vec<String>>().join(", "), 
                                        true_outs.into_iter().map(|o| o.to_string()).collect::<Vec<String>>().join(", ")
                                    ),
                                    format!("fn [{}] -> [{}]", 
                                        false_ins.into_iter().map(|i| i.to_string()).collect::<Vec<String>>().join(", "), 
                                        false_outs.into_iter().map(|o| o.to_string()).collect::<Vec<String>>().join(", ")
                                    ),
                                ),
                                hint: None,
                            });
                        }
                        todo!()
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: op.loc.clone(),
                            message: format!("Expected function but the type stack was empty",),
                            hint: None,
                        });
                    }
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!("Expected function but the type stack was empty",),
                        hint: None,
                    });
                }
            }
            OperationKind::Slice => {
                let t = self.next_generic();

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Slice,
                    loc: op.loc.clone(),
                    ins: vec![
                        TypeKind::Int,
                        TypeKind::Int,
                        TypeKind::Array {
                            el_type: Box::new(t.clone()),
                        },
                    ],
                    outs: vec![TypeKind::Array {
                        el_type: Box::new(t),
                    }],
                })
            }
            OperationKind::Split => {
                let t = self.next_generic();

                Ok(CheckedOperation {
                    kind: CheckedOpKind::Split,
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
                    outs: vec![
                        TypeKind::Array {
                            el_type: Box::new(t.clone()),
                        },
                        TypeKind::Array {
                            el_type: Box::new(t),
                        },
                    ],
                })
            }
            OperationKind::Identity => {
                let t = self.next_generic();
                Ok(CheckedOperation {
                    kind: CheckedOpKind::Len,
                    loc: op.loc.clone(),
                    ins: vec![t.clone()],
                    outs: vec![t],
                })
            }
            OperationKind::Call => {
                //peek behind at the function before
                if self.type_stack.is_empty() {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!("Expected function but the type stack was empty",),
                        hint: None,
                    });
                }
                if let TypeKind::Function { mut ins, outs } = self.peek().unwrap() {
                    let mut inputs: Vec<TypeKind> = vec![];
                    inputs.push(TypeKind::Function {
                        ins: ins.clone(),
                        outs: outs.clone(),
                    });
                    inputs.append(&mut ins);
                    return Ok(CheckedOperation {
                        kind: CheckedOpKind::Call,
                        loc: op.loc.clone(),
                        ins: inputs,
                        outs,
                    });
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: op.loc.clone(),
                        message: format!(
                            "Expected function but got `{}`",
                            self.type_stack[self.type_stack.len() - 1].kind
                        ),
                        hint: Some(format!(
                            "{} introduced at {}",
                            self.type_stack[self.type_stack.len() - 1].kind,
                            self.type_stack[self.type_stack.len() - 1].introduced_at
                        )),
                    });
                }
            }
            OperationKind::FunctionBaseCaseDefinition { function } => {
                if function.ins.len() != 1 {
                    todo!("base cases with more than one input")
                }

                let mut function_type_checker = TypeChecker::new();
                function_type_checker.functions = self.functions.clone();

                let mut inputs = vec![TypeToken {
                    kind: TypeKind::Int, //TODO: other types
                    introduced_at: op.loc.clone(),
                }];
                function_type_checker.type_stack.append(&mut inputs);

                let mut outputs: Vec<TypeToken> = vec![];
                for output in &function.outs {
                    let typ = Self::get_type(output, &mut outputs)?;
                    outputs.push(typ);
                }

                if let Operand::Seq { ops } = &function.body {
                    let checked_body = function_type_checker.check_program(ops)?;

                    let end_type_stack = function_type_checker.type_stack.clone();
                    for output_type in outputs.clone().into_iter().rev() {
                        function_type_checker
                            .expect_type(&output_type.kind, output_type.introduced_at)?;
                    }

                    // if !function_type_checker.type_stack.is_empty() && !outputs.is_empty() {
                    //     return Err(Diagnostic {
                    //         severity: Severity::Error,
                    //         loc: outputs[outputs.len() - 1].introduced_at.clone(),
                    //         message: format!(
                    //             "Type stack at the end of `{}` does not match signature `[{}] -> [{}]`",
                    //             function.name,
                    //             inputs
                    //                 .into_iter()
                    //                 .map(|s| s.kind.to_string())
                    //                 .collect::<Vec<String>>()
                    //                 .join(" "),
                    //             outputs
                    //                 .into_iter()
                    //                 .map(|s| s.kind.to_string())
                    //                 .collect::<Vec<String>>()
                    //                 .join(" "),
                    //         ),
                    //         hint: Some(format!(
                    //             "Type stack at the end of execution was:\n\t{}",
                    //             end_type_stack
                    //                 .into_iter()
                    //                 .rev()
                    //                 .map(|s| s.to_string())
                    //                 .collect::<Vec<String>>()
                    //                 .join("\n\t")
                    //         )),
                    //     });
                    // }

                    if let TypeExpressionKind::Pattern { kind } = &function.ins[0].kind {
                        if let TypePatternKind::Literal { value } = &kind {
                            let base_case = BaseCase {
                                function_name: function.name.clone(),
                                input: *value,
                                body: checked_body,
                            };
                            match self.base_cases.get(&function.name) {
                                Some(base_cases) => {
                                    let mut base_cases = base_cases.clone();
                                    base_cases.push(base_case.clone());
                                    self.base_cases
                                        .insert(function.name.clone(), base_cases.clone());
                                }
                                None => {
                                    self.base_cases
                                        .insert(function.name.clone(), vec![base_case.clone()]);
                                }
                            }
                            return Ok(CheckedOperation {
                                kind: CheckedOpKind::FunctionBaseCaseDefinition { base_case },
                                loc: op.loc.clone(),
                                ins: vec![],
                                outs: vec![],
                            });
                        }
                        todo!()
                    }

                    todo!()
                } else {
                    unreachable!("Function body must be a sequence")
                }
            }
            _ => todo!("{:?}", op.kind),
        }
    }

    fn peek(&self) -> Option<TypeKind> {
        if self.type_stack.is_empty() {
            return None;
        }
        match self.type_stack[self.type_stack.len() - 1].kind {
            TypeKind::Generic { id } => {
                if self.generics.contains_key(&id) {
                    return self.generics.get(&id).clone().cloned();
                }
                return Some(self.type_stack[self.type_stack.len() - 1].kind.clone());
            }
            _ => Some(self.type_stack[self.type_stack.len() - 1].kind.clone()),
        }
    }

    fn next_generic(&mut self) -> TypeKind {
        let id = self.generic_index;
        self.generic_index += 1;
        return TypeKind::Generic { id };
    }

    fn get_type(
        type_expression: &TypeExpression,
        type_stack: &mut Vec<TypeToken>,
    ) -> Result<TypeToken, Diagnostic> {
        match &type_expression.kind {
            TypeExpressionKind::Identifier { text } => match text.as_str() {
                "int" => {
                    return Ok(TypeToken {
                        kind: TypeKind::Int,
                        introduced_at: type_expression.loc.clone(),
                    })
                }
                "bool" => {
                    return Ok(TypeToken {
                        kind: TypeKind::Bool,
                        introduced_at: type_expression.loc.clone(),
                    })
                }
                "char" => {
                    return Ok(TypeToken {
                        kind: TypeKind::Char,
                        introduced_at: type_expression.loc.clone(),
                    })
                }
                "string" => {
                    return Ok(TypeToken {
                        kind: TypeKind::Array {
                            el_type: Box::new(TypeKind::Char),
                        },
                        introduced_at: type_expression.loc.clone(),
                    })
                }
                "array" => match type_stack.pop() {
                    Some(type_token) => {
                        return Ok(TypeToken {
                            kind: TypeKind::Array {
                                el_type: Box::new(type_token.kind),
                            },
                            introduced_at: type_expression.loc.clone(),
                        });
                    }
                    None => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            loc: type_expression.loc.clone(),
                            message: format!("Expected type but type stack was empty"),
                            hint: Some("Declare array types as `<element type> array`".to_owned()),
                        })
                    }
                },
                _ => {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        loc: type_expression.loc.clone(),
                        message: format!("Type `{:?}` is not defined", text),
                        hint: None,
                    })
                }
            },
            TypeExpressionKind::Function { ins, outs } => {
                let mut in_types: Vec<TypeKind> = vec![];
                let mut out_types: Vec<TypeKind> = vec![];

                for in_type in ins.into_iter().rev() {
                    in_types.push(Self::get_type(in_type, type_stack)?.kind);
                }

                for out_type in outs {
                    out_types.push(Self::get_type(out_type, type_stack)?.kind);
                }

                return Ok(TypeToken {
                    kind: TypeKind::Function {
                        ins: in_types,
                        outs: out_types,
                    },
                    introduced_at: type_expression.loc.clone(),
                });
            }
            _ => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    loc: type_expression.loc.clone(),
                    message: format!("Unexpected type expression: `{:?}`", type_expression),
                    hint: None,
                });
            }
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
                let mut sub_type_checker = TypeChecker::new();
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
