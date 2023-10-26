use std::{
    collections::HashMap,
    fmt,
    ops::{Add, BitAnd, BitOr, Div, Mul, Not, Rem, Sub},
    vec,
};

use crate::parser::{Operand, Operation, OperationKind};

#[derive(Clone, Debug)]
pub enum Data {
    Bool { value: bool },
    Int { value: usize },
    Char { value: char },
    Array { elements: Vec<Data> },
    Seq { ops: Vec<Operation> },
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Data::Int { value } => return write!(f, "{}", value),
            Data::Bool { value } => return write!(f, "{}", value),
            Data::Char { value } => return write!(f, "'{}", value),
            Data::Array { elements } => {
                if !elements.is_empty() {
                    if let Data::Char { .. } = elements[0] {
                        let string = elements
                            .into_iter()
                            .map(|c| match c {
                                Data::Char { value } => value,
                                _ => unreachable!(),
                            })
                            .collect::<String>();
                        return write!(f, "{}", string);
                    } else {
                        return write!(
                            f,
                            "[{}]",
                            elements
                                .into_iter()
                                .map(|v| v.to_string())
                                .collect::<Vec<String>>()
                                .join(", ")
                        );
                    }
                }
                return write!(
                    f,
                    "[{}]",
                    elements
                        .into_iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                );
            }
            Data::Seq { .. } => return write!(f, "<Seq>"),
        }
    }
}

impl Add for Data {
    type Output = Data;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Data::Int { value: a } => match rhs {
                Data::Int { value: b } => return Data::Int { value: a + b },
                Data::Bool { .. } | Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                    unreachable!()
                }
            },
            Data::Bool { .. } => unreachable!(),

            Data::Array { elements } => {
                let left_els = elements;
                match rhs {
                    Data::Array { elements } => {
                        return Data::Array {
                            elements: [left_els, elements].concat(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Data::Char { .. } | Data::Seq { .. } => unreachable!(),
        }
    }
}

impl Sub for Data {
    type Output = Data;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Data::Int { value: a } => match rhs {
                Data::Int { value: b } => return Data::Int { value: a - b },
                Data::Bool { .. } | Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                    unreachable!()
                }
            },
            Data::Bool { .. } | Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                unreachable!()
            }
        }
    }
}

impl Mul for Data {
    type Output = Data;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Data::Int { value: a } => match rhs {
                Data::Int { value: b } => return Data::Int { value: a * b },
                Data::Bool { .. } | Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                    unreachable!()
                }
            },
            Data::Bool { .. } | Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                unreachable!()
            }
        }
    }
}

impl Div for Data {
    type Output = Data;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Data::Int { value: a } => match rhs {
                Data::Int { value: b } => return Data::Int { value: a / b },
                Data::Bool { .. } | Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                    unreachable!()
                }
            },
            Data::Bool { .. } | Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                unreachable!()
            }
        }
    }
}

impl Rem for Data {
    type Output = Data;

    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Data::Int { value: a } => match rhs {
                Data::Int { value: b } => return Data::Int { value: a % b },
                Data::Bool { .. } => unreachable!(),
                Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                    unreachable!()
                }
            },
            Data::Bool { .. } | Data::Array { .. } | Data::Seq { .. } | Data::Char { .. } => {
                unreachable!()
            }
        }
    }
}

impl BitAnd for Data {
    type Output = Data;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Bool { value: l_value }, Self::Bool { value: r_value }) => Data::Bool {
                value: l_value && r_value,
            },
            (Self::Int { value: l_value }, Self::Int { value: r_value }) => Data::Int {
                value: l_value & r_value,
            },
            _ => unreachable!(),
        }
    }
}

impl BitOr for Data {
    type Output = Data;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Bool { value: l_value }, Self::Bool { value: r_value }) => Data::Bool {
                value: l_value || r_value,
            },
            (Self::Int { value: l_value }, Self::Int { value: r_value }) => Data::Int {
                value: l_value | r_value,
            },
            _ => unreachable!(),
        }
    }
}

impl Not for Data {
    type Output = Data;

    fn not(self) -> Self::Output {
        match self {
            Data::Bool { value } => Data::Bool { value: !value },
            Data::Int { value } => Data::Int { value: !value },
            _ => unreachable!(),
        }
    }
}

impl PartialEq for Data {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool { value: l_value }, Self::Bool { value: r_value }) => l_value == r_value,
            (Self::Int { value: l_value }, Self::Int { value: r_value }) => l_value == r_value,
            (Self::Char { value: l_value }, Self::Char { value: r_value }) => l_value == r_value,
            _ => unreachable!("{:?}, {:?}", self, other),
        }
    }
}

impl PartialOrd for Data {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Int { value: l_value }, Self::Int { value: r_value }) => {
                l_value.partial_cmp(r_value)
            }
            (Self::Char { value: l_value }, Self::Char { value: r_value }) => {
                l_value.partial_cmp(r_value)
            }
            (Self::Char { value: l_value }, Self::Int { value: r_value }) => {
                l_value.partial_cmp(&(*r_value as u8 as char))
            }
            (Self::Int { value: l_value }, Self::Char { value: r_value }) => {
                l_value.partial_cmp(&(*r_value as usize))
            }
            _ => unreachable!("{:?}, {:?}", self, other),
        }
    }
}

pub struct Interpreter {
    stack: Vec<Data>,
    functions: HashMap<String, Vec<Operation>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        return Interpreter {
            stack: vec![],
            functions: HashMap::new(),
        };
    }

    pub fn interpret_operand(&self, operand: &Operand) -> Data {
        match operand {
            Operand::Bool { value } => Data::Bool { value: *value },
            Operand::Int { value } => Data::Int { value: *value },
            Operand::Char { value } => Data::Char { value: *value },
            Operand::Array { values } => Data::Array {
                elements: values
                    .into_iter()
                    .map(|v| self.interpret_operand(v))
                    .collect(),
            },
            Operand::Seq { ops } => Data::Seq { ops: ops.clone() },
            Operand::String { value } => Data::Array {
                elements: value
                    .chars()
                    .map(|c| Data::Char { value: c })
                    .collect::<Vec<Data>>(),
            },
        }
    }

    pub fn interpret(&mut self, op: &Operation) {
        match &op.kind {
            OperationKind::Push { operand } => self.stack.push(self.interpret_operand(operand)),
            OperationKind::Add => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(a + b);
            }
            OperationKind::Sub => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(a - b);
            }
            OperationKind::Mul => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(a * b);
            }
            OperationKind::Div => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(a / b);
            }
            OperationKind::Mod => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(a % b);
            }
            OperationKind::Lt => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(Data::Bool { value: a < b });
            }
            OperationKind::Gt => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(Data::Bool { value: a > b });
            }
            OperationKind::LtEq => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(Data::Bool { value: a <= b });
            }
            OperationKind::GtEq => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(Data::Bool { value: a >= b });
            }
            OperationKind::LogicalAnd => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(a & b);
            }
            OperationKind::LogicalOr => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(a | b);
            }
            OperationKind::LogicalNot => {
                let v = self.stack.pop().unwrap();

                self.stack.push(!v);
            }
            OperationKind::Print => {
                let v = self.stack.pop().unwrap();
                println!("{}", v);
            }
            OperationKind::Pop => {
                self.stack.pop();
            }
            OperationKind::Swap => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();

                self.stack.push(a);
                self.stack.push(b);
            }
            OperationKind::Dup => {
                let a = self.stack.pop().unwrap();

                self.stack.push(a.clone());
                self.stack.push(a);
            }
            OperationKind::Over => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();

                self.stack.push(a.clone());
                self.stack.push(b);
                self.stack.push(a);
            }
            OperationKind::Rot => {
                let c = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(b);
                self.stack.push(c);
                self.stack.push(a);
            }
            OperationKind::Eq => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(Data::Bool { value: a == b });
            }
            OperationKind::Cons => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(Data::Array {
                    elements: vec![a, b],
                });
            }
            OperationKind::Len => {
                let array = self.stack.pop().unwrap();
                if let Data::Array { elements } = array {
                    self.stack.push(Data::Int {
                        value: elements.len(),
                    })
                } else {
                    panic!("Expected array but got {}", array);
                }
            }
            OperationKind::Concat => {
                let b = self.stack.pop().unwrap();

                let a = self.stack.pop().unwrap();

                self.stack.push(a + b);
            }
            OperationKind::Map => {
                let seq = self.stack.pop().unwrap();

                if let Data::Seq { ops } = seq {
                    let array = self.stack.pop().unwrap();
                    if let Data::Array { elements } = array {
                        let mut sub_interpreter = Interpreter {
                            stack: vec![],
                            functions: self.functions.clone(),
                        };
                        for el in elements {
                            sub_interpreter.stack.push(el);
                            for op in &ops {
                                sub_interpreter.interpret(op);
                            }
                        }
                        self.stack.push(Data::Array {
                            elements: sub_interpreter.stack,
                        });
                    } else {
                        panic!("Expected array but got {}", array);
                    }
                } else {
                    panic!("Expected seq but got {}", seq);
                }
            }
            OperationKind::Filter => {
                let seq = self.stack.pop().unwrap();

                if let Data::Seq { ops } = seq {
                    let array = self.stack.pop().unwrap();
                    if let Data::Array { elements } = array {
                        let mut sub_interpreter = Interpreter {
                            stack: vec![],
                            functions: self.functions.clone(),
                        };
                        for el in elements {
                            sub_interpreter.stack.push(el.clone());
                            sub_interpreter.stack.push(el);
                            for op in &ops {
                                sub_interpreter.interpret(op);
                            }
                            let res = sub_interpreter.stack.pop().unwrap();
                            if let Data::Bool { value } = res {
                                if !value {
                                    sub_interpreter.stack.pop();
                                }
                            } else {
                                panic!("Expected bool but got {}", res);
                            }
                        }
                        self.stack.push(Data::Array {
                            elements: sub_interpreter.stack,
                        });
                    } else {
                        panic!("Expected array but got {}", array);
                    }
                } else {
                    panic!("Expected seq but got {}", seq);
                }
            }
            OperationKind::Fold => {
                let seq = self.stack.pop().unwrap();

                if let Data::Seq { ops } = seq {
                    let mut acc = self.stack.pop().unwrap();
                    let array = self.stack.pop().unwrap();
                    if let Data::Array { elements } = array {
                        let mut sub_interpreter = Interpreter {
                            stack: vec![],
                            functions: self.functions.clone(),
                        };
                        for el in elements {
                            sub_interpreter.stack.push(acc.clone());
                            sub_interpreter.stack.push(el);
                            for op in &ops {
                                sub_interpreter.interpret(op);
                            }
                            acc = sub_interpreter.stack.pop().unwrap();
                        }
                        self.stack.push(acc);
                    } else {
                        panic!("Expected array but got {}", array);
                    }
                } else {
                    panic!("Expected seq but got {}", seq);
                }
            }
            OperationKind::Foreach => {
                let seq = self.stack.pop().unwrap();

                if let Data::Seq { ops } = seq {
                    let array = self.stack.pop().unwrap();
                    if let Data::Array { elements } = array {
                        for el in elements {
                            self.stack.push(el);
                            for op in &ops {
                                self.interpret(op);
                            }
                        }
                    } else {
                        panic!("Expected array but got {}", array);
                    }
                } else {
                    panic!("Expected seq but got {}", seq);
                }
            }
            OperationKind::Call => {
                let seq = self.stack.pop().unwrap();

                if let Data::Seq { ops } = seq {
                    for op in &ops {
                        self.interpret(op);
                    }
                } else {
                    panic!("Expected seq but got {}", seq);
                }
            }
            OperationKind::Identity => {
                let data = self.stack.pop().unwrap();
                self.stack.push(data);
            }
            OperationKind::FunctionDefinition { function } => {
                let seq = self.interpret_operand(&function.body);
                if let Data::Seq { ops } = seq {
                    self.functions.insert(function.name.clone(), ops);
                } else {
                    panic!("Expected seq but got {}", seq);
                }
            }
            OperationKind::FunctionCall { name } => {
                let ops = self
                    .functions
                    .get(name)
                    .expect(&format!("function `{}` went missing", name));

                for op in ops.clone() {
                    self.interpret(&op);
                }
            }
            OperationKind::Zip => {
                let left = self.stack.pop().unwrap();
                if let Data::Array { elements } = left {
                    let left_elements = elements;
                    let right = self.stack.pop().unwrap();
                    if let Data::Array { elements } = right {
                        if left_elements.len() != elements.len() {
                            panic!("elements len mismatch");
                        }

                        let mut array: Vec<Data> = vec![];
                        for (i, left_el) in left_elements.into_iter().enumerate() {
                            array.push(Data::Array {
                                elements: vec![elements[i].clone(), left_el],
                            });
                        }
                        self.stack.push(Data::Array { elements: array });
                    } else {
                        panic!("Expected array but got {}", right);
                    }
                } else {
                    panic!("Expected array but got {}", left);
                }
            }
            OperationKind::Pick => {
                let index = self.stack.pop().unwrap();
                if let Data::Int { value } = index {
                    let array = self.stack.pop().unwrap();
                    if let Data::Array { elements } = array.clone() {
                        self.stack.push(array);
                        self.stack.push(elements[value].clone());
                    } else {
                        panic!("Expected array but got {}", array);
                    }
                } else {
                    panic!("Expected int but got {}", index);
                }
            }
            OperationKind::Slice => {
                let end = self.stack.pop().unwrap();
                if let Data::Int { value: end_val } = end {
                    let start = self.stack.pop().unwrap();
                    if let Data::Int { value: start_val } = start {
                        let array = self.stack.pop().unwrap();
                        if let Data::Array { elements } = array.clone() {
                            self.stack.push(Data::Array {
                                elements: elements[start_val..end_val].to_vec(),
                            })
                        } else {
                            panic!("Expected array but got {}", array);
                        }
                    } else {
                        panic!("Expected int but got {}", start);
                    }
                } else {
                    panic!("Expected int but got {}", end);
                }
            }
            OperationKind::Split => {
                let seq = self.stack.pop().unwrap();

                if let Data::Seq { ops } = seq {
                    let array = self.stack.pop().unwrap();
                    if let Data::Array { elements } = array {
                        let mut sub_interpreter = Interpreter {
                            stack: vec![],
                            functions: self.functions.clone(),
                        };
                        let mut false_list: Vec<Data> = vec![];
                        for el in elements {
                            sub_interpreter.stack.push(el.clone());
                            sub_interpreter.stack.push(el);
                            for op in &ops {
                                sub_interpreter.interpret(op);
                            }
                            let res = sub_interpreter.stack.pop().unwrap();
                            if let Data::Bool { value } = res {
                                if !value {
                                    false_list.push(sub_interpreter.stack.pop().unwrap());
                                }
                            } else {
                                panic!("Expected bool but got {}", res);
                            }
                        }
                        self.stack.push(Data::Array {
                            elements: false_list,
                        });
                        self.stack.push(Data::Array {
                            elements: sub_interpreter.stack,
                        });
                    } else {
                        panic!("Expected array but got {}", array);
                    }
                } else {
                    panic!("Expected seq but got {}", seq);
                }
            }
            OperationKind::If => {
                let false_branch = self.stack.pop().unwrap();

                if let Data::Seq { ops: false_ops } = false_branch {
                    let true_branch = self.stack.pop().unwrap();

                    if let Data::Seq { ops: true_ops } = true_branch {
                        if let Data::Bool { value: condition } = self.stack.pop().unwrap() {
                            if condition {
                                for op in true_ops {
                                    self.interpret(&op);
                                }
                            } else {
                                for op in false_ops {
                                    self.interpret(&op);
                                }
                            }
                        }
                    } else {
                        panic!("Expected seq but got {}", true_branch);
                    }
                } else {
                    panic!("Expected seq but got {}", false_branch);
                }
            }

            OperationKind::Return | OperationKind::Args => {
                todo!();
            }
        }
    }
}

//compiling
#[derive(Debug, Clone, PartialEq)]
enum TypeTag {
    Bool,
    Char,
    Int,
    ArrayBegin,
}
#[derive(Debug, Clone, PartialEq)]
struct StackValue {
    value: usize,
    type_tag: TypeTag,
}

impl StackValue {
    fn of_int(value: usize) -> StackValue {
        return StackValue {
            value: value,
            type_tag: TypeTag::Int,
        };
    }

    fn of_bool(value: bool) -> StackValue {
        return StackValue {
            value: if value { 1 } else { 0 },
            type_tag: TypeTag::Int,
        };
    }
}

impl fmt::Display for StackValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.type_tag {
            TypeTag::Bool => {
                return write!(f, "{}", if self.value == 0 { "false" } else { "true" })
            }
            TypeTag::Int => return write!(f, "{}", self.value),
            TypeTag::Char => {
                return write!(
                    f,
                    "'{}",
                    char::from_u32(self.value.try_into().unwrap()).unwrap()
                )
            }
            _ => todo!(),
        }
    }
}
