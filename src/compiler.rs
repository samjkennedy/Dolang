use crate::{
    parser::Operand,
    type_checker::{CheckedFunction, CheckedOpKind, CheckedOperand, CheckedOperation},
};
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Write},
    vec,
};

pub struct Compiler {
    pub out_file: File,
    next_lambda_label: usize,
    lambdas: HashMap<CheckedOperand, String>,
}

impl Compiler {
    pub fn new(out_file: File) -> Compiler {
        return Compiler {
            out_file,
            next_lambda_label: 0,
            lambdas: HashMap::new(),
        };
    }

    pub fn compile(&mut self, ops: &Vec<CheckedOperation>) -> io::Result<()> {
        //Preamble
        self.out_file.write(b"#include <stdio.h>\n").unwrap();
        self.out_file.write(b"#include <stdint.h>\n").unwrap();
        self.out_file.write(b"#include <stdlib.h>\n").unwrap();
        self.out_file.write(b"#include <stdbool.h>\n").unwrap();
        self.out_file.write(b"#include <string.h>\n").unwrap();
        self.out_file.write(b"\n").unwrap();

        //Define types
        self.out_file.write(b"typedef struct {\n")?;
        self.out_file.write(b"  int typeTag;\n")?;
        self.out_file.write(b"  uint64_t value;\n")?;
        self.out_file.write(b"} Value;\n\n")?;
        self.out_file.write(b"typedef struct {\n")?;
        self.out_file.write(b"  int offset;\n")?;
        self.out_file.write(b"  int length;\n")?;
        self.out_file.write(b"  Value *data;\n")?;
        self.out_file.write(b"} Array;\n\n")?;

        self.out_file.write(b"#define STACK_CAPACITY 0x8000\n")?;
        self.out_file.write(b"Value stack[STACK_CAPACITY];\n")?;
        self.out_file.write(b"int sp = 0;\n")?;

        //Define intrinsics
        //TODO: Bounds checking
        //      Though the type checker should ensure we never go out of bounds negatively
        //push
        self.out_file.write(b"void push(Value operand) {\n")?;
        self.out_file.write(b"    stack[sp] = operand;\n")?;
        self.out_file.write(b"    sp += 1;\n")?;
        self.out_file.write(b"}\n")?;
        //pop
        self.out_file.write(b"Value pop() {\n")?;
        self.out_file.write(b"    sp -= 1;\n")?;
        self.out_file.write(b"    return stack[sp];\n")?;
        self.out_file.write(b"}\n")?;
        //dup
        self.out_file.write(
            b"void dup()
{
    Value a = pop();
    switch (a.typeTag)
    {
    case 3:
    {
        Array *originalArray = (Array *)a.value;
        int length = originalArray->length;

        // Allocate memory for the new array
        Array *duplicateArray = (Array *)malloc(sizeof(Array));
        duplicateArray->offset = originalArray->offset;
        duplicateArray->length = length;
        duplicateArray->data = (Value *)malloc(length * sizeof(Value));

        // Copy the elements from the original array to the duplicate array
        memcpy(duplicateArray->data, originalArray->data, length * sizeof(Value));

        push((Value){3, (int)originalArray});
        push((Value){3, (int)duplicateArray});
    }
    break;
    default:
    {
        Value b = a;
        push(a);
        push(b);
    }
    }
}",
        )?;
        //printArray
        self.out_file.write(b"    void printArray(Value v) {\n")?;
        self.out_file
            .write(b"Array arrayValue = *(Array *)v.value;\n")?;
        self.out_file
            .write(b"Value *arrayData = arrayValue.data;\n")?;
        self.out_file.write(b"printf(\"%s\", \"[\");\n")?;
        self.out_file
            .write(b"for (int i = arrayValue.offset; i < arrayValue.length; i++) {\n")?;
        self.out_file.write(b"switch (arrayData[i].typeTag) {\n")?;

        self.out_file.write(b"case 0:\n")?;
        self.out_file
            .write(b"printf(\"%s\", arrayData[i].value == 0 ? \"false\" : \"true\");\n")?;
        self.out_file.write(b"break;\n")?;
        self.out_file.write(b"case 1:\n")?;
        self.out_file
            .write(b"printf(\"%d\", arrayData[i].value);\n")?;
        self.out_file.write(b"break;\n")?;
        self.out_file.write(b"case 2:\n")?;
        self.out_file
            .write(b"printf(\"%c\", arrayData[i].value);\n")?;
        self.out_file.write(b"break;\n")?;
        self.out_file.write(b"case 3:\n")?;
        self.out_file.write(b"printArray(arrayData[i]);\n")?;
        self.out_file.write(b"break;\n")?;
        self.out_file.write(b"case 5:\n")?;
        self.out_file
            .write(b"printf(\"%s\", arrayData[i].value);\n")?;
        self.out_file.write(b"break;\n")?;
        self.out_file.write(b"default:\n")?;
        self.out_file.write(
            b"      printf(\"%s: %d\\n\", \"unhandled datatype\", arrayData[i].typeTag);\n",
        )?;
        self.out_file.write(b"break;\n")?;
        self.out_file.write(b"    }\n")?;

        self.out_file.write(b"if (i < arrayValue.length - 1) {\n")?;
        self.out_file.write(b"printf(\"%s\", \" \");\n")?;
        self.out_file.write(b"}\n")?;
        self.out_file.write(b"}\n")?;
        self.out_file.write(b"printf(\"%s\", \"]\");\n")?;
        self.out_file.write(b"free(arrayData);\n")?;
        self.out_file.write(b"free((Array *)v.value);\n")?;
        self.out_file.write(b"    }\n")?;

        //print
        self.out_file.write(b"    void print() {\n")?;
        self.out_file.write(b"    Value v = pop();\n")?;
        self.out_file.write(b"    switch (v.typeTag) {\n")?;
        self.out_file.write(b"    case 0:\n")?;
        self.out_file
            .write(b"      printf(\"%s\\n\", v.value == 0 ? \"false\" : \"true\");\n")?;
        self.out_file.write(b"      break;\n")?;
        self.out_file.write(b"    case 1:\n")?;
        self.out_file
            .write(b"      printf(\"%d\\n\", v.value);\n")?;
        self.out_file.write(b"      break;\n")?;
        self.out_file.write(b"    case 3:\n")?;
        self.out_file.write(b"      printArray(v);\n")?;
        self.out_file.write(b"      printf(\"\\n\");\n")?;
        self.out_file.write(b"      break;\n")?;
        self.out_file.write(b"    case 4:\n")?;
        self.out_file.write(b"      printf(\"<seq>\\n\");\n")?;
        self.out_file.write(b"      break;\n")?;
        self.out_file.write(b"    case 5:\n")?;
        self.out_file
            .write(b"      printf(\"%s\\n\", v.value);\n")?;
        self.out_file.write(b"      break;\n")?;
        self.out_file.write(b"    default:\n")?;
        self.out_file
            .write(b"      printf(\"%s: %d\\n\", \"unhandled datatype\", v.typeTag);\n")?;
        self.out_file.write(b"      }\n")?;
        self.out_file.write(b"}\n")?;
        self.out_file.write(b"\n")?;

        //map
        self.out_file.write(b"void map() {\n")?;
        self.out_file.write(b"Value lambda = pop();\n")?;
        self.out_file.write(b"Value array = pop();\n")?;
        self.out_file
            .write(b"Array arrayValue = *(Array *)array.value;\n")?;
        self.out_file
            .write(b"Value *arrayData = arrayValue.data;\n")?;
        self.out_file
            .write(b"for (int i = arrayValue.offset; i < arrayValue.length; i++) {\n")?;
        self.out_file.write(b"push(arrayData[i]);\n")?;
        self.out_file
            .write(b"(((void (*)(void))lambda.value))();\n")?;
        self.out_file.write(b"arrayData[i] = pop();\n")?;
        self.out_file.write(b"}\n")?;
        self.out_file.write(b"push(array);\n")?;
        self.out_file.write(b"}\n")?;
        //filter
        self.out_file.write(b"void filter()\n")?;
        self.out_file.write(b"{\n")?;
        self.out_file.write(b"    Value lambda = pop();\n")?;
        self.out_file.write(b"    Value array = pop();\n")?;
        self.out_file
            .write(b"    Array arrayValue = *(Array *)array.value;\n")?;
        self.out_file
            .write(b"    Value *arrayData = arrayValue.data;\n")?;
        self.out_file
            .write(b"    Array *resultArray = malloc(sizeof(Array));\n")?;
        self.out_file.write(b"    resultArray->offset = 0;\n")?;
        self.out_file.write(b"    resultArray->length = 0;\n")?;
        self.out_file
            .write(b"    resultArray->data = malloc(arrayValue.length * sizeof(Value));\n")?;
        self.out_file
            .write(b"    for (int i = arrayValue.offset; i < arrayValue.length; i++)\n")?;
        self.out_file.write(b"    {\n")?;
        self.out_file.write(b"        push(arrayData[i]);\n")?;
        self.out_file
            .write(b"        (((void (*)(void))lambda.value))();\n")?;
        self.out_file
            .write(b"        Value predicateResult = pop();\n")?;
        self.out_file
            .write(b"        if (predicateResult.value)\n")?;
        self.out_file.write(b"        {\n")?;
        self.out_file
            .write(b"            resultArray->data[resultArray->length] = arrayData[i];\n")?;
        self.out_file
            .write(b"            resultArray->length++;\n")?;
        self.out_file.write(b"        }\n")?;
        self.out_file.write(b"    }\n")?;
        self.out_file
            .write(b"    Value *newData = malloc(resultArray->length * sizeof(Value));\n")?;
        self.out_file.write(
            b"    memcpy(newData, resultArray->data, resultArray->length * sizeof(Value));\n",
        )?;
        self.out_file.write(b"    free(arrayValue.data);\n")?;
        self.out_file.write(b"    resultArray->data = newData;\n")?;
        self.out_file
            .write(b"    push((Value){3, (int)resultArray});\n")?;
        self.out_file.write(b"}\n")?;

        //split
        self.out_file.write(b"void split()\n")?;
        self.out_file.write(b"{\n")?;
        self.out_file.write(b"    Value lambda = pop();\n")?;
        self.out_file.write(b"    Value array = pop();\n")?;
        self.out_file
            .write(b"    Array arrayValue = *(Array *)array.value;\n")?;
        self.out_file
            .write(b"    Value *arrayData = arrayValue.data;\n")?;
        self.out_file
            .write(b"    Array *trueArray = malloc(sizeof(Array));\n")?;
        self.out_file.write(b"    trueArray->offset = 0;\n")?;
        self.out_file.write(b"    trueArray->length = 0;\n")?;
        self.out_file
            .write(b"    trueArray->data = malloc(arrayValue.length * sizeof(Value));\n")?;
        self.out_file
            .write(b"    Array *falseArray = malloc(sizeof(Array));\n")?;
        self.out_file.write(b"    falseArray->offset = 0;\n")?;
        self.out_file.write(b"    falseArray->length = 0;\n")?;
        self.out_file
            .write(b"    falseArray->data = malloc(arrayValue.length * sizeof(Value));\n")?;
        self.out_file
            .write(b"    for (int i = arrayValue.offset; i < arrayValue.length; i++)\n")?;
        self.out_file.write(b"    {\n")?;
        self.out_file.write(b"        push(arrayData[i]);\n")?;
        self.out_file
            .write(b"        (((void (*)(void))lambda.value))();\n")?;
        self.out_file
            .write(b"        Value predicateResult = pop();\n")?;
        self.out_file
            .write(b"        if (predicateResult.value)\n")?;
        self.out_file.write(b"        {\n")?;
        self.out_file
            .write(b"            trueArray->data[trueArray->length] = arrayData[i];\n")?;
        self.out_file.write(b"            trueArray->length++;\n")?;
        self.out_file.write(b"        }\n")?;
        self.out_file.write(b"        else\n")?;
        self.out_file.write(b"        {\n")?;
        self.out_file
            .write(b"            falseArray->data[falseArray->length] = arrayData[i];\n")?;
        self.out_file
            .write(b"            falseArray->length++;\n")?;
        self.out_file.write(b"        }\n")?;
        self.out_file.write(b"    }\n")?;
        self.out_file
            .write(b"    Value *newtrueData = malloc(trueArray->length * sizeof(Value));\n")?;
        self.out_file.write(
            b"    memcpy(newtrueData, trueArray->data, trueArray->length * sizeof(Value));\n",
        )?;
        self.out_file
            .write(b"    Value *newfalseData = malloc(falseArray->length * sizeof(Value));\n")?;
        self.out_file.write(
            b"    memcpy(newfalseData, falseArray->data, falseArray->length * sizeof(Value));\n",
        )?;
        self.out_file.write(b"    free(arrayValue.data);\n")?;
        self.out_file
            .write(b"    trueArray->data = newtrueData;\n")?;
        self.out_file
            .write(b"    falseArray->data = newfalseData;\n")?;
        self.out_file
            .write(b"    push((Value){3, (int)falseArray});\n")?;
        self.out_file
            .write(b"    push((Value){3, (int)trueArray});\n")?;
        self.out_file.write(b"}\n")?;

        //foreach
        self.out_file.write(b"void foreach() {\n")?;
        self.out_file.write(b"Value lambda = pop();\n")?;
        self.out_file.write(b"Value array = pop();\n")?;
        self.out_file
            .write(b"Array arrayValue = *(Array *)array.value;\n")?;
        self.out_file
            .write(b"Value *arrayData = arrayValue.data;\n")?;
        self.out_file
            .write(b"for (int i = arrayValue.offset; i < arrayValue.length; i++) {\n")?;
        self.out_file.write(b"push(arrayData[i]);\n")?;
        self.out_file
            .write(b"(((void (*)(void))lambda.value))();\n")?;
        self.out_file.write(b"}\n")?;
        self.out_file.write(b"free(arrayData);\n")?;
        self.out_file.write(b"free(array.value);\n")?;
        self.out_file.write(b"}\n")?;

        //fold
        self.out_file.write(b"void fold()\n")?;
        self.out_file.write(b"{\n")?;
        self.out_file.write(b"    Value lambda = pop();\n")?;
        self.out_file
            .write(b"    Value initialAccumulator = pop();\n")?;
        self.out_file.write(b"    Value array = pop();\n")?;
        self.out_file
            .write(b"    Array arrayValue = *(Array *)array.value;\n")?;
        self.out_file
            .write(b"    Value *arrayData = arrayValue.data;\n")?;
        self.out_file
            .write(b"    Value accumulator = initialAccumulator;\n")?;
        self.out_file
            .write(b"    for (int i = arrayValue.offset; i < arrayValue.length; i++)\n")?;
        self.out_file.write(b"    {\n")?;
        self.out_file.write(b"        push(accumulator);\n")?;
        self.out_file.write(b"        push(arrayData[i]);\n")?;
        self.out_file
            .write(b"        (((void (*)(void))lambda.value))();\n")?;
        self.out_file.write(b"        accumulator = pop();\n")?;
        self.out_file.write(b"    }\n")?;
        self.out_file.write(b"    push(accumulator);\n")?;
        self.out_file.write(b"}\n")?;

        //concat
        self.out_file.write(b"void concat()\n")?;
        self.out_file.write(b"{\n")?;
        self.out_file.write(b"    Value array2 = pop();\n")?;
        self.out_file.write(b"    Value array1 = pop();\n")?;
        self.out_file
            .write(b"    Array array1Value = *(Array *)array1.value;\n")?;
        self.out_file
            .write(b"    Array array2Value = *(Array *)array2.value;\n")?;
        self.out_file
            .write(b"    int totalLength = array1Value.length + array2Value.length;\n")?;
        self.out_file
            .write(b"    Array *resultArray = malloc(sizeof(Array));\n")?;
        self.out_file.write(b"    resultArray->offset = 0;\n")?;
        self.out_file
            .write(b"    resultArray->length = totalLength;\n")?;
        self.out_file
            .write(b"    resultArray->data = malloc(totalLength * sizeof(Value));\n")?;
        self.out_file.write(b"    memcpy(resultArray->data, array1Value.data, array1Value.length * sizeof(Value));\n")?;
        self.out_file.write(b"    memcpy(resultArray->data + array1Value.length, array2Value.data, array2Value.length * sizeof(Value));\n")?;
        self.out_file
            .write(b"    push((Value){3, (int)resultArray});\n")?;
        self.out_file.write(b"}\n")?;

        //if
        self.out_file.write(b"void _if() {\n")?;
        self.out_file.write(b"Value false_branch = pop();\n")?;
        self.out_file.write(b"Value true_branch = pop();\n")?;
        self.out_file.write(b"Value condition = pop();\n")?;
        self.out_file.write(b"if (condition.value) {\n")?;
        self.out_file
            .write(b"(((void (*)(void))true_branch.value))();\n")?;
        self.out_file.write(b"}\n")?;
        self.out_file.write(b"else {\n")?;
        self.out_file
            .write(b"(((void (*)(void))false_branch.value))();\n")?;
        self.out_file.write(b"}\n")?;
        self.out_file.write(b"}\n")?;

        //partial
        self.out_file.write(
            b"void partial() {
    Value lambda = pop();
    Value arg = pop();
    push(lambda);
        }\n",
        )?;

        for op in ops {
            self.emit(&op.kind)?;
        }

        Ok(())
    }

    fn emit(&mut self, op_kind: &CheckedOpKind) -> io::Result<()> {
        match op_kind {
            CheckedOpKind::Push { operand } => self.emit_push_operand(operand)?,
            CheckedOpKind::Pop => {
                self.out_file.write(b"pop();\n")?;
            }
            CheckedOpKind::Dup => {
                self.out_file.write(b"dup();\n")?;
            }
            CheckedOpKind::Rot => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value c = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        push(b);\n")?;
                self.out_file.write(b"        push(c);\n")?;
                self.out_file.write(b"        push(a);\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Swap => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file.write(b"        push(a);\n")?;
                self.out_file.write(b"        push(b);\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Over => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file.write(b"        Value _a = a;\n")?;
                self.out_file.write(b"        push(a);\n")?;
                self.out_file.write(b"        push(b);\n")?;
                self.out_file.write(b"        push(_a);\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Add => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 1, b.value + a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Sub => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 1, b.value - a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Mul => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value) { 1, b.value * a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Div => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 1, b.value / a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Mod => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 1, b.value % a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Eq => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 0, b.value == a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Lt => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 0, b.value < a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::LtEq => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 0, b.value <= a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Gt => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 0, b.value > a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::GtEq => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 0, b.value >= a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::LogicalAnd => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 0, b.value & a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::LogicalOr => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file.write(b"        Value b = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 0, b.value | a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::LogicalNot => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"        Value a = pop();\n")?;
                self.out_file
                    .write(b"        push((Value)  { 0, !a.value });\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Print => {
                self.out_file.write(b"    print();\n")?;
            }
            CheckedOpKind::FunctionDefinition { function } => {
                self.emit_function_definition(function)?
            }
            CheckedOpKind::FunctionCall { name } => {
                write!(self.out_file, "    {}();\n", name)?;
            }
            CheckedOpKind::Map => {
                self.out_file.write(b"    map();\n")?;
            }
            CheckedOpKind::Filter => {
                self.out_file.write(b"    filter();\n")?;
            }
            CheckedOpKind::Foreach => {
                self.out_file.write(b"    foreach();\n")?;
            }
            CheckedOpKind::Fold => {
                self.out_file.write(b"    fold();\n")?;
            }
            CheckedOpKind::Split => {
                self.out_file.write(b"    split();\n")?;
            }
            CheckedOpKind::Concat => {
                self.out_file.write(b"    concat();\n")?;
            }
            CheckedOpKind::Partial => {
                self.out_file.write(b"    partial();\n")?;
            }
            CheckedOpKind::Call => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"    Value lambda = pop();\n")?;
                self.out_file
                    .write(b"        (((void (*)(void))lambda.value))();\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Len => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"    Value array = pop();\n")?;
                self.out_file
                    .write(b"    Array arrayValue = *(Array *)array.value;\n")?;
                self.out_file.write(b"        push(array);\n")?;
                self.out_file.write(
                    b"        push((Value)  { 1, arrayValue.length - arrayValue.offset });\n",
                )?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Pick => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"    Value index = pop();\n")?;
                self.out_file.write(b"    Value array = pop();\n")?;
                self.out_file
                    .write(b"    Array arrayValue = *(Array *)array.value;\n")?;
                self.out_file
                    .write(b"    Value el = arrayValue.data[index.value + arrayValue.offset];\n")?;
                self.out_file.write(b"        push(array);\n")?;
                self.out_file.write(b"        push(el);\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::Slice => {
                self.out_file.write(b"    {\n")?;
                self.out_file.write(b"    Value upper = pop();\n")?;
                self.out_file.write(b"    Value lower = pop();\n")?;
                self.out_file.write(b"    Value array = pop();\n")?;
                self.out_file
                    .write(b"    Array *arrayValue = (Array *)array.value;\n")?;

                self.out_file
                    .write(b"   arrayValue->offset = lower.value + arrayValue->offset;\n")?;
                self.out_file
                    .write(b"   arrayValue->length = upper.value;\n")?;
                self.out_file.write(b"        push(array);\n")?;
                self.out_file.write(b"    }\n")?;
            }
            CheckedOpKind::If => {
                self.out_file.write(b"    _if();\n")?;
            }
            CheckedOpKind::Return => {
                self.out_file.write(b"    return;\n")?;
            }
            CheckedOpKind::Args => {
                self.out_file.write(
                    b"{
                        Array *arr = (Array *)malloc(sizeof(Array));
                        arr->offset = 0;
                        arr->length = argc;
                        Value *data;
                        data = (Value *)calloc(argc, sizeof(Value));
                        for (int i = 0; i < argc; i++)
                        {
                            data[i] = (Value){5, argv[i]};
                        }
                        arr->data = data;
                        push((Value){3, (int)arr});
                    }\n",
                )?;
            }
            CheckedOpKind::Cons => {
                self.out_file.write(
                    b"{
                        Value a = pop();
                        Value b = pop();
                        Array *arr = (Array *)malloc(sizeof(Array));
                        arr->offset = 0;
                        arr->length = 2;
                        Value *data;
                        data = (Value *)calloc(2, sizeof(Value));
                        
                        data[0] = b;
                        data[1] = a;

                        arr->data = data;
                        push((Value){3, (int)arr});
                    }\n",
                )?;
            }
            CheckedOpKind::Append => {
                self.out_file.write(
                b"{
                        Value array = pop();
                        Value element = pop();

                        Array *arrayValue = (Array *)array.value;
                        int length = arrayValue->length;
                        arrayValue->data = realloc(arrayValue->data, (length + 1) * sizeof(Value)); // Resize array

                        arrayValue->data[length] = element;
                        arrayValue->length++;

                        push(array);
                    }\n",
            )?;
            }
            CheckedOpKind::Range => {
                self.out_file.write(
                    b"{
        Value upper = pop();
        Value lower = pop();
        Array *arr = (Array *)malloc(sizeof(Array));
        arr->offset = 0;
        arr->length = upper.value - lower.value;
        Value *data;
        data = (Value *)calloc(arr->length, sizeof(Value));
        for (int i = lower.value; i < upper.value; i++)
        {
            data[i - lower.value] = (Value){1, i};
        }
        arr->data = data;
        push((Value){3, (int)arr});
                    }\n",
                )?;
            }
            CheckedOpKind::FunctionBaseCaseDefinition { .. } => {}
            CheckedOpKind::Binding { names, body } => {
                write!(self.out_file, "{{\n")?;
                for name in names.into_iter() {
                    write!(self.out_file, "Value {} = pop();\n", name)?;
                }
                for op in body {
                    self.emit(&op.kind)?;
                }
                write!(self.out_file, "}}\n")?;
            }
            CheckedOpKind::Variable { name } => {
                    write!(self.out_file, "push({});\n", name)?;
            }
            _ => todo!("{:?}", op_kind),
        }
        Ok(())
    }

    fn emit_push_operand(&mut self, operand: &CheckedOperand) -> io::Result<()> {
        match operand {
            CheckedOperand::Bool { value } => {
                write!(self.out_file, "push((Value){{0, {}}});\n", value)?
            }
            CheckedOperand::Int { value } => {
                write!(self.out_file, "push((Value){{1, {}}});\n", value)?
            }
            CheckedOperand::Char { value } => {
                write!(self.out_file, "push((Value){{2, {}}});\n", value)?
            }
            CheckedOperand::Array { values } => {
                let length = values.len();
                self.out_file.write(b"{\n")?;
                self.out_file
                    .write(b"    Array *arr = (Array *)malloc(sizeof(Array));\n")?;
                write!(self.out_file, "    arr->offset = 0;\n")?;
                write!(self.out_file, "    arr->length = {};\n", length)?;
                self.out_file.write(b"Value *data;\n")?;
                write!(
                    self.out_file,
                    "    data = (Value *)calloc({}, sizeof(Value));",
                    length
                )?;
                for (i, value) in values.into_iter().enumerate() {
                    write!(self.out_file, "    data[{}] = ", i)?;
                    self.emit_operand(value)?;
                    self.out_file.write(b";\n")?;
                }
                self.out_file.write(b"    arr->data = data;\n")?;
                self.out_file.write(b"    push((Value){3, (int)arr});\n")?;
                self.out_file.write(b"}\n")?;
            }
            CheckedOperand::Seq { .. } => {
                let lambda = self
                    .lambdas
                    .get(operand)
                    .expect(&format!("no lambda for operand {:?}", operand));
                write!(
                    self.out_file,
                    "    void (*{}_var)(int) = &{};\n",
                    lambda, lambda
                )?;
                write!(
                    self.out_file,
                    "    push((Value){{4, (int){}_var}});\n",
                    lambda
                )?;
            }
            CheckedOperand::String { value } => {
                write!(self.out_file, "push((Value){{5, \"{}\"}});\n", value)?
            }
        }
        Ok(())
    }

    //TODO maybe just declare at the start with #define
    fn get_type_tag(operand: &Operand) -> usize {
        match operand {
            Operand::Bool { value } => 0,
            Operand::Int { value } => 1,
            Operand::Char { value } => 2,
            Operand::Array { values } => 3,
            Operand::Seq { ops } => 4,
            Operand::String { value } => 5,
        }
    }

    fn predefine_lambdas(&mut self, ops: &Vec<CheckedOperation>) -> io::Result<()> {
        for op in ops {
            if let CheckedOpKind::Push { operand } = &op.kind {
                //TODO tomorrow: checkedOperand
                if let CheckedOperand::Seq { ops: lambda } = operand {
                    self.predefine_lambdas(lambda)?;
                    write!(
                        self.out_file,
                        "void lambda_{}() {{\n",
                        self.next_lambda_label
                    )?;
                    if !self.lambdas.contains_key(&operand) {
                        self.lambdas.insert(
                            operand.clone(),
                            format!("lambda_{}", self.next_lambda_label),
                        );
                    }
                    self.next_lambda_label += 1;
                    for lambda_op in lambda {
                        self.emit(&lambda_op.kind)?;
                    }
                    self.out_file.write(b"}\n")?;
                }
            }
        }
        Ok(())
    }

    fn emit_function_definition(&mut self, function: &CheckedFunction) -> io::Result<()> {
        self.predefine_lambdas(&function.body)?;

        if function.name == "main" {
            write!(self.out_file, "void main(int argc, char *argv[]) {{\n")?;
        } else {
            write!(self.out_file, "void {}() {{\n", function.name)?;
        }
        for base_case in &function.base_cases {
            write!(self.out_file, "{{\n")?;
            write!(self.out_file, "Value input = stack[sp - 1];\n")?;
            write!(
                self.out_file,
                "if (input.value == {}) {{\n",
                base_case.input
            )?;
            write!(self.out_file, "pop();\n")?;
            for op in &base_case.body {
                self.emit(&op.kind)?;
            }
            write!(self.out_file, "return;\n")?;
            write!(self.out_file, "}}\n")?;
            write!(self.out_file, "}}\n")?;
        }
        for op in &function.body {
            self.emit(&op.kind)?;
        }
        self.out_file.write(b"}\n")?;
        Ok(())
    }

    fn emit_operand_array(&mut self, values: &[CheckedOperand]) -> io::Result<()> {
        self.out_file.write(b"{")?;
        for (i, operand) in values.into_iter().enumerate() {
            self.emit_operand(operand)?;
            if i < values.len() - 1 {
                self.out_file.write(b", \n")?;
            }
        }
        self.out_file.write(b"}")?;
        Ok(())
    }

    fn emit_operand(&mut self, operand: &CheckedOperand) -> io::Result<()> {
        match operand {
            CheckedOperand::Bool { value } => write!(self.out_file, "(Value){{0, {}}}", value)?,
            CheckedOperand::Int { value } => write!(self.out_file, "(Value){{1, {}}}", value)?,
            CheckedOperand::Char { value } => write!(self.out_file, "(Value){{2, {}}}", value)?,
            CheckedOperand::Array { values } => {
                let length = values.len();
                self.out_file.write(b"{")?;
                self.out_file.write(b"    Array arr;")?;
                write!(self.out_file, "    arr.length = {};", length)?;
                self.out_file.write(b"    Value data = ")?;
                self.emit_operand_array(values)?; //TODO: doesn't work for arrays of arrays
                self.out_file.write(b";\n")?;
                self.out_file.write(b"    arr.data = data;\n")?;
            }
            CheckedOperand::Seq { ops } => todo!(),
            CheckedOperand::String { value } => todo!(),
        }
        Ok(())
    }
}
