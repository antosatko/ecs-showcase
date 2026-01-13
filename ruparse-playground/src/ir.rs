use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use arena::{Arena, Key};
use ruparse::lexer::TextLocation;
use smol_str::SmolStr;

/* ===================== SOURCE ===================== */

#[derive(Debug, Clone)]
pub struct Source<T> {
    pub inner: T,
    pub location: TextLocation,
}

impl<T> Source<T> {
    pub fn new(inner: T, location: TextLocation) -> Self {
        Self { inner, location }
    }

    pub fn map<U, F>(self, f: F) -> Source<U>
    where
        F: FnOnce(T) -> U,
    {
        Source::new(f(self.inner), self.location)
    }
}

impl<T> Deref for Source<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Source<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/* ===================== MODULE ===================== */
#[derive(Debug, Copy, Clone)]
pub struct ObjectTag;

#[derive(Debug, Clone)]
pub struct Module {
    pub name: SmolStr,
    pub docs: Vec<Source<SmolStr>>,
    pub objects: Arena<Source<Object>, ObjectTag>,
    pub symbols: HashMap<SmolStr, Key<ObjectTag>>,
}

/* ===================== OBJECTS ===================== */

#[derive(Debug, Clone)]
pub enum Object {
    Scheduler {
        ident: Source<SmolStr>,
        resources: Vec<Source<Value>>,
        systems: Vec<Source<IdentifierPath>>,
        docs: Vec<Source<SmolStr>>,
    },

    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub ident: Source<SmolStr>,
    pub parameters: Vec<Source<Parameter>>,
    pub return_type: Option<Source<Type>>,
    pub body: Source<Block>,
    pub docs: Vec<Source<SmolStr>>,
}

/* ===================== BLOCK / STATEMENTS ===================== */

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Source<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Var {
        ident: Source<SmolStr>,
        ty: Option<Source<Type>>,
        expression: Option<Source<Expression>>,
    },

    Return {
        expression: Source<Expression>,
    },

    Break,
    Continue,

    Loop {
        label: Option<Source<SmolStr>>,
        body: Source<Block>,
    },

    Expr {
        expression: Source<Expression>,
    },
}

/* ===================== PARAMETERS ===================== */

#[derive(Debug, Clone)]
pub struct Parameter {
    pub ident: Source<SmolStr>,
    pub ty: Source<Type>,
}

/* ===================== EXPRESSIONS ===================== */

#[derive(Debug, Clone)]
// #[derive(Debug)]
pub enum Expression {
    Value(Value),

    Binary {
        l: Source<Box<Expression>>,
        r: Source<Box<Expression>>,
        op: Source<Operator>,
    },
}

/* ===================== OPERATORS ===================== */

#[derive(Debug, Clone, Copy)]
// #[derive(Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Eq,
    NEq,
    Gr,
    Le,
    GrEq,
    LeEq,

    And,
    Or,

    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

/* ===================== VALUES ===================== */

// #[derive(Debug)]
#[derive(Debug, Clone)]
pub struct Value {
    pub literal: Source<Literal>,
    pub postfix: Vec<Source<Postfix>>,
}

#[derive(Debug, Clone)]
// #[derive(Debug)]
pub enum Postfix {
    Field(Source<SmolStr>),
    Call(Vec<Source<Expression>>),
    Index(Source<Expression>),
}

/* ===================== TYPES ===================== */

#[derive(Debug, Clone)]
pub struct Type {
    pub path: Source<IdentifierPath>,
}

/* ===================== IDENTIFIERS ===================== */

// #[derive(Debug)]
#[derive(Debug, Clone)]
pub struct IdentifierPath {
    pub path: Vec<Source<SmolStr>>,
}

/* ===================== LITERALS ===================== */

// #[derive(Debug)]
#[derive(Debug, Clone)]
pub enum Literal {
    Identifier(IdentifierPath),

    Number(Number),

    String(SmolStr),
    Char(char),

    Array(Vec<Source<Expression>>),
    Tuple(Vec<Source<Expression>>),
}

/* ===================== NUMBERS ===================== */

// #[derive(Debug)]
#[derive(Debug, Clone)]
pub struct Number {
    pub value: NumberValue,
    pub size: Option<u32>,
}

// #[derive(Debug)]

#[derive(Debug, Clone)]
pub enum NumberValue {
    Float(f64),
    Uint(u128),
    Int(i128),
    Number(u128),
}

/* ====================== LOWERING =====================*/
pub fn numeric_literal(s: &str) -> crate::ir::Number {
    let s = s.replace('_', "");

    let is_float = s.contains('.') || s.contains('f') || s.contains('F');

    let (num_str, suffix) =
        if let Some((num, suff)) = s.rsplit_once(|c: char| c.is_ascii_alphabetic()) {
            (num, Some(suff))
        } else {
            (&s[..], None)
        };

    if is_float {
        let value: f64 = num_str.parse().unwrap();
        let size = suffix.map(|s| s.parse().ok()).flatten();
        crate::ir::Number {
            value: crate::ir::NumberValue::Float(value),
            size,
        }
    } else {
        let value: i128 = num_str.parse().unwrap();
        let (number_value, size) = match suffix.map(|s| s.to_lowercase()) {
            Some(ref s) if s.starts_with('u') => {
                let size = s[1..].parse().ok();
                (crate::ir::NumberValue::Uint(value as u128), size)
            }
            Some(ref s) if s.starts_with('i') => {
                let size = s[1..].parse().ok();
                (crate::ir::NumberValue::Int(value), size)
            }
            Some(ref s) if s.starts_with('c') => (crate::ir::NumberValue::Int(value), None),
            None => (crate::ir::NumberValue::Number(value as u128), None),
            Some(other) => panic!("Unknown numeric suffix: {}", other),
        };

        crate::ir::Number {
            value: number_value,
            size,
        }
    }
}

pub fn float_literal(s: &str) -> crate::ir::NumberValue {
    let s = s.replace('_', "");
    crate::ir::NumberValue::Float(s.parse().unwrap())
}

pub fn char_literal(s: &str) -> char {
    if s.starts_with(r"'\u{") {
        let unicode = s.trim_start_matches(r"'\u{").trim_end_matches("}'");
        char::from_u32(unicode.replace("_", "").parse::<u32>().unwrap()).unwrap()
    } else if s.starts_with(r"\'") {
        match &s[2..3] {
            "0" => '\0',
            "a" => '\x07',
            "b" => '\x08',
            "f" => '\x0C',
            "n" => '\n',
            "r" => '\r',
            "t" => '\t',
            "v" => '\x0B',
            other => other.chars().next().unwrap(),
        }
    } else {
        s.chars().nth(1).unwrap()
    }
}
pub fn string_literal(s: &str) -> SmolStr {
    let start_hashes = s.chars().take_while(|&c| c == '#').count();
    let content = &s[start_hashes + 1..s.len() - (start_hashes + 1)];
    content.into()
}

/* ================ PRECEDENCE ============ */

#[derive(Copy, Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

impl Operator {
    pub fn precedence(self) -> u8 {
        match self {
            Operator::Mul | Operator::Div | Operator::Mod => 70,
            Operator::Add | Operator::Sub => 60,

            Operator::Gr | Operator::Le | Operator::GrEq | Operator::LeEq => 50,

            Operator::Eq | Operator::NEq => 45,
            Operator::And => 30,
            Operator::Or => 20,

            Operator::Assign
            | Operator::AddAssign
            | Operator::SubAssign
            | Operator::MulAssign
            | Operator::DivAssign
            | Operator::ModAssign => 10,
        }
    }

    pub fn associativity(self) -> Associativity {
        match self {
            Operator::Assign
            | Operator::AddAssign
            | Operator::SubAssign
            | Operator::MulAssign
            | Operator::DivAssign
            | Operator::ModAssign => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

pub enum ExprItem {
    Value(crate::ir::Expression),
    Operator(Operator),
}
