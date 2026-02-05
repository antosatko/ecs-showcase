use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use arena::{Arena, Key};
use smol_str::SmolStr;

/* ===================== SOURCE ===================== */

#[derive(Debug, Clone)]
pub struct Span<T> {
    pub inner: T,
    pub location: SpanIndex,
}

#[derive(Debug, Clone, Copy)]
pub struct SpanIndex {
    pub index: usize,
    pub len: usize,
}

impl<T> Span<T> {
    pub fn new(inner: T, location: SpanIndex) -> Self {
        Self { inner, location }
    }

    pub fn map<U, F>(self, f: F) -> Span<U>
    where
        F: FnOnce(T) -> U,
    {
        Span::new(f(self.inner), self.location)
    }
}

impl<T> Deref for Span<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Span<T> {
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
    pub docs: Vec<Span<SmolStr>>,
    pub objects: Arena<Span<Object>, ObjectTag>,
    pub symbols: HashMap<SmolStr, Key<ObjectTag>>,
}

/* ===================== OBJECTS ===================== */

#[derive(Debug, Clone)]
pub enum Object {
    Scheduler {
        ident: Span<SmolStr>,
        resources: Vec<Span<Value>>,
        systems: Vec<Span<IdentifierPath>>,
        docs: Vec<Span<SmolStr>>,
    },

    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub ident: Span<SmolStr>,
    pub parameters: Vec<Span<Parameter>>,
    pub return_type: Option<Span<Type>>,
    pub body: Span<Block>,
    pub docs: Vec<Span<SmolStr>>,
}

/* ===================== BLOCK / STATEMENTS ===================== */

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Span<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Var {
        ident: Span<SmolStr>,
        ty: Option<Span<Type>>,
        expression: Option<Span<Expression>>,
    },

    Return {
        expression: Span<Expression>,
    },

    Break {
        label: Option<Span<SmolStr>>,
    },
    Continue {
        label: Option<Span<SmolStr>>,
    },

    Loop {
        label: Option<Span<SmolStr>>,
        body: Span<Block>,
    },

    Expr {
        expression: Span<Expression>,
    },

    If {
        condition: Span<Expression>,
        then_block: Span<Block>,
        else_if: Vec<(Span<Expression>, Span<Block>)>,
        else_block: Option<Span<Block>>,
    },

    While {
        label: Option<Span<SmolStr>>,
        condition: Span<Expression>,
        body: Span<Block>,
    },
}

/* ===================== PARAMETERS ===================== */

#[derive(Debug, Clone)]
pub struct Parameter {
    pub ident: Span<SmolStr>,
    pub ty: Span<Type>,
}

/* ===================== EXPRESSIONS ===================== */

#[derive(Debug, Clone)]
pub enum Expression {
    Value(Value),

    Binary {
        l: Span<Box<Expression>>,
        r: Span<Box<Expression>>,
        op: Span<Operator>,
    },
}

/* ===================== OPERATORS ===================== */

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub struct Value {
    pub literal: Span<Literal>,
    pub postfix: Vec<Span<Postfix>>,
}

#[derive(Debug, Clone)]
pub enum Postfix {
    Field(Span<SmolStr>),
    Call(Vec<Span<Expression>>),
    Index(Span<Expression>),
    Refs(usize),
    Derefs(usize),
}

/* ===================== TYPES ===================== */

#[derive(Debug, Clone)]
pub struct Type {
    pub path: Span<IdentifierPath>,
}

/* ===================== IDENTIFIERS ===================== */

#[derive(Debug, Clone)]
pub struct IdentifierPath {
    pub path: Vec<Span<SmolStr>>,
}

/* ===================== LITERALS ===================== */
#[derive(Debug, Clone)]
pub enum Literal {
    Identifier(IdentifierPath),

    Number(Number),

    String(SmolStr),
    Char(char),

    Array(Vec<Span<Expression>>),
    Tuple(Vec<Span<Expression>>),
}

/* ===================== NUMBERS ===================== */

#[derive(Debug, Clone)]
pub struct Number {
    pub value: NumberValue,
    pub size: Option<u32>,
}

#[derive(Debug, Clone)]
pub enum NumberValue {
    Float(f64),
    Uint(u128),
    Int(i128),
    Number(u128),
}

/* ====================== LOWERING =====================*/
pub fn numeric_literal(s: &str) -> Number {
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
        Number {
            value: NumberValue::Float(value),
            size,
        }
    } else {
        let value: i128 = num_str.parse().unwrap();
        let (number_value, size) = match suffix.map(|s| s.to_lowercase()) {
            Some(ref s) if s.starts_with('u') => {
                let size = s[1..].parse().ok();
                (NumberValue::Uint(value as u128), size)
            }
            Some(ref s) if s.starts_with('i') => {
                let size = s[1..].parse().ok();
                (NumberValue::Int(value), size)
            }
            Some(ref s) if s.starts_with('c') => (NumberValue::Int(value), None),
            None => (NumberValue::Number(value as u128), None),
            Some(other) => panic!("Unknown numeric suffix: {}", other),
        };

        Number {
            value: number_value,
            size,
        }
    }
}

pub fn float_literal(s: &str) -> NumberValue {
    let s = s.replace('_', "");
    NumberValue::Float(s.parse().unwrap())
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

#[derive(Debug, Clone)]
pub enum ExprItem {
    Value(Expression),
    Operator(Operator),
}
