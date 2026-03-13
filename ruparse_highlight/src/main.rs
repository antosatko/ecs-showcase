use core::panic;
use std::fs;

use lang_ir::ast::{
    Body, Expression, Function, IdentifierPath, Literal, Module, Object, Parameter, Postfix,
    Statement, Type, Value,
};
use line_index::{LineCol, LineIndex, TextSize};
use ruparse_playground::{
    grammar::{Token, gen_parser},
    ruparse_lowering::module_named,
};

use crate::html::{MyStyle, Style};

mod html;

const TEST: &str = include_str!("../../ruparse-playground/src/lang");

fn main() {
    let indexed = index_file(TEST);

    let style = MyStyle;
    fs::write("test.html", &style.render(&indexed, TEST)).unwrap();
}

#[derive(Debug, Copy, Clone)]
struct Span {
    offset: usize,
    len: usize,
    line: usize,
    column: usize,
    ty: u8,
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Types {
    Comment,
    Ident,
    Keyword,
    String,
    Number,
    Char,
    Operator,
    Type,
    SpecialOperator,
    Label,
}

fn index_file(src: &str) -> Vec<Span> {
    let line_index = LineIndex::new(src);
    let mut spans = Vec::new();

    let parser = gen_parser();
    let tokens = parser.lexer.lex_utf8(src).unwrap();
    index_tokens(&tokens, &mut spans);
    let module = parser.parse(&tokens, src).unwrap();

    let ast = module_named("", src, module.entry);

    ast.index(&line_index, &mut spans);

    spans
}

impl IntoSpan for Token<'_> {
    fn span(&self, ty: Types, line_index: &LineIndex) -> Span {
        Span {
            offset: self.index,
            len: self.len,
            line: self.location.line,
            column: self.location.column,
            ty: ty as _,
        }
    }
}

fn index_tokens(tokens: &Vec<Token>, spans: &mut Vec<Span>) {
    let line_index = &LineIndex::new("");
    for token in tokens {
        match &token.kind {
            ruparse::lexer::TokenKinds::Complex(t) => match *t {
                "tl docstr" | "docstr" => spans.push(token.span(Types::Comment, line_index)),
                "numeric" | "float" => spans.push(token.span(Types::Number, line_index)),
                "char" => spans.push(token.span(Types::Char, line_index)),
                "string" => spans.push(token.span(Types::String, line_index)),
                a => panic!("got a: {a}"),
            },
            _ => (),
        }
    }
}

trait IndexedWalk {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>);
}

trait IntoSpan {
    fn span(&self, ty: Types, line_index: &LineIndex) -> Span;
    fn span_word(&self, ty: Types, line_index: &LineIndex, word: &str) -> Span {
        let mut this = self.span(ty, line_index);
        this.len = word.chars().count();
        this
    }
}

impl<T> IntoSpan for lang_ir::ast::Span<T> {
    fn span(&self, ty: Types, line_index: &LineIndex) -> Span {
        let LineCol { line, col } = line_index.line_col(TextSize::new(self.location.index as _));
        Span {
            offset: self.location.index,
            len: self.location.len,
            line: line as _,
            column: col as _,
            ty: ty as _,
        }
    }
}

/* ===================== IMPLEMENTATIONS ===================== */

impl IndexedWalk for Module {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        spans.extend(self.docs.iter().map(|d| d.span(Types::Comment, line_index)));
        self.objects.iter().for_each(|o| o.index(line_index, spans));
    }
}

impl IndexedWalk for lang_ir::ast::Span<Object> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        match &self.inner {
            Object::Scheduler {
                ident,
                resources,
                systems,
                docs,
            } => {
                spans.push(self.span_word(Types::Keyword, line_index, "scheduler"));
                spans.push(ident.span(Types::Ident, line_index));
                spans.extend(docs.iter().map(|d| d.span(Types::Comment, line_index)));

                resources.iter().for_each(|r| r.index(line_index, spans));
                systems.iter().for_each(|s| s.index(line_index, spans));
            }
            Object::Function(Function {
                ident,
                parameters,
                return_type,
                body,
                docs,
            }) => {
                spans.push(self.span_word(Types::Keyword, line_index, "function"));
                spans.push(ident.span(Types::Ident, line_index));
                spans.extend(docs.iter().map(|d| d.span(Types::Comment, line_index)));

                parameters.iter().for_each(|p| p.index(line_index, spans));
                if let Some(ret) = return_type {
                    ret.index(line_index, spans);
                }
                body.index(line_index, spans);
            }
        }
    }
}

impl IndexedWalk for lang_ir::ast::Span<Body> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        match &self.inner {
            Body::Block(stmts) => stmts.iter().for_each(|s| s.index(line_index, spans)),
            Body::Statement(expr) => {
                spans.push(self.span_word(Types::SpecialOperator, line_index, "=>"));
                expr.index(line_index, spans)
            }
        }
    }
}

impl IndexedWalk for lang_ir::ast::Span<Statement> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        match &self.inner {
            Statement::Var {
                ident,
                ty,
                expression,
            } => {
                spans.push(self.span_word(Types::Keyword, line_index, "var"));
                spans.push(ident.span(Types::Ident, line_index));
                if let Some(t) = ty {
                    t.index(line_index, spans);
                }
                if let Some(e) = expression {
                    e.index(line_index, spans);
                }
            }
            Statement::If {
                condition,
                then_block,
                else_if,
                else_block,
            } => {
                spans.push(self.span_word(Types::Keyword, line_index, "if"));
                condition.index(line_index, spans);
                // If your syntax uses 'if cond => block', we capture it here:
                // spans.push(condition.span_word(Types::SpecialOperator, line_index, "=>"));
                then_block.index(line_index, spans);

                for elif in else_if {
                    spans.push(elif.span_word(Types::Keyword, line_index, "else if"));
                    elif.inner.condition.index(line_index, spans);
                    elif.inner.block.index(line_index, spans);
                }
                if let Some(eb) = else_block {
                    spans.push(eb.span_word(Types::Keyword, line_index, "else"));
                    eb.inner.block.index(line_index, spans);
                }
            }
            Statement::While {
                label,
                condition,
                body,
            } => {
                spans.push(self.span_word(Types::Keyword, line_index, "while"));
                if let Some(l) = label {
                    spans.push(l.span(Types::Label, line_index)); // Highlight as Label
                }
                condition.index(line_index, spans);
                body.index(line_index, spans);
            }
            Statement::Loop { label, body } => {
                spans.push(self.span_word(Types::Keyword, line_index, "loop"));
                if let Some(l) = label {
                    spans.push(l.span(Types::Label, line_index)); // Highlight as Label
                }
                body.index(line_index, spans);
            }
            Statement::Break { label } => {
                spans.push(self.span_word(Types::Keyword, line_index, "break"));
                if let Some(l) = label {
                    spans.push(l.span(Types::Label, line_index));
                }
            }
            Statement::Continue { label } => {
                spans.push(self.span_word(Types::Keyword, line_index, "continue"));
                if let Some(l) = label {
                    spans.push(l.span(Types::Label, line_index));
                }
            }
            Statement::Return { expression } => {
                spans.push(self.span_word(Types::Keyword, line_index, "return"));
                expression.index(line_index, spans);
            }
            Statement::Expr { expression } => expression.index(line_index, spans),
        }
    }
}
impl IndexedWalk for lang_ir::ast::Span<Parameter> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        spans.push(self.inner.ident.span(Types::Ident, line_index));
        self.inner.ty.index(line_index, spans);
    }
}

impl IndexedWalk for lang_ir::ast::Span<Type> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        // Differentiate Type highlighting from Standard Identifiers
        for ident in &self.inner.path.inner.path {
            spans.push(ident.span(Types::Type, line_index));
        }
    }
}

impl IndexedWalk for lang_ir::ast::Span<IdentifierPath> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        for ident in &self.inner.path {
            spans.push(ident.span(Types::Ident, line_index));
        }
    }
}

impl IndexedWalk for lang_ir::ast::Span<Expression> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        match &self.inner {
            Expression::Value(v) => v.index(line_index, spans),
            Expression::Binary { l, r, op } => {
                l.index(line_index, spans);
                // Highlight the Operator
                spans.push(op.span(Types::Operator, line_index));
                r.index(line_index, spans);
            }
        }
    }
}

impl IndexedWalk for lang_ir::ast::Span<Box<Expression>> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        let inner_span = lang_ir::ast::Span {
            location: self.location,
            inner: *self.inner.clone(),
        };
        inner_span.index(line_index, spans);
    }
}

// Kept one consistent Value implementation and fixed the missing literal call
impl IndexedWalk for Value {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        self.literal.index(line_index, spans); // Fixed missing call
        for p in &self.postfix {
            p.index(line_index, spans);
        }
    }
}

impl IndexedWalk for lang_ir::ast::Span<Value> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        self.inner.index(line_index, spans);
    }
}

impl IndexedWalk for lang_ir::ast::Span<Postfix> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        match &self.inner {
            Postfix::Field(ident) => spans.push(ident.span(Types::Ident, line_index)),
            Postfix::Call(args) => args.iter().for_each(|a| a.index(line_index, spans)),
            Postfix::Index(expr) => expr.index(line_index, spans),
            // Assuming Refs and Derefs are operators like `&` and `*`
            Postfix::Refs(_) | Postfix::Derefs(_) => {
                // If your Span<Postfix> encapsulates the exact location of `&` or `*`,
                // you can push them as Types::Operator here.
                spans.push(self.span(Types::Operator, line_index));
            }
        }
    }
}

impl IndexedWalk for lang_ir::ast::Span<Literal> {
    fn index(&self, line_index: &LineIndex, spans: &mut Vec<Span>) {
        match &self.inner {
            Literal::Identifier(path) => {
                for ident in &path.path {
                    spans.push(ident.span(Types::Ident, line_index));
                }
            }
            Literal::Array(exprs) | Literal::Tuple(exprs) => {
                exprs.iter().for_each(|e| e.index(line_index, spans));
            }
            // Mapped to specific expanded types
            Literal::Number(_) => spans.push(self.span(Types::Number, line_index)),
            Literal::String(_) => spans.push(self.span(Types::String, line_index)),
            Literal::Char(_) => spans.push(self.span(Types::Char, line_index)),
        }
    }
}
