use std::collections::HashMap;

use arena::Arena;
use ruparse::parser::Nodes;
use smol_str::SmolStr;

use crate::ir::{
    Module, Object, Source, char_literal, float_literal, numeric_literal, string_literal,
};

impl<'a> From<Nodes<'a>> for Module {
    fn from(value: Nodes<'a>) -> Self {
        Module::named("module", "", &value)
    }
}

impl Module {
    pub fn named(name: impl Into<SmolStr>, src: &str, node: &Nodes) -> Self {
        let mut this = Self {
            name: name.into(),
            objects: Arena::new(),
            symbols: HashMap::new(),
        };

        node.get_list("top level statements")
            .iter()
            .for_each(|s| match s.get_name() {
                "scheduler" => {
                    let ident = expect_ident(src, s);
                    let mut resources = Vec::new();
                    let mut systems = Vec::new();
                    if let Some(res) = s.try_get_node("resources").as_ref() {
                        for r in res.get_list("resources") {
                            resources.push(value(src, r));
                        }
                    }
                    if let Some(sys) = s.try_get_node("systems").as_ref() {
                        for p in sys.get_list("systems") {
                            systems.push(ident_path(src, p));
                        }
                    }
                    let obj = Object::Scheduler {
                        ident: ident.clone(),
                        resources,
                        systems,
                    };
                    let key = this.objects.push(Source::new(obj, s.location()));
                    this.symbols.insert(ident.inner, key);
                }
                "function" => {
                    let ident = expect_ident(src, s);

                    let params = parameters(src, s.expect_node("parameters"));

                    let return_type = s.try_get_node("return type").as_ref().map(|t| ty(src, &t));

                    let body = block(src, s.expect_node("code block"));

                    let docs = docstrings(src, s);

                    let obj = Object::Function {
                        ident: ident.clone(),
                        parameters: params,
                        return_type,
                        body,
                        docs,
                    };

                    let key = this.objects.push(Source::new(obj, s.location()));
                    this.symbols.insert(ident.inner, key);
                }
                _ => s.ice("fix your damn compiler"),
            });

        this
    }
}

pub fn ident(src: &str, node: &Nodes) -> Option<Source<SmolStr>> {
    node.try_get_node("identifier").as_ref().map(|t| {
        Source::new(
            t.expect_node("identifier").stringify(src).into(),
            t.location(),
        )
    })
}

pub fn expect_ident(src: &str, node: &Nodes) -> Source<SmolStr> {
    let t = node.expect_node("identifier");
    match t {
        Nodes::Node(n) => {
            // Node containing the identifier token
            let tok = n.try_get_node("identifier").as_ref().unwrap(); // now safe
            Source::new(tok.stringify(src).into(), tok.location())
        }
        Nodes::Token(tok) => {
            // The identifier is directly a token
            Source::new(tok.stringify(src).into(), tok.location)
        }
    }
}
fn ident_path(src: &str, node: &Nodes) -> Source<crate::ir::IdentifierPath> {
    let path = node
        .get_list("path")
        .iter()
        .map(|p| Source::new(p.stringify(src).into(), p.location()))
        .collect();

    Source::new(crate::ir::IdentifierPath { path }, node.location())
}

fn literal(src: &str, node: &Nodes) -> Source<crate::ir::Literal> {
    match node {
        Nodes::Node(n) => match n.name {
            "identifier path" => Source::new(
                crate::ir::Literal::Identifier(ident_path(src, node).inner),
                node.location(),
            ),
            "array literal" => {
                let elements = n
                    .get_list("elements")
                    .iter()
                    .map(|e| expression(src, e))
                    .collect();
                Source::new(crate::ir::Literal::Array(elements), node.location())
            }
            "tuple" => {
                let elements = n
                    .get_list("elements")
                    .iter()
                    .map(|e| expression(src, e))
                    .collect();
                Source::new(crate::ir::Literal::Tuple(elements), node.location())
            }
            other => node.ice(&format!("Unhandled literal node: {}", other)),
        },
        Nodes::Token(tok) => match &tok.kind {
            ruparse::lexer::TokenKinds::Complex(kind) => match kind.as_ref() {
                "string" => {
                    let s = string_literal(&tok.stringify(src));
                    Source::new(crate::ir::Literal::String(s), tok.location)
                }
                "char" => {
                    let c = char_literal(&tok.stringify(src));
                    Source::new(crate::ir::Literal::Char(c), tok.location)
                }
                "numeric" | "float" => {
                    let num = numeric_literal(&tok.stringify(src));
                    Source::new(crate::ir::Literal::Number(num), tok.location)
                }
                other => panic!("Unhandled token literal kind: {}", other),
            },
            _ => panic!("Unexpected token kind for literal: {:?}", tok.kind),
        },
    }
}
fn value(src: &str, node: &Nodes) -> Source<crate::ir::Value> {
    let literal = literal(src, node.expect_node("literal"));

    Source::new(
        crate::ir::Value {
            literal,
            postfix: Vec::new(),
        },
        node.location(),
    )
}
fn ty(src: &str, node: &Nodes) -> Source<crate::ir::Type> {
    let path = ident_path(src, node.expect_node("path"));
    Source::new(crate::ir::Type { path }, node.location())
}

fn parameter(src: &str, node: &Nodes) -> Source<crate::ir::Parameter> {
    let ident = expect_ident(src, node.expect_node("identifier"));
    let ty = ty(src, node.expect_node("type"));
    Source::new(crate::ir::Parameter { ident, ty }, node.location())
}

fn parameters(src: &str, node: &Nodes) -> Vec<Source<crate::ir::Parameter>> {
    node.get_list("parameters")
        .iter()
        .map(|p| parameter(src, p))
        .collect()
}
#[track_caller]
fn docstrings(src: &str, node: &Nodes) -> Vec<Source<SmolStr>> {
    node.expect_node("docs")
        .get_list("docstr")
        .iter()
        .map(|d| Source::new(d.stringify(src).into(), d.location()))
        .collect()
}

fn block(src: &str, node: &Nodes) -> Source<crate::ir::Block> {
    let mut statements = Vec::new();

    for stmt_node in node.get_list("statements") {
        let stmt = match stmt_node.get_name() {
            "variable" => {
                let ident = expect_ident(src, stmt_node.expect_node("identifier"));
                let ty = stmt_node.try_get_node("type").as_ref().map(|t| ty(src, t));
                let expression = stmt_node
                    .try_get_node("expression")
                    .as_ref()
                    .map(|e| expression(src, e));

                Source::new(
                    crate::ir::Statement::Var {
                        ident,
                        ty,
                        expression,
                    },
                    stmt_node.location(),
                )
            }

            "return" => {
                let expr = expression(src, stmt_node.expect_node("expression"));
                Source::new(
                    crate::ir::Statement::Return { expression: expr },
                    stmt_node.location(),
                )
            }

            "loop" => {
                let label = stmt_node
                    .try_get_node("label")
                    .as_ref()
                    .map(|l| expect_ident(src, l));
                let body_node = stmt_node.expect_node("code block");
                let body = block(src, &body_node);

                Source::new(
                    crate::ir::Statement::Loop { label, body },
                    stmt_node.location(),
                )
            }

            "expression statement" => {
                let expr = expression(src, stmt_node.expect_node("expression"));
                Source::new(
                    crate::ir::Statement::Expr { expression: expr },
                    stmt_node.location(),
                )
            }

            "break" => Source::new(crate::ir::Statement::Break, stmt_node.location()),
            "continue" => Source::new(crate::ir::Statement::Continue, stmt_node.location()),

            other => stmt_node.ice(&format!("Unhandled statement type: {}", other)),
        };

        statements.push(stmt);
    }

    Source::new(crate::ir::Block { statements }, node.location())
}
fn expression(src: &str, node: &Nodes) -> Source<crate::ir::Expression> {
    let lvalue_node = node.expect_node("lvalue");
    let lvalue = value(src, &lvalue_node);

    if let Some(op_node) = node.try_get_node("operator").as_ref() {
        let rvalue_node = node.expect_node("rvalue");
        let rvalue = expression(src, &rvalue_node).map(Box::new);
        let op = operator(src, &op_node);

        Source::new(
            crate::ir::Expression::Binary {
                l: Source::new(
                    Box::new(crate::ir::Expression::Value(lvalue.inner)),
                    lvalue.location,
                ),
                r: rvalue,
                op,
            },
            node.location(),
        )
    } else {
        // Simple value
        Source::new(crate::ir::Expression::Value(lvalue.inner), lvalue.location)
    }
}
fn operator(src: &str, node: &Nodes) -> Source<crate::ir::Operator> {
    let op = match node.stringify(src) {
        "+" => crate::ir::Operator::Add,
        "-" => crate::ir::Operator::Sub,
        "*" => crate::ir::Operator::Mul,
        "/" => crate::ir::Operator::Div,
        "%" => crate::ir::Operator::Mod,

        "==" => crate::ir::Operator::Eq,
        "!=" => crate::ir::Operator::NEq,
        ">" => crate::ir::Operator::Gr,
        "<" => crate::ir::Operator::Le,
        ">=" => crate::ir::Operator::GrEq,
        "<=" => crate::ir::Operator::LeEq,

        "&&" => crate::ir::Operator::And,
        "||" => crate::ir::Operator::Or,

        "=" => crate::ir::Operator::Assign,
        "+=" => crate::ir::Operator::AddAssign,
        "-=" => crate::ir::Operator::SubAssign,
        "*=" => crate::ir::Operator::MulAssign,
        "/=" => crate::ir::Operator::DivAssign,
        "%=" => crate::ir::Operator::ModAssign,

        other => node.ice(&format!("Unknown operator: {}", other)),
    };

    Source::new(op, node.location())
}
