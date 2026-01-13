use std::collections::HashMap;

use arena::Arena;
use ruparse::parser::Nodes;
use smol_str::SmolStr;

use crate::ir::{
    Associativity, ExprItem, Function, Module, Object, Source, char_literal, numeric_literal,
    string_literal,
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
            docs: docstrings(src, node),
            objects: Arena::new(),
            symbols: HashMap::new(),
        };

        node.get_list("top level statements")
            .iter()
            .for_each(|s| match s.get_name() {
                "scheduler" => {
                    let ident = expect_ident(src, s);
                    let docs = docstrings(src, s);
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
                        docs,
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

                    let obj = Object::Function(Function {
                        ident: ident.clone(),
                        parameters: params,
                        return_type,
                        body,
                        docs,
                    });

                    let key = this.objects.push(Source::new(obj, s.location()));
                    this.symbols.insert(ident.inner, key);
                }
                _ => s.ice("fix your damn compiler"),
            });

        this
    }
}

#[track_caller]
pub fn ident(src: &str, node: &Nodes) -> Option<Source<SmolStr>> {
    node.try_get_node("identifier").as_ref().map(|t| {
        Source::new(
            t.expect_node("identifier").stringify(src).into(),
            t.location(),
        )
    })
}

#[track_caller]
pub fn expect_ident(src: &str, node: &Nodes) -> Source<SmolStr> {
    let t = node.expect_node("identifier");
    match t {
        Nodes::Node(n) => {
            let tok = n.try_get_node("identifier").as_ref().unwrap();
            Source::new(tok.stringify(src).into(), tok.location())
        }
        Nodes::Token(tok) => Source::new(tok.stringify(src).into(), tok.location),
    }
}

#[track_caller]
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
    let items = expression_items(src, node);
    let mut pos = 0;
    parse_expression_prec(&items, &mut pos, 0)
}

fn parse_expression_prec(
    items: &[Source<ExprItem>],
    pos: &mut usize,
    min_prec: u8,
) -> Source<crate::ir::Expression> {
    let mut lhs = match &items[*pos].inner {
        ExprItem::Value(expr) => {
            let src = &items[*pos];
            *pos += 1;
            Source::new(expr.clone(), src.location)
        }
        _ => unreachable!("expression must start with value"),
    };

    while *pos < items.len() {
        let op_item = &items[*pos];
        let op = match &op_item.inner {
            ExprItem::Operator(op) => *op,
            _ => break,
        };

        let prec = op.precedence();
        if prec < min_prec {
            break;
        }

        *pos += 1;

        let next_min_prec = match op.associativity() {
            Associativity::Left => prec + 1,
            Associativity::Right => prec,
        };

        let rhs = parse_expression_prec(items, pos, next_min_prec);
        let lhs_loc = lhs.location;

        lhs = Source::new(
            crate::ir::Expression::Binary {
                l: lhs.map(Box::new),
                r: rhs.map(Box::new),
                op: Source::new(op, op_item.location),
            },
            lhs_loc,
        );
    }

    lhs
}

fn expression_items(src: &str, node: &Nodes) -> Vec<Source<ExprItem>> {
    let mut items = Vec::new();

    let lvalue_node = node.expect_node("lvalue");
    let lvalue = value(src, &lvalue_node);

    items.push(Source::new(
        ExprItem::Value(crate::ir::Expression::Value(lvalue.inner)),
        lvalue.location,
    ));

    for entry in node.get_list("rest") {
        match entry {
            Nodes::Token(_) => {
                let op = operator(src, entry);
                items.push(Source::new(ExprItem::Operator(op.inner), op.location));
            }

            Nodes::Node(_) => {
                let v = value(src, entry);
                items.push(Source::new(
                    ExprItem::Value(crate::ir::Expression::Value(v.inner)),
                    v.location,
                ));
            }
        }
    }

    items
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
