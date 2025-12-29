use ruparse::Parser;
use ruparse::api::ext::*;
use ruparse::grammar::validator::Validator;
use ruparse::grammar::{MatchToken, VarKind, VariableKind};

const IDENTIFIER: VarKind<'static> = local("identifier");
const IDENTIFIER_VAR: (&'static str, VariableKind) = ("identifier", VariableKind::Node);

const KEYWORDS: &[&'static str] = &[
    "systems",
    "resources",
    "scheduler",
    "system",
    "struct",
    "component",
    "var",
];

fn keyword(kw: &'static str) -> MatchToken<'static> {
    if !KEYWORDS.contains(&kw) {
        panic!("{kw} is not a keyword");
    }
    word(kw)
}

pub fn gen_parser() -> Parser<'static> {
    let mut parser = Parser::new();

    parser
        .lexer
        .add_tokens("+-*/;\"':({[<>]})|&!?=".split("").filter(|s| !s.is_empty()));

    let keywords = parser
        .grammar
        .new_enum("keywords")
        .options(KEYWORDS.iter().map(|kw| word(kw)))
        .build();

    let ident = parser
        .grammar
        .new_node("identifier")
        .rules([
            isnt(keywords).hint("Keywords are reserved and can not be used for identifiers"),
            is(text()).set(IDENTIFIER),
        ])
        .variables([node_var("identifier")])
        .build();

    let end_stmt = parser
        .grammar
        .new_node("end statement")
        .rules([is_one_of([option(token(";")), option(newline())])
            .hint("Expected to end statement on a new line or semicolon")])
        .build();

    let systems = parser
        .grammar
        .new_node("systems")
        .rules([
            is(keyword("systems")).commit(),
            is(token("{")),
            while_(node("identifier")).set(local("systems")),
            is(token("}")),
        ])
        .variables([list_var("systems")])
        .build();

    let resources = parser
        .grammar
        .new_node("resources")
        .rules([
            is(keyword("resources")).commit(),
            is(token("{")),
            // while_(text()).set(local("resources")),
            is(token("}")),
        ])
        .variables([list_var("resources")])
        .build();

    let scheduler = parser
        .grammar
        .new_node("scheduler")
        .rules([
            is(keyword("scheduler")).commit(),
            is(ident.clone()).set(IDENTIFIER),
            is(token("{")),
            maybe(resources).set(local("resources")),
            maybe(systems).set(local("systems")),
            is(token("}")),
        ])
        .variables([IDENTIFIER_VAR, node_var("resources"), node_var("systems")])
        .build();

    let tls = parser
        .grammar
        .new_enum("top level statements")
        .options([scheduler])
        .build();

    parser
        .grammar
        .new_node("entry")
        .rules([loop_().then([is_one_of([
            option(tls).set(local("top level statements")),
            option(eof()).return_node(),
        ])])])
        .variables([list_var("top level statements")])
        .build();
    parser.parser.entry = Some("entry");

    let valid_result = Validator::default().validate(&parser);
    if !valid_result.success() {
        valid_result.print_all().unwrap();
        panic!();
    }

    parser
}
