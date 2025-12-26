use ruparse::Parser;
use ruparse::api::ext::*;
use ruparse::grammar::validator::Validator;
use ruparse::grammar::{Enumerator, MatchToken, Node, VarKind, VariableKind};

const IDENTIFIER: VarKind<'static> = local("identifier");
const IDENTIFIER_VAR: (&'static str, VariableKind) = ("identifier", VariableKind::Node);
const END_STATEMATE: MatchToken<'static> = MatchToken::Node("end statemate");

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

    parser.grammar.add_enum(Enumerator {
        name: "keywords",
        values: KEYWORDS.iter().map(|kw| word(kw)).collect(),
    });

    parser
        .grammar
        .new_node("identifier")
        .rules([isnt(enumerator("keywords")), is(text()).set(IDENTIFIER)])
        .variables([node_var("identifier")])
        .build();

    parser.grammar.add_node(Node {
        name: "identifier",
        rules: rules([isnt(enumerator("keywords")), is(text()).set(IDENTIFIER)]),
        variables: [("identifier", VariableKind::Node)].to_vec(),
        docs: None,
    });

    parser.grammar.add_node(Node {
        name: "end statement",
        rules: rules([is_one_of(options([option(token(";")), option(newline())]))]),
        variables: [].to_vec(),
        docs: None,
    });

    parser.grammar.add_node(Node {
        name: "systems",
        rules: rules([
            is(keyword("systems")).commit(),
            is(token("{")),
            while_(node("identifier")).set(local("systems")),
            is(token("}")),
        ]),
        variables: [("systems", VariableKind::NodeList)].to_vec(),
        docs: None,
    });

    parser.grammar.add_node(Node {
        name: "resources",
        rules: rules([
            is(keyword("resources")).commit(),
            is(token("{")),
            // while_(text()).set(local("resources")),
            is(token("}")),
        ]),
        variables: [("resources", VariableKind::NodeList)].to_vec(),
        docs: None,
    });

    parser.grammar.add_node(Node {
        name: "scheduler",
        rules: rules([
            is(keyword("scheduler")).commit(),
            is(node("identifier")).set(IDENTIFIER),
            is(token("{")),
            maybe(node("resources")).set(local("resources")),
            maybe(node("systems")).set(local("systems")),
            is(token("}")),
        ]),
        variables: [
            IDENTIFIER_VAR,
            ("resources", VariableKind::Node),
            ("systems", VariableKind::Node),
        ]
        .to_vec(),
        docs: None,
    });

    parser.grammar.add_enum(Enumerator {
        name: "top level statements",
        values: [node("scheduler")].to_vec(),
    });

    parser.grammar.add_node(Node {
        name: "entry",
        rules: rules([loop_().then([is_one_of(options([
            option(enumerator("top level statements")).set(local("top level statements")),
            option(eof()).return_node(),
        ]))])]),
        variables: [("top level statements", VariableKind::NodeList)].to_vec(),
        docs: None,
    });
    parser.parser.entry = Some("entry");

    let valid_result = Validator::default().validate(&parser);
    if !valid_result.success() {
        valid_result.print_all().unwrap();
        panic!();
    }

    parser
}
