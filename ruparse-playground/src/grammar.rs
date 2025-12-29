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
    "function",
    "return",
    "if",
    "else",
    "loop",
    "while",
    "break",
    "continue",
];

fn keyword(kw: &'static str) -> MatchToken<'static> {
    if !KEYWORDS.contains(&kw) {
        panic!("{kw} is not a keyword");
    }
    word(kw)
}

pub fn gen_parser() -> Parser<'static> {
    let mut parser = Parser::new();

    parser.lexer.add_tokens(
        "+ - * / ; \" ' : :: ( { [ < > ] } ) | & ! ? = . , == != += -= *= /= %= && || >= <="
            .split_whitespace(),
    );

    let keywords = parser
        .grammar
        .new_enum("keyword")
        .options(KEYWORDS.iter().map(|kw| word(kw)))
        .build();

    let operators = parser
        .grammar
        .new_enum("operator")
        .options(
            "+ - * / == != < > <= >= || && += -= *= /= %= ="
                .split_whitespace()
                .map(|t| token(t)),
        )
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

    let ident_path = parser
        .grammar
        .new_node("identifier path")
        .rules([
            is(ident.clone()).set(local("path")).commit(),
            while_(token("::")).then([is(ident.clone())
                .set(local("path"))
                .hint("Static path must end on an identifier")]),
        ])
        .variables([list_var("path")])
        .build();

    let literals = parser
        .grammar
        .new_enum("literal")
        .options([ident_path.clone()])
        .build();

    let value = parser
        .grammar
        .new_node("value")
        .rules([is(literals.clone()).set(local("literal")).commit()])
        .variables([node_var("literal")])
        .build();

    let expression = parser
        .grammar
        .new_node("expression")
        .rules([
            is(value.clone()).set(local("value")).commit(),
            maybe(operators.clone())
                .set(local("operator"))
                .then([is(node("expression")).set(local("right value"))]),
        ])
        .variables([
            node_var("value"),
            node_var("operator"),
            node_var("right value"),
        ])
        .build();

    let end_stmt = parser
        .grammar
        .new_node("end statement")
        .rules([is_one_of([option(newline()), option(token(";"))])
            .hint("Expected to end statement on a new line or semicolon")])
        .build();

    let type_ = parser
        .grammar
        .new_node("type")
        .rules([is(ident_path).commit().set(local("path"))])
        .variables([node_var("path")])
        .build();

    let label = parser
        .grammar
        .new_node("label")
        .rules([
            is(token("'")),
            is(ident.clone()).set(IDENTIFIER),
            is(token(":")),
        ])
        .variables([IDENTIFIER_VAR])
        .build();

    let parameter = parser
        .grammar
        .new_node("parameter")
        .rules([
            is(ident.clone()).set(local("identifier")).commit(),
            is(token(":")),
            is(type_.clone()).set(local("type")),
        ])
        .variables([node_var("identifier"), node_var("type")])
        .build();

    let parameter_list = parser
        .grammar
        .new_node("parameters")
        .rules([
            is(token("(")).commit(),
            maybe(parameter.clone()).then([while_(token(",")).then([is_one_of([
                option(parameter.clone()).set(local("parameters")),
                option(token(")")).return_node(),
            ])])]),
            is(token(")")),
        ])
        .variables([list_var("parameters")])
        .build();

    let expression_st = parser
        .grammar
        .new_node("expression statement")
        .rules([
            is(expression.clone()).set(local("expression")).commit(),
            is(end_stmt.clone()),
        ])
        .variables([node_var("expression")])
        .build();

    let variable_st = parser
        .grammar
        .new_node("variable")
        .rules([
            is(keyword("var")).commit(),
            is(ident.clone()).set(IDENTIFIER),
            maybe(token(":")).then([is(type_.clone()).set(local("type"))]),
            maybe(token("=")).then([is(expression.clone()).set(local("expression"))]),
            is(end_stmt.clone()),
        ])
        .variables([IDENTIFIER_VAR, node_var("type"), node_var("expression")])
        .build();

    let return_st = parser
        .grammar
        .new_node("return")
        .rules([
            is(keyword("return")).commit(),
            is(expression.clone()).set(local("expression")),
            is(end_stmt),
        ])
        .variables([node_var("expression")])
        .build();

    let continue_st = parser
        .grammar
        .new_node("continue")
        .rules([is(keyword("continue"))])
        .build();

    let break_st = parser
        .grammar
        .new_node("break")
        .rules([is(keyword("break"))])
        .build();

    let code_block = parser
        .grammar
        .new_node("code block")
        .rules([
            is(token("{")).commit(),
            loop_().then([is_one_of([
                option(enumerator("statement")).set(local("statements")),
                option(token("}")).return_node(),
            ])
            .hint("Potentially unclosed code block")]),
        ])
        .variables([node_var("statements")])
        .build();

    let loop_st = parser
        .grammar
        .new_node("loop")
        .rules([
            maybe(label.clone()).set(local("label")),
            is(keyword("loop")).commit(),
            is(code_block.clone()).set(local("code block")),
        ])
        .variables([node_var("label"), node_var("code block")])
        .build();

    // references
    let _ = code_block;
    let _statements = parser
        .grammar
        .new_enum("statement")
        .options([
            expression_st.clone(),
            variable_st.clone(),
            return_st.clone(),
            continue_st.clone(),
            break_st.clone(),
            loop_st.clone(),
        ])
        .build();

    let function = parser
        .grammar
        .new_node("function")
        .rules([
            is(keyword("function")).commit(),
            is(ident.clone()).set(IDENTIFIER),
            is(parameter_list.clone()).set(local("parameters")),
            maybe(token(":")).then([is(type_.clone()).set(local("return type"))]),
            is(code_block.clone()).set(local("code block")),
        ])
        .variables([
            IDENTIFIER_VAR,
            node_var("parameters"),
            node_var("return type"),
            node_var("code block"),
        ])
        .build();

    let systems = parser
        .grammar
        .new_node("systems")
        .rules([
            is(keyword("systems")).commit(),
            is(token("{")),
            while_(ident.clone()).set(local("systems")),
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
            while_(value.clone()).set(local("resources")),
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
        .new_enum("top level statement")
        .options([scheduler, function])
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
