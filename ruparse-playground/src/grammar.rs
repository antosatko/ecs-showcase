use ruparse::Parser;
use ruparse::api::ext::*;
use ruparse::grammar::validator::Validator;
use ruparse::grammar::{Comparison, ErrorDefinition, MatchToken, VarKind, VariableKind};

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

static _ERR: ErrorDefinition = ErrorDefinition {
    header: "------",
    code: "200",
    msg: "-------------",
};

static UNCLOSED_STRING_LIT: ErrorDefinition = ErrorDefinition {
    header: "Unclosed string",
    code: "200",
    msg: "Expected string literal to end before the end of file",
};

static EMPTY_CHAR_LIT: ErrorDefinition = ErrorDefinition {
    header: "Empty character",
    code: "200",
    msg: "Expected character literal to not be empty",
};

fn keyword(kw: &'static str) -> MatchToken<'static> {
    assert!(KEYWORDS.contains(&kw), "{kw} must be a keyword");
    word(kw)
}

pub fn gen_parser() -> Parser<'static> {
    let mut parser = Parser::new();

    parser.lexer.add_tokens(
        "+ - * / \\ ; \" ' : :: ( { [ < > ] } ) | & ! ? = . , == != += -= *= /= %= && || >= <= #"
            .split_whitespace(),
    );
    let escapes_characters_iter = "\\n \\t \\\" \\u \\\\ \\0".split_whitespace();
    parser.lexer.add_tokens(escapes_characters_iter.clone());

    // parser.lexer.preprocessors.push(|src, tokens| {
    //     let mut iter = tokens.iter();
    //     let mut result = Vec::new();

    //     while let Some(token) = iter.next() {
    //         result.push(token.clone());
    //     }

    //     Ok(result)
    // });

    let keywords = parser
        .grammar
        .new_enum("keyword")
        .options(KEYWORDS.iter().map(|kw| word(kw)))
        .build();

    let escapes = parser
        .grammar
        .new_enum("escaped character")
        .options(escapes_characters_iter.map(|kw| token(kw)))
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

    let escaped_character = parser
        .grammar
        .new_node("escaped character")
        .rules([is_one_of([
            option(token("\\u")).commit().then([
                is(token("{")),
                is(text()).set(local("unicode")),
                is(token("}")),
            ]),
            option(escapes).set(local("char")),
        ])])
        .variables([node_var("char"), node_var("unicode")])
        .build();

    let string_literal = {
        let terminating_quote = option(token("\""))
            .end()
            .clone_value(local("delimeters count"), local("terminate delimeters"))
            .then([loop_().then([
                compare(
                    local("zero"),
                    local("terminate delimeters"),
                    Comparison::Equal,
                )
                .then([return_node()]),
                maybe(token("#"))
                    .dec(local("terminate delimeters"))
                    .otherwise([goto("main loop")]),
            ])]);

        parser
            .grammar
            .new_node("string literal")
            .rules([
                while_(token("#")).inc(local("delimeters count")),
                is(token("\"")).commit().start(),
                label("main loop"),
                loop_().then([is_one_of([
                    option(escaped_character.clone()).set(local("tokens")),
                    terminating_quote,
                    option(eof()).fail(&UNCLOSED_STRING_LIT),
                    option(any()).set(local("tokens")),
                ])]),
            ])
            .variables([
                number_var("delimeters count"),
                number_var("terminate delimeters"),
                number_var("zero"),
                list_var("tokens"),
            ])
            .build()
    };

    let char_literal = parser
        .grammar
        .new_node("character literal")
        .rules([
            is(token("'")).commit(),
            is_one_of([
                option(escaped_character.clone()),
                option(whitespace()),
                option(token("'")).fail(&EMPTY_CHAR_LIT),
                option(any()),
            ])
            .set(local("value")),
            is(token("'")).hint("Only one character allowed in character literal"),
        ])
        .variables([node_var("value")])
        .build();

    let array_literal = parser
        .grammar
        .new_node("array literal")
        .rules([
            is(token("[")).commit(),
            maybe(node("expression"))
                .then([
                    loop_().then([maybe(token(",")).then([maybe(node("expression"))
                        .set(local("expressions"))
                        .otherwise([is(token("]")).return_node()])])]),
                ])
                .otherwise([is(token("]"))]),
        ])
        .variables([list_var("expressions")])
        .build();

    let literals = parser
        .grammar
        .new_enum("literal")
        .options([
            ident_path.clone(),
            string_literal.clone(),
            char_literal.clone(),
            array_literal.clone(),
        ])
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
            maybe(token("=")).then([is(expression.clone())
                .set(local("expression"))
                .hint("Variable must be initialized to a valid expression")]),
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
            variable_st.clone(),
            return_st.clone(),
            continue_st.clone(),
            break_st.clone(),
            loop_st.clone(),
            expression_st.clone(),
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
    valid_result.print_all().unwrap();
    if !valid_result.pass() {
        panic!();
    }

    parser
}
