use ruparse::*;

pub fn gen_parser() -> Parser<'static> {
    let mut parser = Parser::new();

    parser.lexer.add_tokens(
        "+-*/:!<>({[]})=%;&|"
            .split("")
            .chain(["+=", "-=", "*=", "/=", "==", "&&", "||"]),
    );

    parser
}
