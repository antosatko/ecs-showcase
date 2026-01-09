use std::path::PathBuf;

mod grammar;

const TXT: &'static str = include_str!("lang");

fn main() {
    let parser = grammar::gen_parser();

    let tokens = match parser.lexer.lex_utf8(TXT) {
        Ok(r) => r,
        Err(err) => {
            err.print(TXT, Some("./src/lang")).unwrap();
            return;
        }
    };

    if false {
        println!("{tokens:?}");
    }

    let result = parser.parse(&tokens, TXT);
    match result {
        Ok(_result) => {
            println!("allgood");
        }
        Err(err) => {
            err.print(TXT, PathBuf::from("./src/lang").to_str())
                .unwrap();
        }
    }
}
