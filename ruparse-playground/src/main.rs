mod grammar;

const TXT: &'static str = include_str!("lang");

fn main() {
    let parser = grammar::gen_parser();

    let tokens = parser.lexer.lex_utf8(TXT).unwrap();
    let result = parser.parse(&tokens, TXT);
    match result {
        Ok(_result) => {
            println!("allgood");
        }
        Err(err) => {
            err.print(TXT, Some("test.lang")).unwrap();
        }
    }
}
