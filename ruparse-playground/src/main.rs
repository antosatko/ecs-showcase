use std::path::PathBuf;

use ruparse::parser::Nodes;

use crate::ruparse_lowering::module_named;

mod grammar;
mod ir;
mod ruparse_lowering;

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
        Ok(result) => {
            let module = module_named("lang", TXT, &Nodes::Node(result.entry));

            println!();
            for doc in module.docs {
                println!("{}", doc.inner);
            }
            println!("IR for module {}", module.name);

            for symbol in module.symbols {
                println!("symbol: {symbol:?}");
            }

            for fun in module.objects.iter() {
                if let lang_ir::ast::Object::Function(lang_ir::ast::Function {
                    ident,
                    parameters,
                    return_type,
                    body,
                    docs,
                }) = &fun.inner
                {
                    println!();
                    for doc in docs {
                        println!("{}", doc.inner);
                    }
                    println!("function {}", ident.inner);
                    println!("param count: {}", parameters.len());
                    println!("has return type: {}", return_type.is_some());
                    println!("body len: {}", body.inner.statements.len());
                }
            }
        }
        Err(err) => {
            err.print(TXT, PathBuf::from("./src/lang").to_str())
                .unwrap();
        }
    }
}
