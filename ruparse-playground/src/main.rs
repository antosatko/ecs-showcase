use std::path::PathBuf;

use crate::ruparse_lowering::module_named;

mod grammar;
mod ruparse_lowering;

pub use lang_ir::ast;

const TXT: &'static str = include_str!("lang.ecs");

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
            let module = module_named("lang", TXT, result.entry).unwrap();

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
                    if let Some(return_type) = return_type {
                        println!("return type: {:?}", return_type.inner);
                    }
                    println!("body len: {}", body.inner.len());
                }
            }
        }
        Err(err) => {
            err.print(TXT, PathBuf::from("./src/lang").to_str())
                .unwrap();
        }
    }
}
