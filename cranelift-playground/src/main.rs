use cranelift::{codegen::Context, module::Module, prelude::types};
use ruparse_playground::{grammar::gen_parser, ir};

use crate::jit_bldr::{IrGen, params};

mod code_gen;
mod jit_bldr;
mod jit_runner;
mod native_bldr;

const SRC: &'static str = include_str!("lang");

fn main() {
    let mut ctx = IrGen::new();

    let parser = gen_parser();

    let tok = match parser.lexer.lex_utf8(SRC) {
        Ok(t) => t,
        Err(e) => {
            e.print(SRC, Some("lang")).unwrap();
            panic!()
        }
    };

    let module = match parser.parse(&tok, SRC) {
        Ok(a) => ir::Module::named("lang", SRC, &a.entry.into()),
        Err(e) => {
            e.print(SRC, Some("lang")).unwrap();
            panic!()
        }
    };

    let funs = ctx.add_module(&module);
    ctx.finalize();

    for (ident, func) in funs {
        println!("{ident}() => {}", ctx.run(func));
    }
}

pub trait Backend {
    type Module: Module;

    fn module(&mut self) -> &mut Self::Module;
    fn context(&mut self) -> &mut Context;
    fn finalize(&mut self);
}
