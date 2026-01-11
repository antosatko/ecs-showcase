use cranelift::{codegen::Context, module::Module, prelude::types};

use crate::jit_bldr::{IrGen, params};

mod code_gen;
mod jit_bldr;
mod jit_runner;
mod native_bldr;

fn main() {
    let mut ctx = IrGen::new();
    let function = ctx.create_function("danda", params([types::I32]), params([types::I32]));
    let main = ctx.make_main();
    ctx.finalize();
    ctx.run(main);
}

pub trait Backend {
    type Module: Module;

    fn module(&mut self) -> &mut Self::Module;
    fn context(&mut self) -> &mut Context;
    fn finalize(&mut self);
}
