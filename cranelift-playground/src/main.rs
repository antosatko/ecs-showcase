use cranelift::prelude::types;

use crate::ir_gen::{IrGen, params};

mod code_gen;
mod ir_gen;

fn main() {
    let mut ctx = IrGen::new();
    let function = ctx.create_function("danda", params([types::I32]), params([types::I32]));
}
