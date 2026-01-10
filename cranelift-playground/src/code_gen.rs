use cranelift::{
    codegen::{Context, ir::Function},
    prelude::*,
};

pub struct CodeGen {
    pub ctx: Context,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            ctx: Context::new(),
        }
    }
}
