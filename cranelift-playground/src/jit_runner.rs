use cranelift::jit::JITModule;

use crate::jit_bldr::IrGen;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_i32(x: i32) {
    println!("{}", x);
}

pub struct Runner {
    pub module: JITModule,
}

impl Runner {
    pub fn new(module: JITModule) -> Self {
        Self { module }
    }

    pub fn run(&mut self) {}
}
