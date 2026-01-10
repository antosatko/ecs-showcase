use cranelift::codegen::{Context, verify_function};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::default_libcall_names;
use cranelift::module::{Linkage, Module};
use cranelift::prelude::*;

use cranelift::codegen::ir::UserFuncName;

pub struct IrGen {
    pub(crate) function_builder_ctx: FunctionBuilderContext,
    pub(crate) ctx: Context,
    pub(crate) module: JITModule,
}

impl IrGen {
    pub fn new() -> Self {
        let m = JITModule::new(JITBuilder::new(default_libcall_names()).unwrap());
        Self {
            function_builder_ctx: FunctionBuilderContext::new(),
            ctx: m.make_context(),
            module: m,
        }
    }

    pub(crate) fn create_function(
        &mut self,
        name: &str,
        params: Vec<AbiParam>,
        returns: Vec<AbiParam>,
    ) {
        let mut sig = self.module.make_signature();

        sig.returns = returns;
        sig.params = params;

        let f = self
            .module
            .declare_function(name, Linkage::Local, &sig)
            .unwrap();

        let fn_builder_ctx = &mut self.function_builder_ctx;
        let func = &mut self.ctx.func;
        func.name = UserFuncName::user(0, f.as_u32());
        func.signature = sig;

        {
            let mut builder = FunctionBuilder::new(func, fn_builder_ctx);

            let block0 = builder.create_block();
            builder.append_block_params_for_function_params(block0);
            builder.switch_to_block(block0);
            builder.seal_block(block0);

            let x = builder.block_params(block0)[0];
            let two = builder.ins().iconst(types::I32, 2);
            let sum = builder.ins().iadd(x, two);
            builder.ins().return_(&[sum]);

            builder.finalize();
        }

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&func, &flags);
        println!("{}", func);
        if let Err(errors) = res {
            panic!("{}", errors);
        }

        self.module.define_function(f, &mut self.ctx).unwrap();
        self.ctx.clear();
        self.module.finalize_definitions().unwrap();

        unsafe {
            let code = self.module.get_finalized_function(f);
            let func = std::mem::transmute::<_, fn(i32) -> i32>(code);
            println!("func(10) = {}", func(10));
            println!("func(-10) = {}", func(-10));
        }
    }
}
pub fn params(p: impl IntoIterator<Item = Type>) -> Vec<AbiParam> {
    p.into_iter().map(|p| AbiParam::new(p)).collect()
}
