use std::fs::File;
use std::io::Write;

use cranelift::codegen::{Context, verify_function};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::default_libcall_names;
use cranelift::module::{Linkage, Module};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::*;

use cranelift::codegen::ir::UserFuncName;

pub struct NativeGen {
    pub(crate) function_builder_ctx: FunctionBuilderContext,
    pub(crate) ctx: Context,
    pub(crate) module: ObjectModule,
}

impl NativeGen {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("is_pic", "true").unwrap();
        let flags = settings::Flags::new(flag_builder);

        let isa = cranelift::native::builder().unwrap().finish(flags).unwrap();

        let builder = ObjectBuilder::new(isa, "main", default_libcall_names()).unwrap();

        let module = ObjectModule::new(builder);

        Self {
            function_builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
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
    }

    pub fn emit_obj(self, path: &str) {
        let object = self.module.finish();
        let bytes = object.emit().unwrap();

        let mut file = File::create(path).unwrap();
        file.write_all(&bytes).unwrap();
    }
}
pub fn params(p: impl IntoIterator<Item = Type>) -> Vec<AbiParam> {
    p.into_iter().map(|p| AbiParam::new(p)).collect()
}
