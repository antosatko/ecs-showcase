use cranelift::codegen::{Context, verify_function};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{FuncId, default_libcall_names};
use cranelift::module::{Linkage, Module};
use cranelift::prelude::*;

use cranelift::codegen::ir::{Function, UserFuncName};

use crate::Backend;
use crate::jit_runner::print_i32;

pub struct IrGen {
    pub(crate) function_builder_ctx: FunctionBuilderContext,
    pub(crate) ctx: Context,
    pub(crate) module: JITModule,
    pub(crate) print_i32: FuncId,
}

impl IrGen {
    pub fn new() -> Self {
        let mut m_bldr = JITBuilder::new(default_libcall_names()).unwrap();
        m_bldr.symbol("print_i32", print_i32 as *const u8);

        let mut m = JITModule::new(m_bldr);

        let print_i32 = {
            let mut print_sig = m.make_signature();
            print_sig.params.push(AbiParam::new(types::I32));
            print_sig.returns.clear();

            m.declare_function("print_i32", Linkage::Import, &print_sig)
                .unwrap()
        };

        Self {
            function_builder_ctx: FunctionBuilderContext::new(),
            ctx: m.make_context(),
            module: m,
            print_i32,
        }
    }

    pub(crate) fn create_function(
        &mut self,
        name: &str,
        params: Vec<AbiParam>,
        returns: Vec<AbiParam>,
    ) -> FuncId {
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

        f
    }

    pub fn make_main(&mut self) -> FuncId {
        let mut main_sig = self.module.make_signature();
        main_sig.returns.clear();

        let main_func = self
            .module
            .declare_function("main", Linkage::Export, &main_sig)
            .unwrap();
        let func = &mut self.ctx.func;
        func.signature = main_sig;
        func.name = UserFuncName::user(0, main_func.as_u32());

        let mut builder = FunctionBuilder::new(func, &mut self.function_builder_ctx);

        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        let forty_two = builder.ins().iconst(types::I32, 42);

        // Call print_i32
        let callee = self
            .module
            .declare_func_in_func(self.print_i32, builder.func);
        builder.ins().call(callee, &[forty_two]);

        builder.ins().return_(&[]);

        builder.finalize();
        self.module
            .define_function(main_func, &mut self.ctx)
            .unwrap();
        self.ctx.clear();
        main_func
    }

    pub fn run(&mut self, f: FuncId) {
        unsafe {
            type AddFn = unsafe extern "C" fn();
            let code = self.module.get_finalized_function(f);
            let func: AddFn = std::mem::transmute(code);
            func()
        }
    }
}
pub fn params(p: impl IntoIterator<Item = Type>) -> Vec<AbiParam> {
    p.into_iter().map(|p| AbiParam::new(p)).collect()
}

impl Backend for IrGen {
    type Module = JITModule;

    fn module(&mut self) -> &mut Self::Module {
        &mut self.module
    }

    fn context(&mut self) -> &mut Context {
        &mut self.ctx
    }

    fn finalize(&mut self) {
        self.module.finalize_definitions().unwrap();
    }
}
