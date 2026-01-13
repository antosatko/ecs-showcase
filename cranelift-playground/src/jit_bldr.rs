use core::panic;
use std::collections::HashMap;

use cranelift::codegen::{Context, verify_function};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{FuncId, default_libcall_names};
use cranelift::module::{Linkage, Module};
use cranelift::prelude::*;

use cranelift::codegen::ir::{Function, UserFuncName};
use smol_str::SmolStr;

use crate::Backend;
use crate::jit_runner::print_i32;
use ruparse_playground::ir::{self as lang_ir, Operator, Source};

pub struct IrGen {
    pub(crate) function_builder_ctx: FunctionBuilderContext,
    pub(crate) ctx: Context,
    pub(crate) module: JITModule,
}

pub type Functions = HashMap<SmolStr, FuncId>;

impl IrGen {
    pub fn new() -> Self {
        let m_bldr = JITBuilder::new(default_libcall_names()).unwrap();
        let m = JITModule::new(m_bldr);

        Self {
            function_builder_ctx: FunctionBuilderContext::new(),
            ctx: m.make_context(),
            module: m,
        }
    }

    pub fn add_module(&mut self, module: &lang_ir::Module) -> Functions {
        let mut functions = HashMap::new();
        for object in module.objects.iter() {
            match &object.inner {
                lang_ir::Object::Function(fun) => {
                    functions.insert(fun.ident.inner.clone(), self.lower_function(fun))
                }
                _ => todo!(),
            };
        }
        functions
    }

    pub fn val_as_int(val: &lang_ir::Value) -> i128 {
        if let lang_ir::Literal::Number(lang_ir::Number {
            value: lang_ir::NumberValue::Int(n),
            ..
        }) = val.literal.inner
        {
            n
        } else if let lang_ir::Literal::Number(lang_ir::Number {
            value: lang_ir::NumberValue::Number(n),
            ..
        }) = val.literal.inner
        {
            n as i128
        } else {
            panic!()
        }
    }

    pub fn lower_expr(
        &mut self,
        expr: &lang_ir::Expression,
        bldr: &mut FunctionBuilder,
        vars: &HashMap<SmolStr, Variable>,
    ) -> Value {
        match expr {
            lang_ir::Expression::Value(value) => bldr
                .ins()
                .iconst(types::I32, Self::val_as_int(&value) as i64),
            lang_ir::Expression::Binary { l, r, op } => {
                let lhs = self.lower_expr(l, bldr, vars);
                let rhs = self.lower_expr(r, bldr, vars);
                match &op.inner {
                    lang_ir::Operator::Add => bldr.ins().iadd(lhs, rhs),
                    lang_ir::Operator::Sub => bldr.ins().isub(lhs, rhs),
                    lang_ir::Operator::Mul => bldr.ins().imul(lhs, rhs),
                    _ => todo!(),
                }
            }
        }
    }

    fn lower_block(
        &mut self,
        block: &lang_ir::Block,
        bldr: &mut FunctionBuilder,
        vars: &mut HashMap<SmolStr, Variable>,
        loops: &mut Vec<(Option<SmolStr>, LoopCtx)>,
    ) -> bool {
        for statement in &block.statements {
            if self.lower_statement(bldr, vars, loops, statement) {
                return true;
            }
        }
        false
    }

    fn lower_statement(
        &mut self,
        bldr: &mut FunctionBuilder<'_>,
        vars: &mut HashMap<SmolStr, Variable>,
        loops: &mut Vec<(Option<SmolStr>, LoopCtx)>,
        statement: &Source<lang_ir::Statement>,
    ) -> bool {
        match &statement.inner {
            lang_ir::Statement::Var {
                ident,
                ty,
                expression,
            } => {
                let val = self.lower_expr(expression.as_ref().unwrap(), bldr, &vars);
                let var = bldr.declare_var(types::I32);
                bldr.def_var(var, val);
                vars.insert(ident.inner.clone().into(), var);
                false
            }
            lang_ir::Statement::Return { expression } => {
                let val = self.lower_expr(&expression.inner, bldr, &vars);
                bldr.ins().return_(&[val]);
                true
            }
            lang_ir::Statement::Loop { label, body } => {
                let header = bldr.create_block();
                let body_block = bldr.create_block();
                let exit = bldr.create_block();

                bldr.ins().jump(header, []);

                bldr.switch_to_block(header);
                bldr.ins().jump(body_block, []);

                loops.push((
                    label.clone().map(|v| v.inner),
                    LoopCtx {
                        continue_block: header,
                        break_block: exit,
                    },
                ));

                bldr.switch_to_block(body_block);
                let terminated = self.lower_block(body, bldr, vars, loops);

                if !terminated {
                    bldr.ins().jump(header, []);
                }
                bldr.seal_block(header);
                bldr.seal_block(body_block);
                bldr.seal_block(exit);

                loops.pop();

                bldr.switch_to_block(exit);
                false
            }
            lang_ir::Statement::Break => {
                let ctx = loops.last().expect("break outside loop");
                bldr.ins().jump(ctx.1.break_block, []);
                true
            }
            lang_ir::Statement::Continue => {
                let ctx = loops.last().expect("break outside loop");
                bldr.ins().jump(ctx.1.continue_block, []);
                true
            }
            _ => false,
        }
    }

    pub(crate) fn lower_function(&mut self, fun: &lang_ir::Function) -> FuncId {
        let int32 = types::I32;
        let mut ctx = self.module.make_context();
        ctx.func.signature.params.clear();
        ctx.func.signature.returns = vec![AbiParam::new(int32)];

        let mut fn_ctx = FunctionBuilderContext::new();
        let mut bldr = FunctionBuilder::new(&mut ctx.func, &mut fn_ctx);

        let entry = bldr.create_block();
        bldr.append_block_params_for_function_params(entry);
        bldr.switch_to_block(entry);
        bldr.seal_block(entry);

        let mut vars: HashMap<SmolStr, Variable> = HashMap::new();
        let mut loops: Vec<(Option<SmolStr>, LoopCtx)> = Vec::new();
        let returned = self.lower_block(&fun.body.inner, &mut bldr, &mut vars, &mut loops);
        if !returned {
            let zero = bldr.ins().iconst(types::I32, 0);
            bldr.ins().return_(&[zero]);
        }
        bldr.seal_all_blocks();
        bldr.finalize();
        println!("=== Cranelift IR for {} ===", fun.ident.inner);
        println!("{}", ctx.func.display());

        let id = self
            .module
            .declare_function(&fun.ident, Linkage::Local, &ctx.func.signature)
            .unwrap();
        verify_function(&ctx.func, self.module.isa()).unwrap();
        self.module.define_function(id, &mut ctx).unwrap();
        self.module.clear_context(&mut ctx);
        id
    }

    pub fn run(&mut self, f: FuncId) -> i32 {
        unsafe {
            type AddFn = unsafe extern "C" fn() -> i32;
            let code = self.module.get_finalized_function(f);
            let func: AddFn = std::mem::transmute(code);
            func()
        }
    }
}
pub fn params(p: impl IntoIterator<Item = Type>) -> Vec<AbiParam> {
    p.into_iter().map(|p| AbiParam::new(p)).collect()
}
struct LoopCtx {
    continue_block: Block,
    break_block: Block,
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
