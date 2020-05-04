use inkwell::{ context::Context, module::Module, builder::Builder };
use crate::ast::*;

pub struct CodeGen<'ctx> {
    context: Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(mod_name: &str) -> CodeGen<'ctx> {
        let context = Context::create();
        CodeGen {
            context,
            module: context.create_module(mod_name),
            builder: context.create_builder(),
        }
    }

    pub fn gen(&self, program: &Program) -> Result<(), &str> {

        Result::Ok(())
    }
}

pub trait GenCode {
    fn gencode(context: &CodeGen);
}
