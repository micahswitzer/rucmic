use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::TryInto;
use inkwell::{context::Context, module::Module, builder::Builder, values::{
    BasicValue,
    BasicValueEnum,
    IntValue,
    FunctionValue,
    GlobalValue,
    PointerValue
}, types::{BasicType, BasicTypeEnum, FunctionType}, IntPredicate, AddressSpace};
use crate::ast::*;
use std::env::var;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    builder: Builder<'ctx>,
    global_vars: HashMap<String, GlobalValue<'ctx>>,
    local_vars: HashMap<String, PointerValue<'ctx>>,
    fn_val_opt: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    fn get_llvm_type(&self, ast_type: &Type) -> BasicTypeEnum<'ctx> {
        if (match ast_type.base_type { BaseType::Void => true, _ => false }) && ast_type.is_ptr {
            // this is a special case that can't be handled generically
            return self.context.i8_type().ptr_type(AddressSpace::Generic).into();
        }
        let basic_type: BasicTypeEnum = match ast_type.base_type {
            BaseType::Int => self.context.i64_type().into(),
            BaseType::Char => self.context.i8_type().into(),
            // for now this function can't handle non-pointer void types
            BaseType::Void => panic!("invalid type")
        };
        let arr_type = match ast_type.array_size {
            Some(size) => basic_type.array_type(size).into(),
            None => basic_type
        };
        if ast_type.is_ptr {
            arr_type.ptr_type(AddressSpace::Generic).into()
        }
        else {
            arr_type
        }
    }

    fn get_llvm_fn_type(&self, ret_type: &Type, params: &[BasicTypeEnum<'ctx>], is_variac: bool) -> FunctionType<'ctx> {
        if ret_type.base_type == BaseType::Void && !ret_type.is_ptr {
            // handle the special `VoidType` case
            self.context.void_type().fn_type(params, is_variac)
        }
        else {
            self.get_llvm_type(ret_type).fn_type(params, is_variac)
        }
    }

    fn create_entry_block_alloca(&self, var_type: &Type, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder() ;

        let entry = self.fn_val_opt.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.get_llvm_type(var_type), name)
    }

    fn get_var_ptr(&self, var_name: &String) -> Option<PointerValue<'ctx>> {
        match self.global_vars.get(var_name.as_str()) {
            Some(global_var) => Some(global_var.as_pointer_value()),
            None => match self.local_vars.get(var_name.as_str()) {
                Some(local_var) => Some(*local_var),
                None => None
            }
        }
    }

    pub fn new(context: &'ctx Context, mod_name: &str) -> CodeGen<'ctx> {
        CodeGen {
            context,
            module: context.create_module(mod_name),
            builder: context.create_builder(),
            global_vars: HashMap::new(),
            local_vars: HashMap::new(),
            fn_val_opt: None,
        }
    }

    /// Compiles the specified `Program`
    pub fn compile_program(&mut self, program: &Program) -> Result<(), CodeGenError> {
        // generate code for each of the decls
        for decl in program.decls.iter().as_ref() {
            self.compile_decl(decl, true)?;
        }
        Ok(())
    }

    /// Compiles a `Stmt`
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), CodeGenError> {
        match &stmt.node {
            Stmt_::Comp(stmts) => {
                for s in stmts {
                    self.compile_stmt(&s)?;
                }
                Ok(())
            },
            Stmt_::Cond(cond_expr, cond_stmt, else_stmt) => {
                // probs a lot of messy code here
                Ok(())
            }
            Stmt_::Decl(decl) => self.compile_decl(decl, false),
            Stmt_::Expr(expr) => self.compile_expr(expr).and(Ok(())),
            Stmt_::Iter(iter_expr, iter_stmt) => {
                // some code here
                Ok(())
            },
            Stmt_::Ret(opt_expr) => {
                if let Some(expr) = opt_expr {
                    let expr_val = self.compile_expr(&expr)?;
                    self.builder.build_return(Some(&expr_val));
                }
                else {
                    self.builder.build_return(None);
                }
                Ok(())
            }
        }
    }

    /// Compiles a `Decl`
    fn compile_decl(&mut self, decl: &Decl, is_global: bool) -> Result<(), CodeGenError> {
        match &decl.node {
            Decl_::FunDecl(fn_ret_type, fn_name, fn_args, fn_body) => {
                assert!(is_global); // functions may only be declared globally
                if self.module.get_function(fn_name).is_some() {
                    return Err(CodeGenError { span: Some(decl.span), message: String::from("Duplicate function declaration.") });
                }

                let arg_vals = fn_args
                    .iter()
                    .map(|arg| self.get_llvm_type(arg.1.borrow()))
                    .collect::<Vec<BasicTypeEnum>>();

                let fn_type = self.get_llvm_fn_type(
                    fn_ret_type, &arg_vals, false);
                let fn_val = self.module.add_function(
                    fn_name.as_str(), fn_type, None);

                for (i, arg) in fn_val.get_param_iter().enumerate() {
                    arg.set_name(fn_args[i].0.as_str());
                }

                let entry = self.context.append_basic_block(fn_val, "entry");

                self.builder.position_at_end(entry);

                self.fn_val_opt = Some(fn_val);

                self.local_vars.reserve(fn_args.len());

                // so I guess this turns args into local vars, not sure why we can't just use them
                // directly? Maybe SSA is to blame? Optimization takes care of this anyways...
                for (i, arg) in fn_val.get_param_iter().enumerate() {
                    let alloca = self.create_entry_block_alloca(
                        &fn_args[i].1, fn_args[i].0.as_str());
                    self.builder.build_store(alloca, arg);
                    self.local_vars.insert(fn_args[i].0.clone(), alloca);
                }

                // compile the function body
                self.compile_stmt(fn_body)?;

                // no return value for the last return
                if !fn_ret_type.is_ptr && fn_ret_type.base_type == BaseType::Void {
                    self.builder.build_return(None);
                }

                self.local_vars.clear();

                Ok(())
            },
            Decl_::VarDecl(var_type, var_name) => {
                if self.global_vars.contains_key(var_name) || (!is_global && self.local_vars.contains_key(var_name)) {
                    return Err(CodeGenError { span: Some(decl.span), message: String::from("Duplicate variable declaration.") });
                }
                if is_global {
                    let var_val = self.module.add_global(
                        self.get_llvm_type(var_type),
                        None,
                        var_name.as_str());
                    self.global_vars.insert(var_name.to_string(), var_val);
                }
                else {
                    let var_val = self.create_entry_block_alloca(
                        var_type,
                        var_name.as_str());
                    self.local_vars.insert(var_name.to_string(), var_val);
                }
                Ok(())
            }
        }
    }

    /// Compiles an `Expr`
    fn compile_expr(&self, expr: &Expr) -> Result<IntValue<'ctx>, CodeGenError> {
        match &expr.node {
            Expr_::BinOp(op, left, right) => {
                // compile the operands
                let lhs = self.compile_expr(&left)?;
                let rhs = self.compile_expr(&right)?;
                // build the instruction corresponding to the operator
                // the names help when debugging IR, but are not necessary to produce the correct results
                Ok(match op {
                    BinOp::CmpEq => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpcmpeq"),
                    BinOp::CmpGT => self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpcmpgt"),
                    BinOp::CmpGTE => self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "tmpcmpgte"),
                    BinOp::CmpLT => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "tmpcmplt"),
                    BinOp::CmpLTE => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "tmpcmplte"),
                    BinOp::CmpNE => self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "tmpcmpne"),
                    BinOp::Add => self.builder.build_int_add(lhs, rhs, "tmpadd"),
                    BinOp::Div => self.builder.build_int_signed_div(lhs, rhs, "tmpdiv"),
                    BinOp::Mul => self.builder.build_int_mul(lhs, rhs, "tmpmul"),
                    BinOp::Sub => self.builder.build_int_sub(lhs, rhs, "tmpsub"),
                })
            },
            Expr_::Assign(var_name, array_expr, assign_expr) => {
                let assign_val = self.compile_expr(assign_expr)?;
                // get a ptr to the stored variable
                let var = self.get_var_ptr(var_name)
                    .ok_or(CodeGenError { span: Some(expr.span), message: String::from("Undefined variable.") })?;
                // create a store instruction
                self.builder.build_store(var, assign_val);
                // return the original value
                Ok(assign_val)
            },
            Expr_::Literal(value) => Ok(self.context.i64_type().const_int(
                // we have a constant signed integer here (i64), but
                // it must fit into an unsigned integer (u64) to be used by LLVM.
                // I'm pretty confident it will always fit, so this error handling code
                // is probably excessive, a simple `.unwrap()` should really do the trick.
                (*value).try_into().map_err(|_| CodeGenError {
                    span: Some(expr.span),
                    message: String::from("Constant does not fit into a u64.")
                })?, false)),
            Expr_::Var(var_name, _idx_expr) => Ok(self.builder.build_load(
                    self.get_var_ptr(var_name).ok_or(
                        CodeGenError {
                            span: Some(expr.span),
                            message: String::from("Undefined variable.")
                        })?,
                        var_name.as_str())
                    .into_int_value()),
            Expr_::Call(fn_name, args) => {
                match self.get_function(&fn_name) {
                    Some(fun) => {
                        // compile each argument expression
                        let mut compiled_args = Vec::with_capacity(args.len());
                        for arg in args { compiled_args.push(self.compile_expr(arg)?) }
                        // turn them into BasicValEnums
                        let argv: Vec<BasicValueEnum<'ctx>> = compiled_args.iter().by_ref()
                            .map(|&arg| arg.into()).collect();
                        // create the call
                        match self.builder.build_call(fun, &argv, "tmpcall").try_as_basic_value().left() {
                            Some(value) => Ok(value.into_int_value()),
                            // this means we had an improper number of arguments most likely
                            None => Err(CodeGenError {
                                span: Some(expr.span),
                                message: String::from("Invalid call.")
                            })
                        }
                    }
                    None => Err(CodeGenError { span: Some(expr.span), message: String::from("Undefined function.") })
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct CodeGenError {
    message: String,
    span: Option<crate::lexer::Span>,
}

