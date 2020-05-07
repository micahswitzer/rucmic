use std::collections::HashMap;
use std::convert::TryInto;
use inkwell::{
    context::Context,
    module::Module,
    builder::Builder,
    values::{
        BasicValue,
        BasicValueEnum,
        IntValue,
        FunctionValue,
        PointerValue
    },
    types::{ BasicType, BasicTypeEnum, AnyTypeEnum },
    IntPredicate,
};
use crate::ast::*;

// TODO: keep track of register numbers?
pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    //functions: HashMap<String, >
}

impl<'ctx> CodeGen<'ctx> {
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    #[inline]
    fn get_llvm_type(&self, ast_type: Type, arr_size: Option<u32>) -> BasicTypeEnum {
        let basic_type: BasicTypeEnum = match ast_type {
            Type::Int => self.context.i64_type().into(),
        };
        match arr_size {
            Some(size) => basic_type.array_type(size).into(),
            None => basic_type
        }
    }

    pub fn new(context: &'ctx Context, mod_name: &str) -> CodeGen<'ctx> {
        CodeGen {
            context,
            module: context.create_module(mod_name),
            builder: context.create_builder(),
            variables: HashMap::new(),
        }
    }

    /// Compiles the specified `Program`
    pub fn compile_program(&self, program: &Program) -> Result<(), CodeGenError> {
        // generate code for each of the decls
        for decl in program.decls {
            self.compile_decl(&decl, true)?;
        }
        Ok(())
    }

    /// Compiles a `Stmt`
    fn compile_stmt(&self, stmt: &Stmt) -> Result<(), CodeGenError> {
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
                    // there's a return value
                    self.compile_expr(&expr)?;
                }
                else {
                    // no return value
                }
                Ok(())
            }
        }
    }

    /// Compiles a `Decl`
    fn compile_decl(&self, decl: &Decl, is_global: bool) -> Result<(), CodeGenError> {
        match &decl.node {
            Decl_::FunDecl(fn_type, fn_name, fn_args, fn_body) => {
                assert!(is_global); // functions may only be declared globally
                if self.module.get_function(fn_name).is_some() {
                    return Err(CodeGenError { span: Some(decl.span), message: String::from("Duplicate function declaration.") });
                }

                let arg_types = fn_args.iter().as_ref().map(|arg| self.context.i64_type()).collect::<Vec<BasicTypesEnum>>;

                let fn_val = match fn_type {
                    Type::Int => self.context.i64_type().fn_type(arg_types)
                }
                
                let ret_type = self.get_llvm_type(*fn_type);
                ret_type.

                // compile the function body
                self.compile_stmt(fn_body)?;

                Ok(())
            },
            Decl_::VarDecl(var_name, var_size) => {
                if self.variables.contains_key(var_name) {
                    return Err(CodeGenError { span: Some(decl.span), message: String::from("Duplicate variable declaration.") });
                }
                self.create_entry_block
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
                let var = self.variables.get(var_name.as_str()).ok_or(
                    CodeGenError { span: Some(expr.span), message: String::from("Undefined variable.") })?;
                // create a store instruction
                self.builder.build_store(*var, assign_val);
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
            Expr_::Var(var_name, _idx_expr) => match self.variables.get(var_name.as_str()) {
                Some(var) => Ok(self.builder.build_load(*var, var_name.as_str()).into_int_value()),
                None => Err(CodeGenError { span: Some(expr.span), message: String::from("Undefined variable.") })
            },
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
