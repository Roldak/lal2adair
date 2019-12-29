open Libdependzlang

type expr_result = {
  decls : Simple_ada.decl list;
  stmts : Simple_ada.stmt list;
  expr : Simple_ada.expr;
}

val translate_expr : Term.t -> expr_result

val translate_def : Ada_ir.Expr.funinfo -> Term.t -> Simple_ada.fun_decl
