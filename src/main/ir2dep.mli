open Libdependzlang

val compile_expr : Ada_ir.Expr.t -> Term.t

val compile_post_expr : Ada_ir.Expr.funinfo -> Ada_ir.Expr.t -> Term.t

val gen_signature : Ada_ir.Expr.funinfo -> Term.t
