type fun_decl = {
  fname : string;
  params : param list;
  rtype : type_expr;
  body : decl_block;
}

and param = { pname : string; ptype : type_expr }

and decl_block = { decls : decl list; stmts : stmt list }

and decl = FunDecl of fun_decl

and stmt = ReturnStmt of expr

and expr =
  | Name of string
  | Binop of expr * string * expr
  | Call of expr * expr list
  | IntLit of int

and type_expr = TypeName of string

val pp_fun_decl : Format.formatter -> fun_decl -> unit

val pp_param : Format.formatter -> param -> unit

val pp_decl_block : Format.formatter -> decl_block -> unit

val pp_decl : Format.formatter -> decl -> unit

val pp_stmt : Format.formatter -> stmt -> unit

val pp_expr : Format.formatter -> expr -> unit

val pp_type_expr : Format.formatter -> type_expr -> unit
