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

let pp_sep_list sep pp =
  let pp_sep fmt () = Format.fprintf fmt "%s@ " sep in
  Format.pp_print_list ~pp_sep pp

let rec pp_fun_decl fmt { fname; params; rtype; body } =
  Format.fprintf fmt "@[<v>function %s (%a) return %a is@,%a@]" fname
    (pp_sep_list ";" pp_param) params pp_type_expr rtype pp_decl_block body

and pp_param fmt { pname; ptype } =
  Format.fprintf fmt "@[%s@ :@ %a@]" pname pp_type_expr ptype

and pp_decl_block fmt { decls; stmts } =
  Format.fprintf fmt "@[<v>declare@[<v 3>%a@]@,@[<v 3>begin@,%a@]@,end;@]"
    (Format.pp_print_list pp_decl)
    decls
    (Format.pp_print_list pp_stmt)
    stmts

and pp_decl fmt decl = match decl with FunDecl fd -> pp_fun_decl fmt fd

and pp_stmt fmt stmt =
  match stmt with
  | ReturnStmt e -> Format.fprintf fmt "@[<h 3>return %a;@]" pp_expr e

and pp_expr fmt expr =
  match expr with
  | Name n -> Format.fprintf fmt "%s" n
  | Binop (lhs, op, rhs) ->
      Format.fprintf fmt "@[<h 3>%a@ %s@ %a@]" pp_expr lhs op pp_expr rhs
  | Call (f, args) ->
      Format.fprintf fmt "@[<h 3>%a@ (%a)@]" pp_expr f (pp_sep_list "," pp_expr)
        args
  | IntLit i -> Format.fprintf fmt "%i" i

and pp_type_expr fmt type_expr =
  match type_expr with TypeName n -> Format.fprintf fmt "%s" n
