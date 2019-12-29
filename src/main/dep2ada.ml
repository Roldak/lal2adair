open Libdependzlang
open Simple_ada

type expr_result = {
  decls : decl list;
  stmts : stmt list;
  expr : Simple_ada.expr;
}

let _counter = ref 0

let unique_anon_id () =
  _counter := !_counter + 1;
  "anon" ^ string_of_int !_counter

let simple_result expr = { decls = []; stmts = []; expr }

let rec abs_param param =
  let pname = Term.p_to_string param in
  let ptype = TypeName "Natural" in
  { pname; ptype }

and abs_body body =
  let body_res = translate_expr (body :> Term.t) in
  {
    decls = body_res.decls;
    stmts = List.append body_res.stmts [ ReturnStmt body_res.expr ];
  }

and translate_abstraction ?(name : string option) (abs : Term.t) :
    Simple_ada.fun_decl =
  let rec extract (t : Term.t) =
    match t with
    | #Abstraction.t as abs ->
        let params, body = extract (Abstraction.f_term abs :> Term.t) in
        (Abstraction.f_ident abs :: params, body)
    | _ -> ([], t)
  in
  let params, body = extract abs in
  {
    fname = (match name with Some x -> x | None -> unique_anon_id ());
    params = List.map params ~f:abs_param;
    rtype = TypeName "Natural";
    body = abs_body body;
  }

and handle_call (f : string) (args : expr list) : expr =
  match (f, args) with
  | "Z", [] -> IntLit 0
  | "S", [ IntLit x ] -> IntLit (x + 1)
  | "S", [ Binop (x, "+", IntLit l) ] -> Binop (x, "+", IntLit (l + 1))
  | "S", [ x ] -> Binop (x, "+", IntLit 1)
  | "add", [ lhs; rhs ] -> Binop (lhs, "+", rhs)
  | "DPMake", [ term; _ ] -> term
  | cst, [] -> Name cst
  | _ -> Call (Name f, args)

and translate_apply (app : Apply.t) : expr_result =
  let app_elems = Apply.p_as_term_array app in
  let called_fun =
    match List.hd_exn app_elems with
    | #Identifier.t as id -> Term.p_to_string id
    | _ -> assert false
  in
  let args = List.tl_exn app_elems in
  let tr_args = List.map args ~f:translate_expr in
  let arg_exprs = List.map tr_args ~f:(fun r -> r.expr) in
  {
    decls = List.concat_map tr_args ~f:(fun r -> r.decls);
    stmts = List.concat_map tr_args ~f:(fun r -> r.stmts);
    expr = handle_call called_fun arg_exprs;
  }

and translate_expr (e : Term.t) : expr_result =
  match e with
  | #Abstraction.t as abs ->
      let fdecl = translate_abstraction abs in
      { decls = [ FunDecl fdecl ]; stmts = []; expr = Name fdecl.fname }
  | #Apply.t as app -> translate_apply app
  | #Identifier.t as id -> simple_result (handle_call (Term.p_to_string id) [])
  | _ -> assert false

and translate_def Ada_ir.Expr.{ fname } term : Simple_ada.fun_decl =
  let name = Libadalang.AdaNode.text fname in
  translate_abstraction ~name term
