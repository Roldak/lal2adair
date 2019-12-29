open Libdependzlang

let ctx = AnalysisContext.create ()

let prelude_unit : AnalysisUnit.t =
  AnalysisContext.get_from_file ctx "prelude.dep"

let prelude = Option.value_exn (AnalysisUnit.root prelude_unit)

let _id name =
  Format.printf "%s" name;
  let res = Option.value_exn (DependzNode.p_make_ident prelude name) in
  Format.printf "%s" (Term.p_to_string res);
  res

let id name = Option.value_exn (DependzNode.p_make_ident prelude name)

let app_impl x y = Option.value_exn (DependzNode.p_make_apply prelude x y)

let app x y = (app_impl (x :> Term.t) (y :> Term.t) :> Apply.t)

let app2 x y z = app (app x y) z

let arr_impl x y = Option.value_exn (DependzNode.p_make_arrow prelude x y)

let _arr x y = (arr_impl (x :> Term.t) (y :> Term.t) :> Arrow.t)

let arr_b_impl x y b =
  Option.value_exn (DependzNode.p_make_arrow prelude x y ~t3:b)

let arr_b x y b =
  (arr_b_impl (x :> Term.t) (y :> Term.t) (b :> Term.t) :> Arrow.t)

let abs_impl x y = Option.value_exn (DependzNode.p_make_abstraction prelude x y)

let abs x y = (abs_impl (x :> Identifier.t) (y :> Term.t) :> Abstraction.t)

let compile_expr_impl (f : Ada_ir.Expr.t -> Term.t) (e : Ada_ir.Expr.t) : Term.t
    =
  match e.node with
  | Name (Var (Source { vname })) ->
      let str = Libadalang.AdaNode.text vname in
      (id str :> Term.t)
  | Binop (Plus, l, r) -> (app2 (id "add") (f l) (f r) :> Term.t)
  | Const (Int n) ->
      let rec aux x =
        match x with
        | x when x > 0 -> (app (id "S") (aux (x - 1)) :> Term.t)
        | 0 -> (id "Z" :> Term.t)
        | _ -> assert false
      in
      aux (Ada_ir.Int_lit.to_int n)
  | _ ->
      Format.printf "%a" Ada_ir.Expr.pp e;
      assert false

let rec compile_expr e = compile_expr_impl compile_expr e

let subst_post_expr_impl (info : Ada_ir.Expr.funinfo)
    (f : Ada_ir.Expr.t -> Term.t) (e : Ada_ir.Expr.t) =
  match e.node with
  | Name (AttributeRef (Result g)) ->
      if Libadalang.Name.equal info.fname g.fname then (id "_res" :> Term.t)
      else assert false
  | _ -> f e

let rec subst_post_expr info e =
  subst_post_expr_impl info (compile_expr_impl (subst_post_expr info)) e

let rec compile_post_expr_impl (f : Ada_ir.Expr.t -> Term.t) (e : Ada_ir.Expr.t)
    =
  match e.node with
  | Binop (Eq, l, r) -> (app2 (id "Eq") (f l) (f r) :> Term.t)
  | Binop (Lt, l, r) -> (app2 (id "Lt") (f l) (f r) :> Term.t)
  | Binop (Gt, l, r) -> (app2 (id "Lt") (f r) (f l) :> Term.t)
  | Binop (And, l, r) ->
      ( app2 (id "Pair")
          (compile_post_expr_impl f l)
          (compile_post_expr_impl f r)
        :> Term.t )
  | _ -> assert false

let compile_post_expr (info : Ada_ir.Expr.funinfo) e =
  compile_post_expr_impl (subst_post_expr info) e

let compile_type (_typ : Ada_ir.Typ.t) : Term.t = (id "Nat" :> Term.t)

let gen_signature (info : Ada_ir.Expr.funinfo) : Term.t =
  let compiled_type type_expr =
    compile_type
      (Option.value_exn (Libadalang.TypeExpr.p_designated_type_decl type_expr))
  in
  let bd = Option.value_exn (Libadalang.DefiningName.p_basic_decl info.fname) in
  let spec = Option.value_exn (Libadalang.BasicDecl.p_subp_spec_or_null bd) in
  let rtype =
    compiled_type (Option.value_exn (Libadalang.BaseSubpSpec.p_returns spec))
  in
  let post_asp = Libadalang.BasicDecl.p_get_aspect bd "Post" in
  let actual_rtype =
    match post_asp.value with
    | Some (#Libadalang.Expr.t as post_expr) ->
        let ir_post = Translation.Translate_expr.translate_expr post_expr in
        let depz_post = compile_post_expr info ir_post in
        (app2 (id "DPair") rtype (abs (id "_res") depz_post) :> Term.t)
    | _ -> rtype
  in
  let rec gen_arrow params =
    match params with
    | [] -> actual_rtype
    | p :: ps ->
        let name = Option.value_exn (Libadalang.BasicDecl.p_defining_name p) in
        let binder = id (Libadalang.AdaNode.text name) in
        let param_type = compiled_type (Libadalang.ParamSpec.f_type_expr p) in
        (arr_b param_type (gen_arrow ps) binder :> Term.t)
  in
  gen_arrow (Libadalang.BaseSubpSpec.p_params spec)
