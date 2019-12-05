open Libadalang

let rec translate_expr (expr : Expr.t) : Ada_ir.Expr.t =
  { node=
      ( match%nolazy expr with
      | #ContractCases.t as contract_cases ->
          translate_contract_cases contract_cases
      | `ParenExpr {f_expr} ->
          (translate_expr (f_expr :> Expr.t)).node
      | #UnOp.t as unop ->
          translate_unop unop
      | #BinOp.t as binop ->
          translate_binop binop
      | #MembershipExpr.t as membership_expr ->
          translate_membership_expr membership_expr
      | #BaseAggregate.t as base_aggregate ->
          translate_base_aggregate base_aggregate
      | #Name.t as name ->
          translate_name name
      | #BoxExpr.t as box_expr ->
          translate_box_expr box_expr
      | #IfExpr.t as if_expr ->
          translate_if_expr if_expr
      | #CaseExpr.t as case_expr ->
          translate_case_expr case_expr
      | #CaseExprAlternative.t as case_expr_alternative ->
          translate_case_expr_alternative case_expr_alternative
      | #QuantifiedExpr.t as quantified_expr ->
          translate_quantified_expr quantified_expr
      | #Allocator.t as allocator ->
          translate_allocator allocator
      | #RaiseExpr.t as raise_expr ->
          translate_raise_expr raise_expr )
  ; orig_node= expr
  ; typ= Translate_typ.translate_type_of_expr expr }

and translate_lval (expr : Expr.t) =
  match expr with
  | #Lal_typ.identifier as ident when Lal_typ.is_variable ident ->
      translate_variable ident
  | #DottedName.t as dotted_name when Lal_typ.is_record_access dotted_name ->
      translate_record_access dotted_name
  | _ ->
      Utils.legality_error "Cannot translate %a as an lvalue" Utils.pp_node
        expr

and translate_variable (var : Lal_typ.identifier) =
  let vname = Utils.defining_name var in
  Ada_ir.Expr.(Var {vname}, NoOffset)

and translate_record_access (name : DottedName.t) =
  let prefix = translate_expr (DottedName.f_prefix name :> Expr.t) in
  match (prefix.node, DottedName.f_suffix name) with
  | Lval (lhost, offset), (#Identifier.t as ident) ->
      let fieldname = Utils.defining_name ident in
      (lhost, Field ({fieldname}, offset))
  | _, #Identifier.t ->
      Utils.legality_error "Cannot access a field of a non lvalue: %a"
        Ada_ir.Expr.pp prefix
  | _ ->
      assert false

and translate_contract_cases (_contract_cases : ContractCases.t) = assert false

and translate_unop (_unop : UnOp.t) = assert false

and translate_binop (_binop : BinOp.t) = assert false

and translate_membership_expr (_membership_expr : MembershipExpr.t) =
  assert false

and translate_base_aggregate (_base_aggregate : BaseAggregate.t) = assert false

and translate_name (name : Name.t) : Ada_ir.Expr.expr_node =
  match name with
  | _ when Lal_typ.is_lvalue name ->
      Ada_ir.Expr.Lval (translate_lval (name :> Expr.t))
  | #Lal_typ.literal as literal when Lal_typ.is_literal literal ->
      translate_literal literal
  | #Lal_typ.call as call when Lal_typ.is_call call ->
      translate_call call
  | #AttributeRef.t as attribute_ref ->
      translate_attribute_ref attribute_ref
  | _ ->
      assert false

and translate_literal (literal : Lal_typ.literal) =
  let open Ada_ir.Expr in
  match%nolazy literal with
  | #IntLiteral.t as int_literal ->
      Utils.try_or_undefined "IntLiteral.p_denoted_value"
        (fun n ->
          Const (Int (Ada_ir.Int_lit.of_int (IntLiteral.p_denoted_value n))) )
        int_literal
  | #StringLiteral.t as string_literal ->
      Utils.try_or_undefined "StringLiteral.p_denoted_value"
        (fun n -> Const (String (StringLiteral.p_denoted_value n)))
        string_literal
  | #NullLiteral.t ->
      Const Null
  | #CharLiteral.t as char_lit ->
      (* A char literal is a regular enum. Use p_eval_as_int which correctly
         evaluates the position of the enum *)
      Utils.try_or_undefined "Expr.p_eval_as_int"
        (fun n ->
          let name = AdaNode.text n in
          Const
            (Enum
               { Ada_ir.Enum.name= StdCharLiteral name
               ; pos= Ada_ir.Int_lit.of_int (Expr.p_eval_as_int n) }) )
        char_lit
  | #RealLiteral.t as real_literal ->
      (* Not implemented *)
      Utils.unimplemented real_literal
  | #Lal_typ.identifier as ident ->
      (* Assume it denotes an enum. Otherwise, translate_literal should not
         have been called *)
      assert (Lal_typ.is_literal ident) ;
      let name = Utils.defining_name ident in
      Utils.try_or_undefined "Expr.p_eval_as_int"
        (fun n ->
          Const
            (Enum
               { Ada_ir.Enum.name= EnumLiteral name
               ; pos= Ada_ir.Int_lit.of_int (Expr.p_eval_as_int n) }) )
        ident

and translate_call (call : Lal_typ.call) =
  let funinfo ident = Ada_ir.Expr.{fname= Utils.defining_name ident} in
  let add_self self subp_spec param_actuals =
    (* Assuming we are facing a dot call, add self to the param actuals with
       the good identifier *)
    match%nolazy BaseSubpSpec.p_params subp_spec with
    | `ParamSpec {f_ids= `DefiningNameList {list= first_id :: _}} :: _ ->
        {ParamActual.param= Some first_id; actual= (Some self :> Expr.t option)}
        :: param_actuals
    | _ ->
        Utils.legality_error
          "%a should have at least one parameter for dot call" Utils.pp_node
          subp_spec
  in
  match%nolazy call with
  | `DottedName {f_prefix} as ident when Name.p_is_dot_call call ->
      (* Call to const function with self as arg *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let args = translate_args subp_spec (add_self f_prefix subp_spec []) in
      Ada_ir.Expr.CallExpr (Cfun (funinfo ident), args)
  | #Lal_typ.identifier as ident ->
      (* Call to const function with no args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let args = translate_args subp_spec [] in
      Ada_ir.Expr.CallExpr (Cfun (funinfo ident), args)
  | `CallExpr
      {f_name= `DottedName {f_prefix} as ident; f_suffix= #AssocList.t as args}
    when Lal_typ.is_subprogram ident && Name.p_is_dot_call ident ->
      (* Const dot call with args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let param_actuals =
        add_self f_prefix subp_spec (AssocList.p_zip_with_params args)
      in
      let args = translate_args subp_spec param_actuals in
      Ada_ir.Expr.CallExpr (Cfun (funinfo ident), args)
  | `CallExpr
      { f_name= (#Identifier.t | #DottedName.t) as ident
      ; f_suffix= #AssocList.t as args }
    when Lal_typ.is_subprogram ident ->
      (* Const call with args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let args = translate_args subp_spec (AssocList.p_zip_with_params args) in
      Ada_ir.Expr.CallExpr (Cfun (funinfo ident), args)
  | `ExplicitDeref {f_prefix} ->
      (* Call to non const function with no args *)
      let expr = translate_expr (f_prefix :> Expr.t) in
      Ada_ir.Expr.CallExpr (Pfun expr, [])
  | `CallExpr
      { f_name= `ExplicitDeref {f_prefix= called} | called
      ; f_suffix= #AssocList.t as args } ->
      (* Call to non const function with args and explicit deref,
         or implicit *)
      let subp_spec = Utils.accessed_subp_spec called in
      let expr = translate_expr (called :> Expr.t) in
      let args = translate_args subp_spec (AssocList.p_zip_with_params args) in
      Ada_ir.Expr.CallExpr (Pfun expr, args)
  | `CallExpr {f_suffix} ->
      Utils.legality_error "Args should be an AssocList, found %a"
        Utils.pp_node f_suffix

and translate_args (subp_spec : BaseSubpSpec.t)
    (param_actuals : ParamActual.t list) =
  let module DefiningNameMap = Caml.Map.Make (DefiningName) in
  (* First gather all ids of params in the right order *)
  let params = BaseSubpSpec.p_params subp_spec in
  let formals_with_default =
    let prepend_ids default_expr ids current_formals =
      List.fold_right
        ~f:(fun id acc -> (id, default_expr) :: acc)
        ~init:current_formals ids
    in
    let prepend_param param current_formals =
      let default_expr = (ParamSpec.f_default_expr param :> Expr.t option) in
      let ids = DefiningNameList.f_list (ParamSpec.f_ids param) in
      prepend_ids default_expr ids current_formals
    in
    List.fold_right ~f:prepend_param ~init:[] params
  in
  (* Then fill the map with each position *)
  let pos_map =
    List.foldi
      ~f:(fun i map (id, _) -> DefiningNameMap.add id i map)
      ~init:DefiningNameMap.empty formals_with_default
  in
  let find_pos id =
    match DefiningNameMap.find_opt id pos_map with
    | Some pos ->
        pos
    | None ->
        Utils.lal_error "Cannot find parameter %a in subprogram spec %a"
          Utils.pp_node id Utils.pp_node subp_spec
  in
  (* Then sort the given param_actual according the the pos map *)
  let actuals =
    let compare {ParamActual.param= lparam} {ParamActual.param= rparam} =
      match (lparam, rparam) with
      | Some lparam, Some rparam ->
          Pervasives.compare (find_pos lparam) (find_pos rparam)
      | _, _ ->
          (* Should not happen, the type in lal is not precise enough *)
          Utils.lal_error "Not able to find a parameter"
    in
    List.sort ~compare param_actuals
  in
  (* sorted_param_actuals is now sorted according to the positions of
     params *)
  if List.length formals_with_default < List.length actuals then
    Utils.lal_error
      "Number of parameters between actuals and formals differ for %a"
      Utils.pp_node subp_spec ;
  let rec build_args formals actuals =
    match (formals, actuals) with
    | ( (id_formal, _) :: formals
      , {ParamActual.param= Some id_actual; actual= Some actual} :: actuals )
      when DefiningName.equal id_formal id_actual ->
        (* id_formal and id_actual are the same, translate the actual
           expression *)
        translate_expr actual :: build_args formals actuals
    | (_, Some formal) :: formals, actuals ->
        (* Since actuals are sorted according to params, if id_actual and
           id_formal are different, we should use a default expression *)
        translate_expr formal :: build_args formals actuals
    | (id_formal, None) :: _, _ ->
        Utils.legality_error "No default expr for parameter %a" Utils.pp_node
          id_formal
    | [], {ParamActual.param= Some id_actual} :: _ ->
        Utils.legality_error
          "Actual %a does not have a corresponding parameter" Utils.pp_node
          id_actual
    | [], [] ->
        (* No args *)
        []
    | _ ->
        (* Should not happen, the type in lal is not precise enough *)
        Utils.lal_error "Not able to find a parameter"
  in
  build_args formals_with_default actuals

and translate_attribute_ref (attribute_ref : AttributeRef.t) =
  let attribute = Utils.attribute (AttributeRef.f_attribute attribute_ref) in
  match attribute with
  | (`Access | `Unchecked_Access | `Unrestricted_Access | `Address) as
    lal_access_kind ->
      let access_kind =
        let open Ada_ir.Expr in
        match lal_access_kind with
        | `Access ->
            Access
        | `Unchecked_Access ->
            Unchecked_Access
        | `Unrestricted_Access ->
            Unrestriced_Access
        | `Address ->
            Address
      in
      let accessed =
        translate_lval (AttributeRef.f_prefix attribute_ref :> Expr.t)
      in
      Ada_ir.Expr.AccessOf (access_kind, accessed)
  | _ ->
      Utils.unimplemented attribute_ref

and translate_box_expr (_box_expr : BoxExpr.t) = assert false

and translate_if_expr (_if_expr : IfExpr.t) = assert false

and translate_case_expr (_case_expr : CaseExpr.t) = assert false

and translate_case_expr_alternative
    (_case_expr_alternative : CaseExprAlternative.t) =
  assert false

and translate_quantified_expr (_quantified_expr : QuantifiedExpr.t) =
  assert false

and translate_allocator (_allocator : Allocator.t) = assert false

and translate_raise_expr (_raise_expr : RaiseExpr.t) = assert false

let translate_expr (expr : [< Expr.t]) = translate_expr (expr :> Expr.t)