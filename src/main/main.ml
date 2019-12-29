open Libadalang

let f = Sys.argv.(1)

let ctx = AnalysisContext.create ()

let u = AnalysisContext.get_from_file ctx f

let root = match AnalysisUnit.root u with Some n -> n | None -> assert false

let handle_basic_decl bd : unit =
  if BasicDecl.p_is_subprogram bd then (
    let name = Option.value_exn (BasicDecl.p_defining_name bd) in
    let info = Ada_ir.Expr.{ fname = name } in
    let dep_sig = Ir2dep.gen_signature info in
    let synthesized_term =
      Option.value_exn (Libdependzlang.Term.p_synthesize dep_sig)
    in
    let simple_ada_term = Dep2ada.translate_def info synthesized_term in
    Format.printf "@[%s@]@." (Libdependzlang.Term.p_to_string dep_sig);
    Format.printf "@[%s@]@." (Libdependzlang.Term.p_to_string synthesized_term);
    Format.printf "@[%a@]@." Simple_ada.pp_fun_decl simple_ada_term )

let () = AdaNode.findall BasicDecl root |> List.iter ~f:handle_basic_decl
