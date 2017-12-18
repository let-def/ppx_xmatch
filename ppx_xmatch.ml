open Migrate_parsetree

(* Define the rewriter on OCaml 4.05 AST *)
open Ast_405
let ocaml_version = Versions.ocaml_405

module P = Parsetree
module M = Ast_mapper

let is_resume = function
  | {P. pexp_desc = P.Pexp_extension ({Location. txt = "resume"}, P.PStr [])} ->
      true
  | _ -> false

let is_match_resume = function
  | {P. pexp_desc = P.Pexp_extension ({Location. txt = "resume"}, P.PStr [item])} ->
      begin match item.P.pstr_desc with
      |  P.Pstr_eval (expr, []) ->
          begin match expr.P.pexp_desc with
          | P.Pexp_match _ | P.Pexp_function _ -> Some expr
          | _ -> None
          end
      | _ -> None
      end
  | _ -> None

let rewrite_match pexp =
  let rec find_resume pexp =
    if is_resume pexp then raise Exit else
      match pexp.P.pexp_desc with
      | P.Pexp_let (_,_,k) | P.Pexp_sequence (_,k)
      (*| P.Pexp_poly (k,_) | P.Pexp_constraint (k,_) | P.Pexp_coerce (k,_,_)*)
      | P.Pexp_letmodule (_,_,k) | P.Pexp_letexception (_,k)
      | P.Pexp_newtype (_,k) | P.Pexp_open (_,_,k) ->
          find_resume k
      | P.Pexp_ifthenelse (_,k1,None) ->
          find_resume k1
      | P.Pexp_ifthenelse (_,k1,Some k2) ->
          find_resume k1; find_resume k2
      | P.Pexp_match (_,ks) ->
          List.iter (fun case -> find_resume case.P.pc_rhs) ks
      | P.Pexp_try (k,ks) ->
          List.iter (fun case -> find_resume case.P.pc_rhs) ks;
          find_resume k
      | P.Pexp_ident _          | P.Pexp_constant _
      | P.Pexp_function _       | P.Pexp_fun (_,_,_,_)
      | P.Pexp_apply (_,_)      | P.Pexp_tuple _
      | P.Pexp_construct (_,_)  | P.Pexp_variant (_,_)
      | P.Pexp_record (_,_)     | P.Pexp_field (_,_)
      | P.Pexp_setfield (_,_,_) | P.Pexp_array _
      | P.Pexp_while (_,_)      | P.Pexp_for (_,_,_,_,_)
      | P.Pexp_send (_,_)       | P.Pexp_new _
      | P.Pexp_setinstvar (_,_) | P.Pexp_override _
      | P.Pexp_assert _         | P.Pexp_lazy _
      | P.Pexp_object _         | P.Pexp_pack _
      | P.Pexp_extension _      | P.Pexp_unreachable
      | P.Pexp_poly (_,_)       | P.Pexp_constraint (_,_)
      | P.Pexp_coerce (_,_,_)   -> ()
  in
  let rec traverse pexp =
    let return pexp_desc = {pexp with P.pexp_desc} in
    if is_resume pexp then
      return ([%expr false] [@metaloc pexp.P.pexp_loc]).P.pexp_desc
    else match pexp.P.pexp_desc with
      (* Recursive cases *)
      | P.Pexp_let (rf,vbs,k) ->
        return (P.Pexp_let (rf,vbs, traverse k))
      | P.Pexp_sequence (e,k) ->
        return (P.Pexp_sequence (e, traverse k))
      | P.Pexp_letmodule (n,v,k) ->
        return (P.Pexp_letmodule (n,v, traverse k))
      | P.Pexp_letexception (c,k) ->
        return (P.Pexp_letexception (c, traverse k))
      | P.Pexp_newtype (t,k) -> return (P.Pexp_newtype (t, traverse k))
      | P.Pexp_open (f,p,k) -> return (P.Pexp_open (f,p, traverse k))
      | P.Pexp_ifthenelse (c,k1,None) ->
        let k2 = [%expr true] [@metaloc pexp.P.pexp_loc] in
        return (P.Pexp_ifthenelse (c, traverse k1, Some (traverse k2)))
      | P.Pexp_ifthenelse (c,k1,Some k2) ->
        return (P.Pexp_ifthenelse (c, traverse k1, Some (traverse k2)))
      | P.Pexp_match (e,ks) ->
        return (P.Pexp_match (e, List.map traverse_case ks))
      | P.Pexp_try (k,ks) ->
        return (P.Pexp_try (traverse k, List.map traverse_case ks))
      (* End cases *)
      | P.Pexp_ident _          | P.Pexp_constant _
      | P.Pexp_function _       | P.Pexp_fun (_,_,_,_)
      | P.Pexp_apply (_,_)
      | P.Pexp_tuple _          | P.Pexp_construct (_,_)
      | P.Pexp_variant (_,_)    | P.Pexp_record (_,_)
      | P.Pexp_field (_,_)      | P.Pexp_setfield (_,_,_) | P.Pexp_array _
      | P.Pexp_while (_,_)      | P.Pexp_for (_,_,_,_,_)
      | P.Pexp_constraint (_,_) | P.Pexp_coerce (_,_,_)
      | P.Pexp_send (_,_)       | P.Pexp_new _
      | P.Pexp_setinstvar (_,_) | P.Pexp_override _
      | P.Pexp_assert _         | P.Pexp_lazy _
      | P.Pexp_poly (_,_)       | P.Pexp_object _
      | P.Pexp_pack _           | P.Pexp_extension _ ->
        [%expr
          __resume_var.Pervasives.contents <- [%e pexp];
          true
        ] [@metaloc pexp.P.pexp_loc]
      | P.Pexp_unreachable -> pexp
  and traverse_case case =
    {case with P.pc_rhs = traverse case.P.pc_rhs}
  in
  let process_case case =
    match find_resume case.P.pc_rhs with
    | () -> case
    | exception Exit ->
      let rhs = traverse case.P.pc_rhs in
      let pc_guard =
        match case.P.pc_guard with
        | None -> Some rhs
        | Some guard ->
          let loc =
            {Location.
              loc_start = guard.P.pexp_loc.Location.loc_start;
              loc_end = rhs.P.pexp_loc.Location.loc_end;
              loc_ghost = true}
          in
          Some ([%expr [%e guard] && [%e rhs]] [@metaloc loc])
      in
      let pc_rhs =
        [%expr __resume_var.Pervasives.contents] [@metaloc pexp.P.pexp_loc]
      in
      {case with P. pc_guard; pc_rhs}
  in
  let process_cases e cs =
    {pexp with P.pexp_desc = P.Pexp_match (e, List.map process_case cs)}
  in
  let find_resume cases =
    match List.iter (fun case -> find_resume case.P.pc_rhs) cases with
    | () -> false
    | exception Exit -> true
  in
  match pexp.P.pexp_desc with
  | P.Pexp_match (e, cases) when find_resume cases ->
    [%expr
      let __resume_var = { Pervasives.contents = Obj.magic () } in
      [%e process_cases e cases]
    ] [@metaloc pexp.P.pexp_loc]
  | P.Pexp_function cases when find_resume cases ->
    [%expr
      fun __resume_arg ->
        let __resume_var = { Pervasives.contents = Obj.magic () } in
        [%e process_cases [%expr __resume_arg] cases]
    ] [@metaloc pexp.P.pexp_loc]
  | _ -> pexp

(* Action of the rewriter:
   In matchings of the form:
      match%resume x with ...
   or
      function%resume ...

   using %resume in a return position on the right-hand side of a branch will
   resume execution of the pattern matching.
*)

let mapper _config _cookies =
  let open Ast_mapper in
  let open Ast_helper in
  let expr mapper pexp =
    let pexp = match is_match_resume pexp with
      | Some expr -> rewrite_match expr
      | None -> pexp
    in
    M.default_mapper.M.expr mapper pexp
  in
  {default_mapper with expr}

(* Register the rewriter in the driver *)
let () =
  Driver.register ~name:"ppx_xmatch" ocaml_version mapper
