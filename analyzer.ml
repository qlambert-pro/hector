(*
 * Copyright 2013, Inria
 * Suman Saha, Julia Lawall, Gilles Muller
 * This file is part of Hector.
 *
 * Hector is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Hector is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Hector.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Hector under other licenses.
 *)

open Common

let all_def = ref []


let rec find_ptr_args_list = function
    []->[]
  | h::t->
    match (Def.is_pointer h) with
      Def.IsPtr       -> h::find_ptr_args_list t
    | Def.UnknownType -> h::find_ptr_args_list t
    | _               -> find_ptr_args_list t


let rec is_error_return_code ((e,_),_) =
  match e with
  | (Ast_c.Constant (Ast_c.Int("0", _))) ->
    (Var_dec.zero_rtrn_ehc:= !Var_dec.zero_rtrn_ehc + 1; (* for PHP true *) false)
  | (Ast_c.Constant (Ast_c.Int("1", _))) -> false  (* for PHP false *)
  | (Ast_c.ParenExpr e) -> is_error_return_code e
  | (Ast_c.Unary (_, Ast_c.UnMinus)) -> true
  | (Ast_c.Unary ((((Ast_c.Constant (Ast_c.Int("0", _))), _), _), Ast_c.Tilde)) -> true
  | (Ast_c.Cast (_, e)) -> is_error_return_code e
  | (Ast_c.FunCall ((((Ast_c.Ident (Ast_c.RegularName(s, _))), _), _), _)) ->
    if s =~ "ERR_PTR" || s =~ "PTR_ERR"
    then true
    else (Var_dec.func_rtrn_ehc:= !Var_dec.func_rtrn_ehc + 1; false)
  | (Ast_c.Ident (Ast_c.RegularName(_, _)))->
    (Var_dec.oth_rtrn_ehc:= !Var_dec.oth_rtrn_ehc + 1; false)
  | _-> Var_dec.oth_rtrn_ehc:= !Var_dec.oth_rtrn_ehc + 1; false


let rec remove_string_args = function
    []-> []
  | h::t-> if(Def.string_exists_in_explist [h]) then
      remove_string_args t
    else h::(remove_string_args t)

let rec args_list_contain_id id = function
    []-> false
  | h::t-> match h with
      (((Ast_c.Unary (e, Ast_c.GetRef)), typ), ii)-> if (Def.compare_exps id e) then true
      else args_list_contain_id id t
    |_-> args_list_contain_id id t


let rec assign_var_is_null var = function
    []-> false
  | h::t-> match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.Ident (Ast_c.RegularName("NULL",ii2))), typ1), ii1))), typ), ii)) ->
      if (Def.compare_exps var e1) then true
      else assign_var_is_null var t
    |_-> assign_var_is_null var t


let rec find_recent_id_values_paths_inner id id_value any_access  mark =  function
    []-> id_value
  | h::t->
    match Ast_c.unwrap h with
    | Ast_c.Decl decl ->
      (match decl with
         Ast_c.DeclList decls ->
         (match Ast_c.unwrap decls with
            [one] ->
            let onedecl = Ast_c.unwrap2 one in
            (match onedecl.Ast_c.v_namei with
               Some (nm, vl) ->
               (match vl with
                  Ast_c.ValInit (ii, init) ->
                  (match Ast_c.unwrap init with
                     Ast_c.InitExpr (e1) ->
                     (match id with
                        (((Ast_c.Ident (ident)), typ11), ii11)->
                        if (Def.compare_names nm ident) then
                          find_recent_id_values_paths_inner  id (Some e1) any_access false t
                        else find_recent_id_values_paths_inner  id id_value any_access mark t
                      | (((Ast_c.RecordAccess   (e, ident)), typ11), ii11) ->
                        if (Def.compare_names nm ident)
                        then find_recent_id_values_paths_inner id (Some e1) any_access false t
                        else find_recent_id_values_paths_inner id id_value any_access mark t
                      | (((Ast_c.RecordPtAccess   (e, ident)), typ11), ii11) ->
                        if (Def.compare_names nm ident) then
                          find_recent_id_values_paths_inner  id (Some e1) any_access false t
                        else find_recent_id_values_paths_inner  id id_value any_access mark t
                      | _-> find_recent_id_values_paths_inner  id id_value any_access mark t)
                   | _-> find_recent_id_values_paths_inner  id id_value any_access mark t)
                |	Ast_c.ConstrInit _ ->
                  failwith "constrinit not supported"
                |	Ast_c.NoInit -> find_recent_id_values_paths_inner  id id_value any_access mark t )
             |  None-> find_recent_id_values_paths_inner  id id_value any_access mark t )
          | _-> find_recent_id_values_paths_inner  id id_value any_access mark t
         )
       | _-> find_recent_id_values_paths_inner  id id_value any_access mark t)

    |  Ast_c.ExprStatement (Some (((Ast_c.ParenExpr ((((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ4), ii4))), typ3), ii3))), typ), ii))->
      let args_list = remove_string_args(Def.remove_optionlist (Def.create_argslist [] es)) in
      let args_list = find_ptr_args_list args_list in
      if (args_list_contain_id id args_list) then
        find_recent_id_values_paths_inner  id (Some ((( Ast_c.FunCall  (e, es)), typ4), ii4))  any_access false t
      else find_recent_id_values_paths_inner  id id_value any_access mark t
    | Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, ((( Ast_c.FunCall  (e, es)), typ1), ii1))), typ), ii)) ->
      let args_list = remove_string_args(Def.remove_optionlist (Def.create_argslist [] es)) in
      let args_list = find_ptr_args_list args_list in
      if (args_list_contain_id id args_list) then
        if (assign_var_is_null e1 t) then
          find_recent_id_values_paths_inner  id id_value any_access mark t
        else find_recent_id_values_paths_inner  id (Some ((( Ast_c.FunCall  (e, es)), typ1), ii1)) true false t
      else if (Def.compare_exps id e1) then find_recent_id_values_paths_inner  id (Some ((( Ast_c.FunCall  (e, es)), typ1), ii1)) any_access false t
      else find_recent_id_values_paths_inner  id id_value any_access mark t

    | Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
      if (Def.compare_exps id e1) then(
        find_recent_id_values_paths_inner  id (Some e2) any_access false t)
      else (find_recent_id_values_paths_inner id id_value any_access mark t)
    | Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName (fn_n, [ii3]))), typ10), ii10), es)), typ), ii)) ->
      (match id_value with
         None-> let args_list = remove_string_args(Def.remove_optionlist (Def.create_argslist [] es)) in
         let args_list1 = find_ptr_args_list args_list in
         if (List.length args_list1)> 1 then
           find_recent_id_values_paths_inner id id_value any_access mark t
         else if(Def.exp_exists_in_list id args_list) && (List.length args_list1)=1  && (List.length args_list)<2 && any_access = false
                &&(not(Def.string_exists_in_stmt h))then
           find_recent_id_values_paths_inner  id
             (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName (fn_n, [ii3]))), typ10), ii10), es)), typ), ii)) any_access true t
         else find_recent_id_values_paths_inner id id_value any_access mark t
       | Some exp -> if mark = false then find_recent_id_values_paths_inner  id id_value any_access mark t
         else find_recent_id_values_paths_inner  id
             (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName (fn_n, [ii3]))), typ10), ii10), es)), typ), ii)) any_access true t
      )
    | _-> if (Def.exp_exists_in_stmt (Some id) h) then find_recent_id_values_paths_inner  id id_value true mark t
      else find_recent_id_values_paths_inner  id id_value any_access mark t


let rec find_recent_id_values_paths id values = function
    []   -> values
  | h::t ->
    let id_value =
      find_recent_id_values_paths_inner id None false false h in
    match id_value with
      None   -> find_recent_id_values_paths id values      t
    | Some a -> find_recent_id_values_paths id (a::values) t


let rec create_rr_ops_list rr_ops_list = function
    [] -> rr_ops_list
  | block::t ->
    let new_rr_ops_list = Block.get_resource_release block rr_ops_list in
    create_rr_ops_list new_rr_ops_list t


let rec recheck_blks c_function = function
    [] -> []
  | block::t->
    let is_assigned_error fin_lineno expr =
      let exe_paths_list =
        C_function.generate_exe_paths_simple fin_lineno [] c_function in
      let id_values = find_recent_id_values_paths expr [] exe_paths_list in
      List.exists is_error_return_code id_values
    in
    if not (Block.is_error_handling_block is_assigned_error block)
    then recheck_blks c_function t
    else
      block::(recheck_blks c_function t)


let rec refine_rr_ops = function
    [] -> []
  | ((Resource.Resource (_, args, _)) as resource)::t ->
    let refine_rr_ops_inner args_list resources =
      List.exists
        (fun (Resource.Resource (_, args, _)) ->
           Def.compare_explists args args_list)
        resources
    in
    if refine_rr_ops_inner args t
    then refine_rr_ops t
    else resource::(refine_rr_ops t)

let analyze_each_blk_new c_function rr_ops_list iifunc1 all_def func_name errblks_list =
  let rec analyze_each_blk_loop_new = function
      [] -> []
    | block::t ->
      let miss_rr_ops_list = Rr_op_finder.find_missing_rr_ops_new c_function
          block rr_ops_list in
      let update_miss_rr_ops_list = refine_rr_ops miss_rr_ops_list in
      let miss_rr_ops_list_new = Rm_true_positives.is_resource_having_same_def_new
          block c_function errblks_list update_miss_rr_ops_list in
      let miss_rr_ops_list1 = Rm_true_positives.is_rrwa_alloc
          miss_rr_ops_list_new in
      let miss_rr_ops_list_new2 = Rm_true_positives.return_resource_new
          block miss_rr_ops_list1 in
      let miss_rr_ops_list_new3 =
        Rm_true_positives.rls_in_exe_paths block c_function errblks_list
          miss_rr_ops_list_new2 in
      let miss_rr_ops_list_new4 =
        C_function.find_interproc_calls all_def block c_function
          miss_rr_ops_list_new3 in
      let miss_rr_ops_list_new5 =
        Rm_true_positives.resource_is_not_allocated_yet errblks_list
          block c_function (List.rev miss_rr_ops_list_new4) in

      let _ = Report.generate_report_new func_name block iifunc1
          miss_rr_ops_list_new5
      in
      analyze_each_blk_loop_new t
  in analyze_each_blk_loop_new errblks_list

let analyze_def full_file all_fn = function
    (defbis, iifunc1::iifunc2::i1::i2::ifakestart::isto) ->
    let {Ast_c.f_name = name;
         Ast_c.f_body = body;
        } = defbis
    in
    let func_name = fst (Ast_c.get_s_and_ii_of_name name) in

    Var_dec.func_name := func_name;
    Var_dec.file_info := Ast_c.file_of_info iifunc1;

    let clean_ast = Def.remove_stmtElelist body in
    let function_data = C_function.mk_c_function clean_ast in
    let new_errblks_list = C_function.find_errorhandling function_data in
    let updated_blk_list = recheck_blks function_data new_errblks_list in
    let updated_blk_list = Rm_true_positives.remove_blks_that_returns_resource
        function_data updated_blk_list in
    let updated_blk_list = List.rev updated_blk_list in
    let rr_ops_list_new =
      match updated_blk_list with
        h1::h2::t -> create_rr_ops_list [] updated_blk_list
      | _ -> []
    in
    let _ =
      match (updated_blk_list, rr_ops_list_new) with
        (h1::h2::t, l::u) ->
        analyze_each_blk_new function_data rr_ops_list_new iifunc1 !all_def
          func_name updated_blk_list
      | _ -> []
    in
    ({defbis with Ast_c.f_body = []},
     iifunc1::iifunc2::i1::i2::ifakestart::isto)
  | _ -> failwith "This is not a definition."

let analyze_toplevel file all_fn x =
  match x with
    Ast_c.Definition def -> Ast_c.Definition (analyze_def file all_fn def)
  | Ast_c.Declaration _
  | Ast_c.CppTop _
  | Ast_c.EmptyDef _
  | Ast_c.NotParsedCorrectly _
  | Ast_c.FinalDef _
  | Ast_c.IfdefTop _
  | Ast_c.MacroTop _ -> x
  | _ -> x
