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

let ref_var_mark = ref false

(**************Is return statement access the resource  *********)


let rec find_ptr_args_list = function
    []->[]
  |   h::t-> match (Def.is_pointer h) with
      Def.IsPtr->h::find_ptr_args_list t
    |   Def.UnknownType-> h::find_ptr_args_list t
    |   _-> find_ptr_args_list t

let rec any_exp_exists_in_stmt stmt = function
    []-> false
  | h::t-> if (Def.exp_exists_in_stmt (Some h) stmt) then true
    else any_exp_exists_in_stmt stmt t

let rec exp_exists_in_stmtlist exp = function
    []-> false
  | h::t-> if (Def.exp_exists_in_stmt exp h) then true
    else exp_exists_in_stmtlist exp t

let rec create_stmtlist = function
    []->[]
  | h::t-> ((Ast_c.ExprStatement (Some h),[]))::(create_stmtlist t)


let rec return_st_access_resource miss_st = function
    []   -> false
  | h::t ->
    (match Ast_c.unwrap h with
       Ast_c.Jump (Ast_c.ReturnExpr e1) ->
       (match Ast_c.unwrap miss_st with
          Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
          let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
          let args_stmtlist = create_stmtlist (find_ptr_args_list args_list) in
          (match args_list with
             [] -> false
           | _  ->
             any_exp_exists_in_stmt h args_list ||
             exp_exists_in_stmtlist (Some e1) args_stmtlist)
        | _ -> false)
      | _ -> return_st_access_resource miss_st t)


let rec stmtlist_func_contains_same_val return_val = function
    []-> false
  | h::t-> ( match Ast_c.unwrap h with
        Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
        let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
        if (Def.exp_exists_in_list return_val args_list) then true
        else stmtlist_func_contains_same_val return_val t
      |_-> stmtlist_func_contains_same_val return_val t
    )

let rec other_errblk_contains_same_val return_val = function
    []       -> false
  | block::t ->
    if Block.expression_used_as_argument block return_val then true
    else other_errblk_contains_same_val return_val t


let remove_blks_that_returns_resource c_function blocks =
  let remove_blks_that_returns_resource_aux block acc =
    (match Block.get_returned_expression block with
       None -> acc
     | Some (Ast_c.Jump (Ast_c.ReturnExpr e1), _) ->
       if C_function.exists_same_return_value e1 c_function
       then
         match Def.is_pointer e1 with
           Def.IsntPtr-> block::acc
         | _ -> acc
       else if other_errblk_contains_same_val e1 blocks
       then acc
       else block::acc
     | _ -> block::acc)
  in List.fold_right remove_blks_that_returns_resource_aux blocks []


let return_resource_new errblk =
  let return_resource_new_aux acc miss_rr =
    let (Resource.Resource (_, _, h)) = miss_rr in
    if Block.return_st_access_resource h errblk
    then acc
    else miss_rr::acc
  in List.fold_left return_resource_new_aux []



(* Ranking *)

let rec any_exp_exists_in_list args_list2 = function
    []-> false
  | h::t-> if (Def.exp_exists_in_list h args_list2) then true
    else  any_exp_exists_in_list args_list2 t



let rec ranking args_list1 upper lower miss_line   lbl_list  rrl = function
    []-> (upper,lower)
  |  h::t-> match Ast_c.unwrap h with
    |   Ast_c.Labeled (Ast_c.Label (name, st)) ->
      (match (ranking args_list1 upper lower miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |  (a,b) -> ranking args_list1 a b miss_line  lbl_list  rrl t)
    |   Ast_c.Labeled (Ast_c.Case  (e, st)) ->
      (match (ranking args_list1 upper lower miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b miss_line  lbl_list  rrl t)


    |   Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->
      (match (ranking args_list1 upper lower miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b miss_line  lbl_list  rrl t)



    |   Ast_c.Labeled (Ast_c.Default st) ->
      (match (ranking args_list1 upper lower miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b miss_line  lbl_list  rrl t)



    |   Ast_c.Compound statxs -> 
      (match (ranking args_list1 upper lower miss_line  lbl_list  rrl (Def.create_stmtlist h)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b  miss_line  lbl_list  rrl t)

    |   Ast_c.ExprStatement (Some (((Ast_c.Cast    (ty, (((Ast_c.FunCall  (e3, es3)), typ1), ii1))), typ), ii))->

      let args_list2 = Def.remove_optionlist (Def.create_argslist [] es3) in
      let line_no = Def.find_startline_no (Def.create_stmtlist h) in
      if (any_exp_exists_in_list args_list2 args_list1) then
        if line_no < miss_line then
          ranking args_list1 true lower  miss_line  lbl_list  rrl t
        else ranking args_list1 upper true  miss_line  lbl_list  rrl t
      else ranking args_list1 upper lower  miss_line  lbl_list  rrl t


    |   Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e3, es3)), typ), ii)) ->

      let args_list2 = Def.remove_optionlist (Def.create_argslist [] es3) in
      let line_no = Def.find_startline_no (Def.create_stmtlist h) in 
      if (List.length args_list1) = 0 && (Def.compare_stmts rrl h) then(
        if line_no < miss_line then 
          (
            print_string(string_of_int(miss_line));
            ranking args_list1 true lower  miss_line  lbl_list  rrl t)
        else (
          print_string(string_of_int(miss_line));

          ranking args_list1 upper true  miss_line  lbl_list  rrl t)
      )  
      else if (any_exp_exists_in_list args_list2 args_list1) then
        if line_no < miss_line then
          ranking args_list1 true lower  miss_line  lbl_list  rrl t
        else ranking args_list1 upper true  miss_line  lbl_list  rrl t
      else ranking args_list1 upper lower  miss_line  lbl_list  rrl t

    |   Ast_c.ExprStatement (Some e) -> ranking args_list1 upper lower  miss_line  lbl_list  rrl t
    |   Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
      (match (ranking args_list1 upper lower miss_line  lbl_list  rrl (Def.create_stmtlist st1)) with
         (true,true)-> (true,true)
       |    (a,b) -> ( match (ranking args_list1 a b  miss_line  lbl_list  rrl (Def.create_stmtlist st2)) with
             (true,true)-> (true,true)
           | (a1,b1) -> ranking args_list1 a1 b1  miss_line  lbl_list  rrl t

         )
      )


    |   Ast_c.Selection  (Ast_c.Switch (e, st)) ->
      (match (ranking args_list1 upper lower  miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b  miss_line  lbl_list  rrl t)

    |   Ast_c.Iteration  (Ast_c.While (e, st)) ->
      (match (ranking args_list1 upper lower  miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b  miss_line  lbl_list  rrl t)

    |   Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->
      (match (ranking args_list1 upper lower  miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b  miss_line  lbl_list  rrl t)

    | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
      failwith "for loop with declaration in first argument not supported"
    |   Ast_c.Iteration  (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->
      (match (ranking args_list1 upper lower miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b  miss_line  lbl_list  rrl t)

    |   Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) ->
      (match (ranking args_list1 upper lower  miss_line  lbl_list  rrl (Def.create_stmtlist st)) with
         (true,true)-> (true,true)
       |   (a,b) -> ranking args_list1 a b  miss_line  lbl_list  rrl t)

    |   Ast_c.Jump (Ast_c.Goto name) -> 
      let goto_code = Errorhandling.gather_goto_code lbl_list [] [h]  in
      (match (ranking args_list1 upper lower  miss_line  lbl_list  rrl goto_code) with
         (true,true)-> (true,true)
       |  (a,b) -> ranking args_list1 a b  miss_line  lbl_list  rrl t)

    |   Ast_c.Jump ((Ast_c.Continue|Ast_c.Break)) -> ranking args_list1 upper lower  miss_line  lbl_list  rrl t

    |   Ast_c.Jump (Ast_c.Return) -> ranking args_list1 upper lower  miss_line  lbl_list  rrl []
    |   Ast_c.Jump (Ast_c.ReturnExpr e) -> ranking args_list1 upper lower  miss_line  lbl_list  rrl []
    |   Ast_c.Jump (Ast_c.GotoComputed e) -> ranking args_list1 upper lower  miss_line  lbl_list  rrl t
    |   Ast_c.Decl decl -> ranking args_list1 upper lower  miss_line  lbl_list  rrl t
    |   Ast_c.Asm asmbody -> ranking args_list1 upper lower  miss_line  lbl_list  rrl t
    |   Ast_c.NestedFunc def -> ranking args_list1 upper lower  miss_line  lbl_list  rrl t
    |   _ ->  ranking args_list1 upper lower  miss_line  lbl_list  rrl t




let rec find_ranking_main blk_strtlineno prog  lbl_list = function
    []-> []
  | (alloc,args,rrl)::t-> 
    let (upper,lower) = ranking args false false blk_strtlineno  lbl_list  rrl prog in
    (match (upper, lower) with
       (true, true) -> (alloc,args,rrl,Def.Hr)::(find_ranking_main blk_strtlineno prog  lbl_list t)
     |	(true, false) -> (alloc,args,rrl,Def.Mr)::(find_ranking_main blk_strtlineno prog  lbl_list t)
     |	(false, true) -> (alloc,args,rrl,Def.Lr)::(find_ranking_main blk_strtlineno prog  lbl_list t)
     |	(_,_)-> (alloc,args,rrl,Def.No)::(find_ranking_main blk_strtlineno prog  lbl_list t)

    )

let rec refine_ref_list args_list = function
    []       -> []
  | (a,b)::t ->
    if Def.compare_exps a (List.hd args_list)
    then (a,b)::(refine_ref_list args_list t)
    else refine_ref_list args_list t


let rec any_ref_var_access_stmt args_list = function
    []-> false
  | h::t->
    match h with 
      (((Ast_c.Ident (Ast_c.RegularName("NULL",ii2))), typ), ii)-> any_ref_var_access_stmt args_list t
    |_->
      if (Def.exp_exists_in_list h args_list) then (true)
      else (any_ref_var_access_stmt args_list t)

let rec any_var_access_stmt args_list = function
    []-> false
  | (var,ref_vars)::t-> 
    if (Def.exp_exists_in_list var args_list) then (ref_var_mark:= false; true)
    else if  any_ref_var_access_stmt args_list ref_vars then ((*print_string("\nTrue Inner\n");*) ref_var_mark:= true; true)
    else any_var_access_stmt args_list t


let rec fn_exists_in_list = function
    []-> false
  | h::t->
    (match Ast_c.unwrap h with
       Common.Left es -> (match  es with
           (((Ast_c.FunCall  (e, es1)), typ), ii)-> true
         |(((Ast_c.SizeOfExpr  (e)), typ), ii)-> true
         | (((Ast_c.SizeOfType  (t)), typ), ii)-> true
         | _ ->  fn_exists_in_list t )
     | _ -> fn_exists_in_list t
    )

let rec rls_by_getref vars = function
    []-> false 
  | h::t-> 
    match h with
    | (((Ast_c.Unary (e, Ast_c.GetRef)), typ), ii)->
      if (Def.exp_exists_in_list e vars) then true
      else rls_by_getref vars t

    |_-> rls_by_getref vars t 

let rec rls_by_getref_outer args_list = function
    []-> false
  | (a,b)::t-> if (rls_by_getref b args_list ) then true
    else rls_by_getref_outer args_list t


let rec find_last_access var ref_vars last_access = function
    []-> last_access
  | h::t-> 
    match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e3, es3)), typ), ii)) ->
      let args_list = Def.remove_optionlist (Def.create_argslist [] es3) in
      if (not(Def.string_exists_in_stmt h)) && (not(fn_exists_in_list es3))then (
        if (Def.exp_exists_in_list var args_list) then(
          ref_var_mark:= false;
          find_last_access var ref_vars (Some h) t)
        else if (any_var_access_stmt args_list ref_vars) then(
          find_last_access var ref_vars (Some h) t)
        else if (match var with
              (((Ast_c.RecordAccess   (e, name)), typ), ii)->
              if (Def.compare_exps var e) then true
              else if (rls_by_getref [e] args_list) then (ref_var_mark:= true; true)
              else false
            |  (((Ast_c.RecordPtAccess   (e, name)), typ), ii)-> 
              if (Def.compare_exps var e) then true
              else false  
            | _-> false
          ) then find_last_access var ref_vars (Some h) t
        else if (rls_by_getref_outer args_list ref_vars)  then ( ref_var_mark:= true; find_last_access var ref_vars (Some h) t)
        else if (rls_by_getref [var] args_list) then (ref_var_mark:= false; find_last_access var ref_vars (Some h) t)
        else find_last_access var ref_vars last_access t)
      else find_last_access var ref_vars last_access t
    | Ast_c.ExprStatement (Some  ((( Ast_c.Assignment (e1, op, 
                                                       (((Ast_c.CondExpr ( ((( Ast_c.FunCall  
                                                                                 ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR",ii4))), typ3), ii3), es10)), typ2), ii2), e2, e3)), typ1), ii1))), typ), ii))->

      let args_list10 = Def.remove_optionlist (Def.create_argslist [] es10)  in 
      if (Def.exp_exists_in_list var args_list10) then ( ref_var_mark:= false;

                                                         find_last_access var ref_vars (Some h) t)
      else if (any_var_access_stmt args_list10 ref_vars) then(
        find_last_access var ref_vars (Some h) t)
      else  find_last_access var ref_vars last_access t


    | Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, ((Ast_c.FunCall  (e3, es3), typ1), ii1))), typ2), ii2)) ->
      let args_list = Def.remove_optionlist (Def.create_argslist [] es3) in
      if (Def.exp_exists_in_list var args_list) then ( ref_var_mark:= false;
                                                       find_last_access var ref_vars (Some h) t)
      else if (any_var_access_stmt args_list ref_vars) then(
        find_last_access var ref_vars (Some h) t)
      else find_last_access var ref_vars last_access t
    | _->
      if (Def.exp_exists_in_stmt (Some var) h) then (
        find_last_access var ref_vars None  t)
      else find_last_access var ref_vars last_access t


let rec stmt_exists_in_list st = function
    []-> false
  |  h::t -> if (Def.find_startline_no [st]) = (Def.find_startline_no [h])  then true
    else stmt_exists_in_list st t


let rec stmt_exists_in_any_list st = function
    []-> false
  |    h::t -> if(stmt_exists_in_list st h) then true
    else  stmt_exists_in_any_list st t


let rec rr_in_exe_paths_init_lbl rls c_function last_func blk_strtlineno = function
    []-> false
  | stmtlist::t->
    if(Def.stmt_exists_in_list rls stmtlist) then
      let exe_paths_list = C_function.generate_exe_paths_simple
          (blk_strtlineno-1) [] c_function in
      if(stmt_exists_in_any_list last_func exe_paths_list) then true
      else rr_in_exe_paths_init_lbl rls c_function last_func blk_strtlineno t
    else rr_in_exe_paths_init_lbl rls c_function last_func blk_strtlineno t


let rec rr_in_exe_paths_new rls c_function last_func = function
    []->false
  | (brnch_strtlineno,test_case,typ,blk_strtlineno,blk_endlineno,stmtlist)::t->
    if(Def.stmt_exists_in_list rls stmtlist) then
      let exe_paths_list = C_function.generate_exe_paths_simple
          (blk_strtlineno-1) [] c_function in
      if(stmt_exists_in_any_list last_func exe_paths_list) then true
      else rr_in_exe_paths_new rls c_function last_func t
    else rr_in_exe_paths_new rls c_function last_func t

let rec stmt_exists_in_list st = function
    []-> false
  | h::t-> if (Def.compare_stmts st h) && (not(Def.string_exists_in_stmt h)) then true
    else stmt_exists_in_list st t

let rec release_by_address arg = function
    []-> false
  | h::t->
    match h with
    | (((Ast_c.Unary (e, Ast_c.GetRef)), typ), ii)->
      if (Def.compare_exps arg e) then true
      else
        (
          match e with
            ((( Ast_c.RecordAccess   (e, name)), typ1), ii1)-> if(Def.compare_exps arg e) then true
            else release_by_address arg t
          | ((( Ast_c.RecordPtAccess   (e, name)), typ1), ii1)->if(Def.compare_exps arg e) then true
            else release_by_address arg t
          | _->release_by_address arg t
        )

    | _-> release_by_address arg t



let rec is_locally_dec branch_lineno = function
    []   -> false
  | h::t ->
    let start_line = Def.find_startline_no (Def.create_stmtlist h) in
    let end_line = Def.find_endline_no (Def.create_stmtlist h) in
    if (branch_lineno>= start_line && branch_lineno<=end_line) then true
    else is_locally_dec branch_lineno t


let rec is_locally_main branch_lineno name local = function
    []   -> local
  | h::t ->
    let rec_call name local st =
      let new_local =
        is_locally_main branch_lineno name local (Def.create_stmtlist st) in
      is_locally_main branch_lineno name new_local t
    in
    match Ast_c.unwrap h with
    | Ast_c.Labeled (Ast_c.Label (name, st)) ->
      rec_call name local st

    | Ast_c.Labeled (Ast_c.Case  (_, st))
    | Ast_c.Labeled (Ast_c.CaseRange  (_, _, st))
    | Ast_c.Labeled (Ast_c.Default st)
    | Ast_c.Selection (Ast_c.Switch (_, st))
    | Ast_c.Iteration (Ast_c.While (_, st))
    | Ast_c.Iteration (Ast_c.DoWhile (st, _))
    | Ast_c.Iteration (Ast_c.For (Ast_c.ForExp _, _, _, st))
    | Ast_c.Iteration (Ast_c.MacroIteration (_, _, st)) ->
      rec_call name local st

    | Ast_c.Compound _ ->
      rec_call name local h

    | Ast_c.Selection (Ast_c.If (_, st1, st2)) ->
      (* !! no use of st2 ???? *)
      let new_local1 =
        is_locally_main branch_lineno name local (Def.create_stmtlist st1) in
      let new_local2 =
        is_locally_main branch_lineno name new_local1 (Def.create_stmtlist st1) in
      is_locally_main branch_lineno name new_local2 t

    | Ast_c.Decl (Ast_c.DeclList decls) ->
      (match Ast_c.unwrap decls with
         [one] ->
         let onedecl = Ast_c.unwrap2 one in
         (match onedecl.Ast_c.v_namei with
            Some (nm, _) when Def.compare_names nm name ->
            let is_declared_locally = is_locally_dec branch_lineno t in
            is_locally_main branch_lineno name is_declared_locally t
          | _ -> is_locally_main branch_lineno name local t)
       | _ -> is_locally_main branch_lineno name local t)

    | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _, _, _, _)) ->
      failwith "for loop with declaration in first argument not supported"

    |  _ -> is_locally_main branch_lineno name local t


let any_exp_exists_in_stmtlist_inner arg =
  let any_exp_exists_in_stmtlist_inner_aux h =
    match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some ((( Ast_c.FunCall  (e, es)), typ), ii)) ->
      if not (Def.string_exists_in_stmt h)
      then
        let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
        Def.exp_exists_in_list arg args_list ||
        release_by_address arg args_list
      else false
    | Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
      Def.compare_exps arg e1
    | _ -> false
  in
  List.exists any_exp_exists_in_stmtlist_inner_aux


let rec any_exp_exists_in_stmtlist stmtlist = function
    []-> false
  | h::t-> if(any_exp_exists_in_stmtlist_inner h stmtlist) then true
    else any_exp_exists_in_stmtlist stmtlist t

let is_defined_alloc alloc =
  match Ast_c.unwrap alloc with
  | Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident
                                                     (Ast_c.RegularName (id,
                                                                         _))),
                                                  _), _), _)), _), _)) ->
    Errorhandling.defined_alloc id
  | _-> false


(* The function test for numbers and lower case character presumbly as an
 * heuristic to differentiate with macros *)
let is_function_call alloc =
  let reg_alloc = Str.regexp "[a-z0-9_]+" in
  match alloc with
    (Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident (Ast_c.RegularName (id, ii2))), typ1), ii1), es)), typ), ii)), ii4)
    when Str.string_match reg_alloc id 0 -> true
  | _ -> false


let rls_in_exe_paths block c_function errblk_list miss_rrs =
  let rls_in_exe_paths_aux (Resource.Resource (alloc, args, rr)) =
    match Ast_c.unwrap rr with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (_, es)), _), _)) ->
      let final_return = C_function.find_final_return c_function in
      let args_list =
        find_ptr_args_list (Def.remove_optionlist (Def.create_argslist [] es)) in
      (is_function_call alloc) &&
      ((List.length args_list = 0) ||
       not (List.length args = 0) &&
       not ((
           match List.hd args_list with
             ((Ast_c.Ident ident, _), _)
           | (((Ast_c.RecordAccess (((Ast_c.Ident ident, _), _), _)), _), _)
           | (((Ast_c.RecordPtAccess (((Ast_c.Ident ident, _), _), _)), _), _) ->
             not (C_function.is_locally_main block ident true c_function)
           | _ -> false) ||
           Block.does_block_contains_statement block rr ||
           List.exists (Block.contains_expression block) args ||
           is_defined_alloc alloc) &&
       C_function.check_each_path c_function alloc rr args args_list block
         final_return errblk_list)
    | _ -> false
  in
  List.rev (List.filter rls_in_exe_paths_aux miss_rrs)


let rec find_all_poss_alloc args_list = function
    []->[]
  |  h::t-> match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
      let args_list1 = Def.create_argslist [] es in
      if (List.length args_list1) > 0 && (Def.compare_explists (Def.remove_optionlist args_list1) (Def.remove_optionlist args_list)) then(
        h::(find_all_poss_alloc args_list t))
      else find_all_poss_alloc args_list t
    | _-> find_all_poss_alloc args_list t


let rec find_all_poss_alloc_main args_list poss_list = function
    []-> poss_list
  |  h::t-> let tmp_list = find_all_poss_alloc args_list h in
    if(List.length tmp_list) > 0 then
      find_all_poss_alloc_main args_list (tmp_list::poss_list) t
    else find_all_poss_alloc_main args_list poss_list t


(* Filter out the element with arguments *)
let find_all_rrwa statments =
  let find_all_rrwa_aux s acc =
    match Ast_c.unwrap s with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (_, es)), _), _)) ->
      let args_list = Def.create_argslist [] es in
      (match args_list with
         [] -> s::acc
       | _  -> acc)
    | _ -> acc
  in
  List.fold_right find_all_rrwa_aux statments []


let rec find_all_rrwa_main rrwa_list = function
    []   -> rrwa_list
  | h::t ->
    let tmp_list = find_all_rrwa h in
    if List.length tmp_list > 0
    then
      find_all_rrwa_main (tmp_list::rrwa_list) t
    else find_all_rrwa_main rrwa_list t


let rec make_pairs list1 list2 pairs_list =
  match (list1, list2) with
    (h::t, h1::t1) ->
    if not (Def.compare_stmts h h1)
    then make_pairs t t1 ([(h, h1)]@pairs_list)
    else make_pairs t t1  pairs_list
  | ([], _)
  | (_, []) -> pairs_list


(* Find the resource allocation from the resource release *)
let rec find_alloc_op miss_st = function
    [] -> None
  | (alloc, rr)::t ->
    if Def.compare_stmts rr miss_st
    then Some alloc
    else find_alloc_op miss_st t


let find_first_model_blk miss_st errblk_list =
  let find_first_model_blk_aux model block =
    match model with
      None       -> Some block
    | Some model_block ->
      if Block.does_block_contains_statement block miss_st &&
         Block.compare_branch_start block model_block < 0
      then Some block
      else model
  in
  List.fold_left find_first_model_blk_aux None errblk_list


let rec find_model_blk_init_lbl miss_st = function
    [] -> None
  | stmtlist::t ->
    if Def.stmt_exists_in_list miss_st stmtlist
    then
      Some ((Def.find_startline_no stmtlist), stmtlist)
    else
      find_model_blk_init_lbl miss_st t



let rec find_model_blk miss_st = function
    []->(0,[])
  |  (brnch_strtlineno,test_case,typ,blk_strtlineno,blk_endlineno,stmtlist)::t->
    if(Def.stmt_exists_in_list miss_st stmtlist) then
      (blk_strtlineno, stmtlist)
    else find_model_blk miss_st t


let find_app_model_blk miss_st miss_blk =
  let rec find_app_model_blk_aux (diff, model) = function
      []   -> model
    | block::t ->
    let miss_blk_strtlineno_diff = Block.compare_branch_start miss_blk block in
    if Block.does_block_contains_statement block miss_st
    then
      if miss_blk_strtlineno_diff < 0 &&
         diff = 0
      then Some block
      else
      if miss_blk_strtlineno_diff > diff
      then
        find_app_model_blk_aux (miss_blk_strtlineno_diff, Some block) t
      else find_app_model_blk_aux (diff, model) t
    else find_app_model_blk_aux (diff, model) t
  in
  find_app_model_blk_aux (0, None)

let rec find_app_model_blk_temp miss_st diff model model_line miss_blk = function
    [] -> (model_line, model)
  | (_, _, _, blk_strtlineno, _, stmtlist)::t ->
    let miss_blk_strtlineno_diff = miss_blk - blk_strtlineno in
    if Def.stmt_exists_in_list miss_st stmtlist
    then
      if miss_blk_strtlineno_diff < 0 &&
         diff = 0
      then (blk_strtlineno, stmtlist)
      else
      if miss_blk_strtlineno_diff > diff
      then
        find_app_model_blk_temp miss_st miss_blk_strtlineno_diff stmtlist blk_strtlineno miss_blk t
      else find_app_model_blk_temp miss_st diff model model_line miss_blk t
    else find_app_model_blk_temp miss_st diff model model_line miss_blk t


let rec find_common_rrwa_in_model_path_inner list = function
    []->[]
  | h::t-> if(Def.stmt_exists_in_all_list h list) then
      h::(find_common_rrwa_in_model_path_inner list t)
    else find_common_rrwa_in_model_path_inner list t


let find_common_rrwa_in_model_path = function
    []->[]
  | h::t-> (find_common_rrwa_in_model_path_inner  t h)


let rec rem_brn_st_frm_list_inner st = function
    []   -> false
  | h::t ->
    if Def.compare_stmts h st &&
       (Def.find_startline_no [h]) = (Def.find_startline_no [st])
    then
      true
    else rem_brn_st_frm_list_inner st t

(* Remove duplicate ? ... *)
let rec rem_brn_st_frm_list all_rrwa_in_model_branch = function
    []   -> []
  | h::t ->
    if rem_brn_st_frm_list_inner h all_rrwa_in_model_branch
    then
      rem_brn_st_frm_list all_rrwa_in_model_branch t
    else h::(rem_brn_st_frm_list all_rrwa_in_model_branch t)


let is_rrwa_alloc errblk_list miss_blk c_function =
  let is_rrwa_alloc_aux acc (Resource.Resource (alloc, args, rls)) =
    match Ast_c.unwrap rls with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (_, es)), _), _)) ->
      let args_list = Def.create_argslist [] es in
      (match args_list with
         [] -> acc
       | _  -> (Resource.Resource (alloc, args, rls))::acc)
    | _ -> acc
  in
  List.fold_left is_rrwa_alloc_aux []

(* Return a list of block starting line that does not release the resource *)
let no_previous_blk_has_rrl miss_block rrl alloc_line =
  let no_previous_blk_has_rrl_aux block =
    Block.compare_branch_start block alloc_line > 0 &&
    Block.compare_branch_end_with_start block miss_block < 0 &&
    not (Block.does_block_contains_statement block rrl)
  in
  List.filter no_previous_blk_has_rrl_aux


let rec stmt_exists_in_exe_paths_inner stmt = function
    []-> false
  | h::t -> if (Def.compare_stmts h stmt) then true
    else stmt_exists_in_exe_paths_inner stmt t

let rec stmt_exists_in_exe_paths stmt = function
    []-> false
  | h::t-> if stmt_exists_in_exe_paths_inner stmt h then true
    else stmt_exists_in_exe_paths stmt t

let rec find_actual_alloc exe_paths_candidate = function
    []   -> None
  | h::t ->
    if not (stmt_exists_in_exe_paths h exe_paths_candidate)
    then
      Some h
    else find_actual_alloc exe_paths_candidate t


let resource_is_not_allocated_yet errblks block c_function =
  let resource_is_not_allocated_yet_aux acc
      (Resource.Resource (alloc, args, rrl)) =
    let alloc_line = Def.find_startline_no (Def.create_stmtlist alloc) in
    let alloc_block = Block.mk_block_simple alloc_line (Def.create_stmtlist
                                                          alloc) in
    match Ast_c.unwrap rrl with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (_, es)), _), _)) ->
      let args_list = Def.create_argslist [] es in
      if List.length args_list = 1
      then
        let unreleasing_blocks = no_previous_blk_has_rrl block rrl alloc_block
            errblks in
        if List.length unreleasing_blocks > 0
        then
          let model_blk = find_app_model_blk rrl block errblks in

          let is_allocated =
            C_function.is_resource_allocated c_function model_blk block
              args_list
          in
          if is_allocated
          then acc
          else (Resource.Resource (alloc, args, rrl))::acc

        else (Resource.Resource (alloc, args, rrl))::acc
      else (Resource.Resource (alloc, args, rrl))::acc
    | _ -> acc
  in
  List.fold_left resource_is_not_allocated_yet_aux []

(* Return the list of last assigned valeus for each path when only one argument
 * has a list *)
let find_idvalues_list exe_paths_list id_values args_list =
  let find_idvalues_list_aux el =
    Errorhandling.find_recent_id_values_paths_second el [] exe_paths_list in
  let all_id_values = List.map find_idvalues_list_aux args_list in
  let tmp = Def.filter_empty_list_out all_id_values in
  match tmp with
    [id_values] -> id_values
  | _           -> []


let same_id_values list =
  List.for_all (fun v -> Def.compare_exps v (List.hd list)) list

let filter_null_out =
  let filter_null_out_aux acc expr =
    match expr with
      (((Ast_c.Ident (Ast_c.RegularName("NULL", _))), _), _) -> acc
    | _ -> expr::acc
  in
  List.fold_left filter_null_out_aux []

let is_resource_having_same_def_new block c_function errblk_list =
  let is_resource_having_same_def_new_aux acc
      (Resource.Resource(alloc, args_list, h)) =
    match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  _), _), _)) ->
      if args_list != []
      then
        let match_model = C_function.find_model_block_by_release_statement
            c_function h in

        let model_blk =
          match (find_first_model_blk h errblk_list, match_model) with
            (Some model, _) -> Some model
          | (_,          e) -> e
        in

        let id_values1 = C_function.get_identifier_values
            c_function (Some block) 0 args_list in
        let id_values2 = C_function.get_identifier_values
            c_function model_blk (-1) args_list in

        let unique_id_values =
          if List.length id_values1 = 1 &&
             List.length id_values2 = 1
          then
            match (List.hd id_values1, List.hd id_values2) with
              ((((Ast_c.FunCall (_, _)), _), _) as e1,
               (((Ast_c.Cast (_, ((((Ast_c.FunCall  (_, _)), _), _) as e2))), _),
                _)) when Def.compare_exps e1 e2
              -> [e1]
            | ((((Ast_c.FunCall (_, _)), _), _) as e1,
               ((((Ast_c.FunCall (_, _)), _), _) as e2)) when Def.compare_exps e1 e2
              -> [e1]
            | ((((Ast_c.Cast (_, ((((Ast_c.FunCall  (_, _)), _), _) as e1))), _),
                _),
               ((((Ast_c.FunCall (_, _)), _), _) as e2)) when Def.compare_exps e1 e2
              -> [e1]
            | ((((Ast_c.Cast (_, ((((Ast_c.FunCall (_, _)), _), _) as e1))), _),
                _),
               (((Ast_c.Cast (_, ((((Ast_c.FunCall (_, _)), _), _) as e2))), _), _))
              when Def.compare_exps e1 e2
              -> [e1]
            | _ -> []
          else [] in
        if List.length unique_id_values = 1
        then
          let new_alloc =
            (Ast_c.ExprStatement (Some (List.hd unique_id_values)), [])
          in
          (Resource.Resource (new_alloc, args_list, h))::acc
        else acc
      else (Resource.Resource (alloc, args_list, h))::acc
    | _ -> acc
  in
  List.fold_left is_resource_having_same_def_new_aux []
