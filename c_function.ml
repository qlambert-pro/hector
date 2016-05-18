(* **
 * Copyright 2013-2016, Inria
 * Suman Saha, Julia Lawall, Gilles Muller, Quentin Lambert
 * This file is part of Hector.

 * Hector is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.

 * Hector is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with Hector.  If not, see <http://www.gnu.org/licenses/>.

 * The authors reserve the right to distribute this or future versions of
 * Hector under other licenses.
 * *)

open Common

let ref_var_mark = ref false

type c_function =
  {ast: Ast_c.statement list;
   init_labels: Ast_c.statement list;
   cfg: Annotated_cfg.t;
   labels: (Ast_c.name * Ast_c.statement list) list}

let any_ref_var_access_stmt stmt ref_vars =
  List.exists (fun x -> Def.exp_exists_in_stmt (Some x) stmt) ref_vars

let insert_exp_into_ref_list e re e2 ref_list =
  let insert_exp_into_ref_list_aux (a, b) acc =
    if Def.compare_exps a e2
    then acc@[(a, b@[e])]@[(e2, [re])]
    else acc@[(a, b)]
  in
  if List.exists (fun (a, _) -> Def.compare_exps a e2) ref_list
  then List.fold_right insert_exp_into_ref_list_aux ref_list []
  else ref_list@[(e2, [e])]@[(e2, [re])]


let insert_exp_into_ref_list_loop e1 =
  let insert_exp_into_ref_list_loop_aux acc arg =
    insert_exp_into_ref_list e1 e1 arg acc
  in
  List.fold_left insert_exp_into_ref_list_loop_aux

let rec any_var_access_stmt stmt = function
    []-> false
  | (var, ref_vars)::t ->
    if Def.exp_exists_in_stmt (Some var) stmt
    then true
    else
    if any_ref_var_access_stmt stmt ref_vars
    then true
    else any_var_access_stmt stmt t

let gather_all_ref_var statements =
  let gather_all_ref_var_aux acc statement =
    match Ast_c.unwrap statement with
      Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.RecordAccess (e, name)), typ1), ii1), op,
                                                     (((Ast_c.Ident (ident)), typ2), ii2))), typ), ii))
    | Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.Ident (ident)), typ2), ii2) , op,
                                                     (((Ast_c.RecordAccess (e, name)), typ1), ii1))), typ), ii)) ->

      insert_exp_into_ref_list e (((Ast_c.RecordAccess   (e, name)), typ1), ii1)
        (((Ast_c.Ident (ident)), typ2), ii2) acc

    | Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.RecordPtAccess (e, name)), typ1), ii1), op,
                                                     (((Ast_c.Ident (ident)), typ2), ii2))), typ), ii))
    | Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.Ident (ident)), typ2), ii2), op,
                                                     (((Ast_c.RecordPtAccess (e, name)), typ1), ii1))), typ), ii)) ->

      insert_exp_into_ref_list e (((Ast_c.RecordPtAccess (e, name)), typ1), ii1)
        (((Ast_c.Ident (ident)), typ2), ii2) acc

    | Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ1), ii1))), typ), ii)) ->
      let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
      insert_exp_into_ref_list_loop e1 acc args_list

    | _ -> acc
  in
  List.fold_left gather_all_ref_var_aux [] statements

let find_final_return c_function =
  let {ast = ast} = c_function in
  let rec find_final_return_aux statements =
    match statements with
      [] -> 1000000
    | h::t-> match Ast_c.unwrap h with
        Ast_c.Jump (Ast_c.Return) -> Def.find_startline_no (Def.create_stmtlist h)
      | Ast_c.Jump (Ast_c.ReturnExpr e) -> Def.find_startline_no (Def.create_stmtlist h)
      | _ -> find_final_return_aux t
  in
  find_final_return_aux ast

let rec refine_args_list list  = function
    []->list
  |  h::t-> match h with
      (((Ast_c.Ident ( Ast_c.RegularName (s, ii2))), typ), ii)-> (refine_args_list (list@[s]) t)
    |  ((( Ast_c.RecordAccess   ((((Ast_c.Ident (Ast_c.RegularName (s1, ii3))), typ1), ii1), (Ast_c.RegularName (s2, ii2)))), typ), ii) ->
      (refine_args_list (list@[s1]@[s2]) t)
    |   ((( Ast_c.RecordPtAccess   ((((Ast_c.Ident (Ast_c.RegularName (s1, ii3))), typ1), ii1), (Ast_c.RegularName (s2, ii2)))), typ), ii) ->
      (refine_args_list (list@[s1]@[s2]) t)
    | _->  refine_args_list list t

(* Whether return statement exists in the statement list *)
let rec return_exists_in_list = function
    []   -> false
  | h::t ->
    match Ast_c.unwrap h with
      Ast_c.Jump (Ast_c.ReturnExpr _)
    | Ast_c.Jump (Ast_c.Return ) -> true
    | _ -> return_exists_in_list t

let rec gather_all_callin_func line_no = function
    []   -> []
  | h::t ->
    let start_lineno = Def.find_startline_no (Def.create_stmtlist h) in
    let end_lineno = Def.find_endline_no (Def.create_stmtlist h) in
    if (line_no>=start_lineno && line_no <= end_lineno) ||
       line_no >= start_lineno
    then
      match Ast_c.unwrap h with
      |	Ast_c.Labeled (Ast_c.Label (_, st))
      |	Ast_c.Labeled (Ast_c.Case (_, st))
      |	Ast_c.Labeled (Ast_c.CaseRange  (_, _, st))
      |	Ast_c.Labeled (Ast_c.Default st) ->
        if (line_no >= start_lineno && line_no <= end_lineno)
        then gather_all_callin_func line_no (Def.create_stmtlist st)
        else
        if line_no > end_lineno &&
           not (Def.boolean_of_option (Def.goto_exists_in_list (Def.create_stmtlist st))) &&
           not(return_exists_in_list (Def.create_stmtlist st))
        then
          gather_all_callin_func line_no (Def.create_stmtlist st)
        else
        if line_no < end_lineno
        then []
        else gather_all_callin_func line_no t

      |	Ast_c.ExprStatement (Some (((Ast_c.Assignment (_, _, (((Ast_c.FunCall
                                                                  _), _), _))),
                                    _), _))
      | Ast_c.ExprStatement (Some (((Ast_c.FunCall  _), _), _)) ->
        ((Def.find_startline_no (Def.create_stmtlist h)),h)::
        (gather_all_callin_func line_no t)
      |	Ast_c.ExprStatement (Some (((Ast_c.Assignment (_, _, e2)), _), _))->
        ((Def.find_startline_no (Def.create_stmtlist h)),
         (Ast_c.ExprStatement (Some e2), []))::
        (gather_all_callin_func line_no t)
      |	Ast_c.Selection (Ast_c.If (e, st1, st2)) ->
        let start_lineno_st1 = Def.find_startline_no (Def.create_stmtlist st1) in
        let end_lineno_st1 = Def.find_endline_no (Def.create_stmtlist st1) in
        let start_lineno_st2 = Def.find_startline_no (Def.create_stmtlist st2) in
        let end_lineno_st2 = Def.find_endline_no (Def.create_stmtlist st2) in
        if (line_no >= start_lineno_st1 && line_no <= end_lineno_st1)
        then
          gather_all_callin_func line_no (Def.create_stmtlist st1)
        else
        if (line_no >= start_lineno_st2 && line_no <= end_lineno_st2)
        then
          gather_all_callin_func line_no (Def.create_stmtlist st2)
        else gather_all_callin_func line_no t

      | Ast_c.Selection (Ast_c.Switch (_, st))
      | Ast_c.Iteration (Ast_c.While (_, st))
      | Ast_c.Iteration (Ast_c.DoWhile (st, _))
      | Ast_c.Iteration (Ast_c.For (Ast_c.ForExp _, _, _, st))
      | Ast_c.Iteration (Ast_c.MacroIteration (_, _, st)) ->
        if (line_no >= start_lineno && line_no <= end_lineno)
        then gather_all_callin_func line_no (Def.create_stmtlist st)
        else gather_all_callin_func line_no t

      | Ast_c.Iteration (Ast_c.For (Ast_c.ForDecl _, _, _, _)) ->
        failwith "for loop with declaration in first argument not supported"

      |	Ast_c.Jump (Ast_c.Return)
      |	Ast_c.Jump (Ast_c.ReturnExpr _) -> []
      | _ -> gather_all_callin_func line_no t
    else gather_all_callin_func line_no t


let rec find_func_frm_all_func callin_func = function
    [] -> None
  | (name, paramset, stmts)::t ->
    match Ast_c.unwrap callin_func with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident ident), _), _),
                                                  _)), _), _))
    | Ast_c.ExprStatement (Some (((Ast_c.Ident (ident)), _), _))->
      if Def.compare_names name ident
      then
        Some (name, paramset, stmts)
      else find_func_frm_all_func callin_func t

    | _ -> find_func_frm_all_func callin_func t


let rec check_for_calling_func stmt trans_arglist all_function =
  let check_for_calling_func_aux el =
    match (Ast_c.unwrap el, Ast_c.unwrap stmt) with
      (Ast_c.Labeled (Ast_c.Label (_, st)), _) ->
      check_for_calling_func stmt trans_arglist all_function (Def.create_stmtlist st)

    | (Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident _), _), _),
                                                   es)), _), _)),
       Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident _), _), _),
                                                   es1)), _), _))) ->
      let args_list1 = Def.remove_optionlist (Def.create_argslist [] es1) in
      let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
      let args_list2 =
        List.map (function arg -> (Ast_c.ExprStatement (Some arg), [])) args_list in
      let check_for_calling_func_of_option cfunc =
        match find_func_frm_all_func cfunc all_function with
          None -> false
        | Some (_, _, stmts) ->
          check_for_calling_func stmt trans_arglist all_function stmts
      in
      Def.compare_stmts stmt el ||
      (refine_args_list [] args_list1) = (refine_args_list [] args_list) ||
      List.exists check_for_calling_func_of_option args_list2 ||
      check_for_calling_func_of_option el

    | (Ast_c.Iteration (Ast_c.For (Ast_c.ForDecl _, _, _, _)), _) ->
      failwith "for loop with declaration in first argument not supported"

    | _ -> false
  in
  List.exists check_for_calling_func_aux


let find_func_frm_all_func_main st args_list all_function =
  let find_func_frm_all_func_main_aux (_, h) =
    match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some _) ->
      (match find_func_frm_all_func h all_function with
         None -> false
       | Some (_, _, stmts) ->
         check_for_calling_func st args_list all_function stmts)
    | _ -> false
  in
  List.exists find_func_frm_all_func_main_aux


let find_interproc_calls all_function block c_function =
  let branch = Block.extract_statements block in
  let miss_lineno = Block.extract_branch_start block in
  let {ast = prog} = c_function in
  let find_interproc_calls_aux acc (Resource.Resource (alloc, args, h)) =
    match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall (_, es)), _), _)) ->
      let args_list = Def.create_argslist [] es in
      if (args_list = [])
      then (Resource.Resource (alloc, args, h))::acc
      else
        let all_callin_func =
          (gather_all_callin_func (Def.find_endline_no branch) branch)
          @(gather_all_callin_func miss_lineno prog) in
        if find_func_frm_all_func_main h args_list all_function all_callin_func
        then
          acc
        else
          (Resource.Resource (alloc, args, h))::acc
    | _ -> (Resource.Resource (alloc, args, h))::acc
  in
  List.fold_left find_interproc_calls_aux []

let goto_jump_toback c_function st =
  let {labels = labels} = c_function in
  let rec goto_jump_toback_aux labels =
    match (Ast_c.unwrap st, labels) with
      (_, []) -> true
    | (Ast_c.Jump (Ast_c.Goto name), (a,b)::t) ->
      (match (Def.compare_names name a, (List.length b) > 0) with
         (false, _) -> goto_jump_toback_aux t
       | (_, false) -> true
       | _          -> (Def.find_startline_no b) < (Def.find_startline_no [st]))
    | _ -> true
  in
  goto_jump_toback_aux labels

let rec gather_goto_code c_function goto_code = function
    []   -> goto_code
  | h::t ->
    let {labels = lbl_list} = c_function in
    match Ast_c.unwrap h with
      Ast_c.Jump (Ast_c.Return) -> (goto_code@[h])
    | Ast_c.Jump (Ast_c.ReturnExpr e) -> (goto_code@[h])
    | Ast_c.Jump (Ast_c.Goto name) ->
      if not (goto_jump_toback c_function h)
      then
        let new_goto_code = Def.code_for_goto name lbl_list in
        if (List.length new_goto_code) = 0
        then []
        else
          gather_goto_code c_function goto_code new_goto_code
      else
          []
    | _-> gather_goto_code c_function (goto_code@[h]) t

let rec is_locally_dec branch_lineno = function
    []   -> false
  | h::t ->
    let start_line = Def.find_startline_no (Def.create_stmtlist h) in
    let end_line = Def.find_endline_no (Def.create_stmtlist h) in
    if (branch_lineno>= start_line && branch_lineno<=end_line) then true
    else is_locally_dec branch_lineno t

let is_locally_main block name local c_function =
  let branch_lineno = Block.extract_branch_start block in
  let {ast = ast} = c_function in
  let rec is_locally_main_aux name local = function
      []   -> local
    | h::t ->
      let rec_call name local st =
        let new_local =
          is_locally_main_aux name local (Def.create_stmtlist st) in
        is_locally_main_aux name new_local t
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
          is_locally_main_aux name local (Def.create_stmtlist st1) in
        let new_local2 =
          is_locally_main_aux name new_local1 (Def.create_stmtlist st1) in
        is_locally_main_aux name new_local2 t

      | Ast_c.Decl (Ast_c.DeclList decls) ->
        (match Ast_c.unwrap decls with
           [one] ->
           let onedecl = Ast_c.unwrap2 one in
           (match onedecl.Ast_c.v_namei with
              Some (nm, _) when Def.compare_names nm name ->
              let is_declared_locally = is_locally_dec branch_lineno t in
              is_locally_main_aux name is_declared_locally t
            | _ -> is_locally_main_aux name local t)
         | _ -> is_locally_main_aux name local t)

      | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _, _, _, _)) ->
        failwith "for loop with declaration in first argument not supported"

      |  _ -> is_locally_main_aux name local t
  in
  is_locally_main_aux name local ast

(* TODO clean*)
let rec create_init_lbl_prog_list init_lbl_list = function
    []   -> init_lbl_list
  | h::t ->
    match Ast_c.unwrap h with
      Ast_c.Labeled (Ast_c.Label (name, st)) ->
      if (List.length init_lbl_list) > 0
      then
        match Ast_c.unwrap st with
        | Ast_c.Decl _
        | Ast_c.ExprStatement (Some _) ->
          create_init_lbl_prog_list (init_lbl_list@[st]) t
        | Ast_c.Jump (Ast_c.Goto _)
        | Ast_c.Jump Ast_c.Return
        | Ast_c.Jump (Ast_c.ReturnExpr _) ->
          init_lbl_list@[st]
        | _ -> []
      else init_lbl_list

    | Ast_c.ExprStatement (Some _)
    | Ast_c.Decl _ ->
      create_init_lbl_prog_list (init_lbl_list@[h]) t
    | Ast_c.Jump (Ast_c.Goto _)
    | Ast_c.Jump (Ast_c.Return)
    | Ast_c.Jump (Ast_c.ReturnExpr _) ->
      init_lbl_list@[h]
    | _ -> create_init_lbl_prog_list [] t

let mk_c_function toplevel body =
  {ast = body;
   init_labels = create_init_lbl_prog_list [] body;
   cfg = Annotated_cfg.of_ast_c toplevel;
   labels = Def.create_lbl_list [] [] body}



let rec find_case_stmts fin_lineno case_stmts = function
    []->case_stmts
  | h::t-> let start_line = Def.find_startline_no (Def.create_stmtlist h)  in
    let end_line   = Def.find_endline_no (Def.create_stmtlist h) in
    if(fin_lineno>= start_line && fin_lineno<= end_line) then
      match Ast_c.unwrap h with
        Ast_c.Labeled (Ast_c.Case  (e, st)) -> ([((Ast_c.ExprStatement (Some e)),[])]@[st])
      | Ast_c.Labeled (Ast_c.Default st) -> [st]
      | _ -> (case_stmts@[h])
    else
      match Ast_c.unwrap h with
        Ast_c.Labeled (Ast_c.Case  (e, st)) -> find_case_stmts fin_lineno  ([((Ast_c.ExprStatement (Some e)),[])]@[st]) t
      | Ast_c.Labeled (Ast_c.Default st) -> find_case_stmts fin_lineno [st] t
      | _ -> find_case_stmts fin_lineno (case_stmts@[h]) t

let rec exe_path_for_switch_inner1  path = function
    []-> path
  | h::t-> exe_path_for_switch_inner1 (path@[h]) t


let rec exe_path_for_switch_inner new_path path = function
    []-> new_path
  | h::t-> let new_path1 = exe_path_for_switch_inner1  path h in
    exe_path_for_switch_inner (new_path@[new_path1]) path t

let rec exe_path_for_switch new_path path1 = function
    []-> new_path
  | h::t-> let new_path1 = exe_path_for_switch_inner [] h path1 in
    exe_path_for_switch (new_path@new_path1) path1 t

let rec create_compound_st_for_switch exp case_stmts stmt_list = function
    []->stmt_list
  | h::t-> match Ast_c.unwrap h with
      Ast_c.Labeled (Ast_c.Case  (e, st)) ->
      if(List.length case_stmts =0) then create_compound_st_for_switch e (case_stmts@[st]) stmt_list t
      else( create_compound_st_for_switch exp []
              (stmt_list@[(Ast_c.Labeled (Ast_c.Case  (exp, (Ast_c.Compound(Def.add_stmtElelist (case_stmts)),[]))),[])]) (h::t)
          )
    | Ast_c.Labeled (Ast_c.Default st) ->
      (stmt_list@[(Ast_c.Labeled (Ast_c.Case  (exp, (Ast_c.Compound(Def.add_stmtElelist (case_stmts)),[]))),[])]@
       [(Ast_c.Labeled (Ast_c.Default (Ast_c.Compound(Def.add_stmtElelist ([st]@t)),[])),[])])
    | _ -> create_compound_st_for_switch exp (case_stmts@[h]) stmt_list t




let map_append st = List.map (function x -> x@st)

let has_multiple_st = function
    []   -> false
  | h::t ->
    match Ast_c.unwrap h with
    | Ast_c.ExprStatement (None) -> false
    | _ -> true


let is_return_or_goto x =
  match Ast_c.unwrap x with
    Ast_c.Jump (Ast_c.ReturnExpr _) -> true
  | Ast_c.Jump (Ast_c.Return) -> true
  | Ast_c.Jump (Ast_c.Goto _) -> true
  | Ast_c.Jump (Ast_c.GotoComputed _) -> true
  | _ -> false


let rec invert_st st =
  let blank_type_info = ref (None, Ast_c.NotTest) in
  let mk_expr_st s = Ast_c.mk_st (Ast_c.ExprStatement (Some s)) [] in
  let nst = mk_expr_st st in
  match st with
  | (((Ast_c.Binary ((((Ast_c.ParenExpr (((Ast_c.Assignment (e1, op1, e2), _), _))), _), _),
                     (Ast_c.Logical Ast_c.Eq, _),
                     ((Ast_c.Ident (Ast_c.RegularName("NULL", _)), _), _))), _), _) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, op1, (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), blank_type_info), [])), blank_type_info), [])],
     [mk_expr_st ((Ast_c.Assignment (e1, op1, e2), blank_type_info), [])])

  | (((Ast_c.Binary ((((Ast_c.ParenExpr (((Ast_c.Assignment (e1, op1, e2), _), _))), _), _),
                     (Ast_c.Logical Ast_c.NotEq, _),
                     ((Ast_c.Ident (Ast_c.RegularName("NULL", _)), _), _))), _), _) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, op1, e2), blank_type_info), [])],
     [mk_expr_st ((Ast_c.Assignment (e1, op1, ((Ast_c.Ident (Ast_c.RegularName("NULL",[])), blank_type_info), [])), blank_type_info), [])])

  | (((Ast_c.Binary ((((Ast_c.ParenExpr (e1)), _), _),
                     (Ast_c.Logical Ast_c.Eq, _),
                     ((Ast_c.Ident (Ast_c.RegularName("NULL", _)), _), _))), _), _) ->
    ([mk_expr_st (((Ast_c.Assignment (e1, (Ast_c.SimpleAssign, []), (((Ast_c.Ident (Ast_c.RegularName("NULL",[]))), blank_type_info), []))), blank_type_info), [])],
     [mk_expr_st e1])

  | (((Ast_c.Assignment (e1, op, e2)), _), _) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, op, e2), blank_type_info), [])],
     [mk_expr_st ((Ast_c.Assignment (e1, op, (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), blank_type_info), [])), blank_type_info), [])])

  | (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR",ii3))), typ1), ii1), es) ), typ), ii) ->
    if List.length es > 0
    then ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))),
                                          (Ast_c.SimpleAssign, []),
                                          ((Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
          [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])

  | (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("dma_mapping_error",ii3))), typ1), ii1), es) ), typ), ii) ->
    if List.length es > 0
    then ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))),
                                          (Ast_c.SimpleAssign, []),
                                          ((Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
          [mk_expr_st(List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])

  | (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("is_bad",ii3))), typ1), ii1), es) ), typ), ii) ->
    if (List.length es> 0) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st(List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])
  | (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR_OR_NULL",ii3))), typ1), ii1), es) ), typ), ii) ->
    if (List.length es> 0) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])
  | (((Ast_c.Binary   ((((Ast_c.FunCall (e1, es)), typ1), ii1),
                       (Ast_c.Logical Ast_c.Eq, _),(((Ast_c.Unary (e, Ast_c.UnMinus)), typ4), ii4) )), typ), ii)->
    if (List.length es = 1) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else
      ([mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall (e1, es)), typ1), ii1),
                                       (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (((Ast_c.FunCall (e1, es)), typ1), ii1)])

  | (((Ast_c.Binary   ((((Ast_c.FunCall(e1, es)), typ1), ii1),
                       (Ast_c.Logical Ast_c.NotEq, _),(((Ast_c.Unary (e, Ast_c.UnMinus)), typ4), ii4) )), typ), ii)->
    if (List.length es = 1) then
      [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))],
      [mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))),
                                      (Ast_c.SimpleAssign, []),
                                      (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])]
    else
      [mk_expr_st (((Ast_c.FunCall(e1, es)), typ1), ii1)],
      [mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall(e1, es)), typ1), ii1), (Ast_c.SimpleAssign, []),
                                      (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])]
  | ((Ast_c.FunCall (((Ast_c.Ident (Ast_c.RegularName("unlikely",ii3)), typ1), ii1), es), typ), ii) ->
    let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
    invert_st (List.hd args_list)
  | (((Ast_c.Unary    ((((Ast_c.ParenExpr ((((Ast_c.FunCall ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR",ii1))), typ4), ii4), es)), typ3), ii3))), typ2), ii2), Ast_c.Not)), typ), ii) ->
    if (List.length es = 1) then
      ([mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))],
       [mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])
    else ([nst],[nst])
  | (((Ast_c.Unary ((((Ast_c.FunCall  (e, es)), typ1), ii1), Ast_c.Not)), typ), ii)->
    let ptr_args = Def.find_ptr_args_list ((Def.remove_optionlist (Def.create_argslist [] es))) in
    if (List.length ptr_args > 0) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd ptr_args), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else
      ([mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall  (e, es)), typ1), ii1), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [(Ast_c.ExprStatement (None),[])])
  | (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.OrLog, _), e2)), typ), ii)->
    let (norm_exp1, opp_exp1)  = invert_st e1 in
    let (norm_exp2, opp_exp2)  = invert_st e2 in
    ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
  | (((Ast_c.FunCall  (e, es)), typ1), ii1)->
    if (List.length es =0) then
      (
        [mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall  (e, es)), typ1), ii1), (Ast_c.SimpleAssign, []),
                                        (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ1), [])), typ1), [])], [mk_expr_st(((Ast_c.FunCall  (e, es)), typ1), ii1)])
    else if (List.length es >0) then
      ([(Ast_c.ExprStatement (None),[])],
       [(Ast_c.ExprStatement (None),[])])
    else ([nst],[nst])
  | (((Ast_c.Unary    ((((Ast_c.ParenExpr (((Ast_c.FunCall ((e, es)), typ3), ii3))), typ2), ii2), Ast_c.Not)), typ), ii) ->
    if (List.length es = 1) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else
      ([mk_expr_st ((Ast_c.Assignment (((Ast_c.FunCall ((e, es)), typ3), ii3), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st ((Ast_c.FunCall ((e, es)), typ3), ii3)])
  | (((Ast_c.Binary ((((Ast_c.FunCall  (e, es)), typ2), ii2),
                     (Ast_c.Logical Ast_c.Eq, _),(((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii) ->
    if (List.length es = 1) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ1), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])
  | (((Ast_c.Binary ((((Ast_c.FunCall  (e, es)), typ2), ii2),
                     (Ast_c.Logical Ast_c.NotEq, _),(((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii) ->
    if (List.length es = 1) then
      ([mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))],
       [mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign, []),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ1), [])), typ), [])])
    else ([nst],[nst])

  | (((Ast_c.Unary ((((Ast_c.ParenExpr ((((Ast_c.Assignment (e1,
                                                             (Ast_c.SimpleAssign,
                                                             _), e2)), typ2), ii2))), typ1), ii1), Ast_c.Not)), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign, []),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
     [mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign, []), e2), typ), [])])

  | (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.Eq, _),(((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])], [mk_expr_st e11])

  | (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.NotEq, _),(((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii) ->
    ([mk_expr_st e11],[mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])

  | (((Ast_c.Binary ((((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1),
                      ii1),(Ast_c.Logical Ast_c.Eq, _),e11)), typ), ii) ->
    ([mk_expr_st((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
     [mk_expr_st e11])
  | (((Ast_c.Binary ((((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1),
                      ii1),(Ast_c.Logical Ast_c.NotEq, _),e11)), typ), ii) ->
    ([mk_expr_st e11],[mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])

  | (((Ast_c.Binary   (((((Ast_c.FunCall  (e, es)), typ2), ii2)),
                       (Ast_c.Logical Ast_c.Eq, _), ((Ast_c.Constant (Ast_c.Int ("0", _)), typ1), ii1))), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e, (Ast_c.SimpleAssign, []),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
     [(Ast_c.ExprStatement (None),[])])
  | (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.Eq, _), ((Ast_c.Constant (Ast_c.Int ("0", _)), typ1), ii1))), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign, []),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],[mk_expr_st e1])

  | (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.Inf, _), ((Ast_c.Constant (Ast_c.Int ("0", _)), typ1), ii1))), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign, []),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],[mk_expr_st e1])

  | (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.NotEq, _), ((Ast_c.Constant (Ast_c.Int ("0", _)), typ1), ii1))), typ), ii) ->
    ([mk_expr_st e1], [mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign, []), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])
  | (((Ast_c.ParenExpr ((((Ast_c.Unary (e11, Ast_c.Not)), typ), ii))), typ1), ii1) ->
    ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])], [mk_expr_st e11])
  | (((Ast_c.Unary ((((Ast_c.ParenExpr (e11)), typ1), ii1), Ast_c.Not)), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ1), [])), typ1), [])],[mk_expr_st e11])

  | (((Ast_c.Unary (e11, Ast_c.Not)), typ), ii)  ->
    (match (Def.is_pointer e11) with
       Def.IsPtr  ->
       ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []),
                                        (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],[mk_expr_st e11])
     | Def.UnknownType ->

       ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []),
                                        (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],[mk_expr_st e11])

     | Def.IsntPtr->
       ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []),
                                        (( Ast_c.Ident (Ast_c.RegularName("0",[])), typ), [])), typ), [])],[mk_expr_st e11]))

  | (((Ast_c.Binary ((((Ast_c.FunCall  (e, es)), typ1), ii1), (Ast_c.Logical
                                                                 Ast_c.Eq, _),e22)), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall  (e, es)), typ1), ii1), (Ast_c.SimpleAssign, []),e22), typ), [])],
     [])

  | (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.Eq, _),e22)), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign, []),e22), typ), [])],[mk_expr_st e11])
  | ((exp, typ), ii)->
    ([mk_expr_st ((exp, typ), ii)],
     [mk_expr_st ((Ast_c.Assignment (((exp, typ), ii), (Ast_c.SimpleAssign, []), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])


let rec generate_exe_paths_simple fin_lineno exe_paths c_function =
  let {ast = prog; init_labels = init_labels; cfg = cfg; labels = lbl_list} = c_function in
  match prog with
    [] -> exe_paths
  | h::t->
    let start_line = Def.find_startline_no (Def.create_stmtlist h) in
    let end_line   = Def.find_endline_no   (Def.create_stmtlist h) in

    let generate_exe_paths_simple_aux st =
      if (fin_lineno >= start_line && fin_lineno <= end_line)
      then
        let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st)} in
        generate_exe_paths_simple fin_lineno new_exe_paths
          {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}
      else
      if not(List.exists is_return_or_goto (Def.create_stmtlist st))
      then
        let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st)} in
        generate_exe_paths_simple fin_lineno new_exe_paths
          {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}
      else generate_exe_paths_simple fin_lineno exe_paths
          {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}
    in
    if fin_lineno >= start_line
    then
      match Ast_c.unwrap h with
      | Ast_c.Labeled (Ast_c.Label (_, st))
      | Ast_c.Labeled (Ast_c.Case  (_, st))
      | Ast_c.Labeled (Ast_c.CaseRange (_, _, st))
      | Ast_c.Iteration (Ast_c.For (Ast_c.ForExp _, _, _, st))
      | Ast_c.Iteration  (Ast_c.MacroIteration (_, _, st))
      | Ast_c.Labeled (Ast_c.Default st) ->
        generate_exe_paths_simple_aux st

      | Ast_c.Compound statxs ->
        generate_exe_paths_simple_aux h

      | Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
        let (norm_exp,opp_exp) = (
          match e with
            (((Ast_c.Binary (e1, (Ast_c.Logical Ast_c.OrLog, _), e2)), typex), iiex) ->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          | (((Ast_c.Binary (e1, (Ast_c.Logical Ast_c.AndLog, _), e2)), typex), iiex) ->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          | _ -> invert_st e
        ) in

        let exe_paths1 = if (List.length exe_paths) = 0 then (exe_paths@[norm_exp])
          else (map_append norm_exp exe_paths) in
        let exe_paths2 = if (List.length exe_paths) = 0 then (exe_paths@[opp_exp])
          else (map_append opp_exp exe_paths)
        in
        let st2_new = if(has_multiple_st (Def.create_stmtlist st2)) then (Def.create_stmtlist st2) else [] in
        let start_line_inner1 = Def.find_startline_no (Def.create_stmtlist st1) in
        let end_line_inner1 =   Def.find_endline_no (Def.create_stmtlist st1) in
        let start_line_inner2 = Def.find_startline_no (Def.create_stmtlist st2) in
        let end_line_inner2 =   Def.find_endline_no (Def.create_stmtlist st2) in
        if fin_lineno = start_line_inner1 then(
          let goto_code = gather_goto_code c_function [] (Def.create_stmtlist st1) in
          let exe_paths1 =   if (List.length exe_paths1) = 0 then (exe_paths1@[goto_code])
            else (map_append goto_code exe_paths1)
          in
          exe_paths1)


        else if fin_lineno = start_line_inner2 then
          let goto_code = gather_goto_code c_function [] (Def.create_stmtlist st2) in
          let exe_paths2 =   if (List.length exe_paths2) = 0 then (exe_paths2@[goto_code])
            else (map_append goto_code exe_paths2)
          in
          exe_paths2


        else if (fin_lineno>= start_line_inner1 && fin_lineno <= end_line_inner1)
        then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st1)} in
          generate_exe_paths_simple fin_lineno new_exe_paths1
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}

        else if (fin_lineno>= start_line_inner2 && fin_lineno <= end_line_inner2)
        then
          let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths2
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st2)} in
          generate_exe_paths_simple fin_lineno new_exe_paths2
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st1))) && (not(List.exists is_return_or_goto (Def.create_stmtlist st2))) && ((List.length st2_new) >0)then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st1)} in
          generate_exe_paths_simple fin_lineno (new_exe_paths1)
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st1))) && (not(List.exists is_return_or_goto (Def.create_stmtlist st2))) && ((List.length st2_new) =0)then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st1)} in
          generate_exe_paths_simple fin_lineno (new_exe_paths1)
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st1))) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st1)} in
          generate_exe_paths_simple fin_lineno new_exe_paths1
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st2))) && ((List.length st2_new) >0)  then(
          let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths2
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st2)} in
          generate_exe_paths_simple fin_lineno new_exe_paths2
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t})

        else generate_exe_paths_simple fin_lineno exe_paths2
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}


      |  Ast_c.Selection  (Ast_c.Switch (e, st)) ->
        let exe_paths1 = if (List.length exe_paths) = 0 then (exe_paths@[[((Ast_c.ExprStatement (Some e)),[])]])
          else (map_append [((Ast_c.ExprStatement (Some e)),[])] exe_paths)
        in
        let start_line_inner = Def.find_startline_no (Def.create_stmtlist st) in
        let end_line_inner =   Def.find_startline_no (Def.create_stmtlist st) in
        if(fin_lineno>= start_line_inner && fin_lineno <= end_line_inner) then
          let case_stmts = find_case_stmts fin_lineno []  (Def.create_stmtlist st) in
          let new_exe_paths  = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = case_stmts}  in
          generate_exe_paths_simple fin_lineno new_exe_paths
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let switch_st_compound = create_compound_st_for_switch e [] [] (Def.create_stmtlist st) in

          let new_exe_paths = generate_exe_paths_simple fin_lineno []
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = switch_st_compound} in
          let switch_exe = if(List.length new_exe_paths =0) then exe_paths1
            else exe_path_for_switch [] new_exe_paths exe_paths1
          in

          generate_exe_paths_simple fin_lineno switch_exe
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}
        else generate_exe_paths_simple fin_lineno exe_paths1
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}
      |  Ast_c.Iteration  (Ast_c.While (e, st)) ->

        let (norm_exp,opp_exp) = (
          match e with
            (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.OrLog, _), e2)), typex), iiex)->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          |  (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.AndLog, _), e2)), typex), iiex)->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          |  _->let (norm_exp,opp_exp)= invert_st e in
            (norm_exp,opp_exp)
        ) in


        let exe_paths1 = if (List.length exe_paths) = 0 then (exe_paths@[norm_exp])
          else (map_append norm_exp exe_paths)
        in
        let exe_paths2 = if (List.length exe_paths) = 0 then (exe_paths@[opp_exp])
          else (map_append opp_exp exe_paths)
        in
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = (Def.create_stmtlist st)} in
          generate_exe_paths_simple fin_lineno new_exe_paths
            {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = t}

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = (Def.create_stmtlist st)} in
          generate_exe_paths_simple fin_lineno (new_exe_paths1)
            {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = t}
        else ( let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths2
                   {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = []} in
               generate_exe_paths_simple fin_lineno new_exe_paths2
                 {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = t}
             )
      |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->
        let (norm_exp,opp_exp) = (
          match e with
            (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.OrLog, _), e2)), typex), iiex)->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          |  (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.AndLog, _), e2)), typex), iiex)->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          |  _->let (norm_exp,opp_exp)= invert_st e in
            (norm_exp,opp_exp)
        ) in


        let exe_paths1 = if (List.length exe_paths) = 0 then (exe_paths@[norm_exp])
          else (map_append norm_exp exe_paths)
        in
        let exe_paths2 = if (List.length exe_paths) = 0 then (exe_paths@[opp_exp])
          else (map_append opp_exp exe_paths)
        in
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st)} in
          generate_exe_paths_simple fin_lineno new_exe_paths
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1
              {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = (Def.create_stmtlist st)} in
          generate_exe_paths_simple fin_lineno (new_exe_paths1)
            {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = t}
        else ( let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths2
                   {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = []} in
               generate_exe_paths_simple fin_lineno new_exe_paths2
                 {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = t}
             )


      | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
        failwith "for loop with declaration in first argument not supported"
      |   Ast_c.Jump (Ast_c.Goto name) ->
        generate_exe_paths_simple fin_lineno exe_paths
          {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = t}

      |   Ast_c.Jump (Ast_c.Return) -> exe_paths
      |   Ast_c.Jump (Ast_c.ReturnExpr e) -> exe_paths
      |   Ast_c.Jump (Ast_c.GotoComputed e) -> generate_exe_paths_simple fin_lineno exe_paths
                                                 {labels = lbl_list; cfg = cfg; init_labels = init_labels; ast = t}
      | _->
        if(List.length exe_paths) = 0 then
          generate_exe_paths_simple fin_lineno (exe_paths@[[h]])
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}
        else
          generate_exe_paths_simple fin_lineno (map_append [h] exe_paths)
            {labels = lbl_list; init_labels = init_labels; cfg = cfg; ast = t}

    else exe_paths

let is_following_code_access_exp exp point_lineno follow final_return c_function =
  let {ast = ast} = c_function in
  let rec is_following_code_access_exp_aux follow =
    function
      []   -> follow
    | h::t ->
      if final_return > (Def.find_startline_no (Def.create_stmtlist h)) then
        (
          match Ast_c.unwrap h with

          |  Ast_c.Labeled (Ast_c.Label (name, st)) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.Labeled (Ast_c.Case  (e, st)) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.Labeled (Ast_c.Default st) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.Compound statxs ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist h) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.ExprStatement (Some e) ->
            if (any_var_access_stmt h exp) &&
               (point_lineno < (Def.find_startline_no (Def.create_stmtlist h)))
            then
              is_following_code_access_exp_aux true t
            else is_following_code_access_exp_aux follow t
          |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
            let new_follow1 =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st1) in
            let new_follow2 =
              is_following_code_access_exp_aux new_follow1 (Def.create_stmtlist st2) in
            let new_follow3 =
              is_following_code_access_exp_aux new_follow2 ([(Ast_c.ExprStatement (Some e),[])]) in
            is_following_code_access_exp_aux new_follow3 t
          |  Ast_c.Selection  (Ast_c.Switch (e, st)) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.Iteration  (Ast_c.While (e, st)) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
            failwith "for loop with declaration in first argument not supported"
          |  Ast_c.Iteration  (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) ->
            let new_follow =
              is_following_code_access_exp_aux follow (Def.create_stmtlist st) in
            is_following_code_access_exp_aux new_follow t
          |  Ast_c.Jump (Ast_c.Goto name) ->
            is_following_code_access_exp_aux follow t
          |  Ast_c.Jump (Ast_c.Return) ->
            is_following_code_access_exp_aux follow t

          |  Ast_c.Jump (Ast_c.ReturnExpr e) ->
            if (any_var_access_stmt h exp) &&
               (point_lineno < (Def.find_startline_no (Def.create_stmtlist h)))
            then
              is_following_code_access_exp_aux true t
            else is_following_code_access_exp_aux follow t

          |  Ast_c.Jump (Ast_c.GotoComputed e) ->
            is_following_code_access_exp_aux follow t
          |  Ast_c.Decl decl ->
            is_following_code_access_exp_aux follow t
          |  Ast_c.Asm asmbody ->
            is_following_code_access_exp_aux follow t
          |  Ast_c.NestedFunc def ->
            is_following_code_access_exp_aux follow t
          |  Ast_c.MacroStmt ->
            is_following_code_access_exp_aux follow t
          | _-> is_following_code_access_exp_aux follow t

        )
      else follow
  in
  is_following_code_access_exp_aux follow ast

(* TODO TEMP *)
let create_block c_function index =
  let {cfg = cfg} = c_function in
  let node = cfg#nodes#assoc index in
  let {Annotated_cfg.parser_node = parser_node} = node in
  let [(if_index, _)] = (cfg#predecessors index)#tolist in
  let {Annotated_cfg.parser_node = if_parser_node} = cfg#nodes#assoc if_index in

  let Some full_if_statement =
    Control_flow_c.extract_fullstatement if_parser_node
  in

  let (if_expression, st1, st2) =
    match Ast_c.unwrap full_if_statement with
      Ast_c.Selection (Ast_c.If (e, st1, st2)) -> (e, st1, st2)
    | _ -> failwith "should be a if statement"
  in

  let (st, part) =
    match Control_flow_c.unwrap parser_node with
      Control_flow_c.TrueNode _ -> (st1, Def.Then)
    | Control_flow_c.FalseNode
    | Control_flow_c.FallThroughNode -> (st2, Def.Else)
    | _ -> failwith "this should be the head of a error handling branch"
  in
  (***************************)

  let st_normal = Def.create_stmtlist st in
  let stmt_list_st = gather_goto_code c_function [] st_normal in
  let h_normal = Def.create_stmtlist full_if_statement in

  let branch_st_lineno = Def.find_startline_no st_normal in
  let branch_en_lineno = Def.find_endline_no stmt_list_st in
  let goto = Def.goto_exists_in_list st_normal in
  Block.mk_block (index, node) (Def.find_startline_no h_normal) if_expression goto
    st_normal part branch_st_lineno branch_en_lineno stmt_list_st
(************)

let find_errorhandling c_function =
  let {cfg = cfg} = c_function in
  let branch_heads = Annotated_cfg.get_error_handling_branch_head cfg in
  Graph_operations.NodeiSet.fold
    (fun index acc -> (create_block c_function index)::acc) branch_heads []


let rec stmt_exists_in_list st = function
    []-> false
  |  h::t -> if (Def.find_startline_no [st]) = (Def.find_startline_no [h])  then true
    else stmt_exists_in_list st t

let rec stmt_exists_in_any_list st = function
    []-> false
  |    h::t -> if(stmt_exists_in_list st h) then true
    else  stmt_exists_in_any_list st t


let rr_in_exe_paths_init_lbl rls c_function last_func =
  let {init_labels = init_labels} = c_function in
  let blk_strtlineno = Def.find_startline_no init_labels in
  let exe_paths_list () = generate_exe_paths_simple
      (blk_strtlineno-1) [] c_function in
  Def.stmt_exists_in_list rls init_labels &&
  stmt_exists_in_any_list last_func (exe_paths_list ())

let rec find_ptr_args_list = function
    []->[]
  | h::t->
    match (Def.is_pointer h) with
      Def.IsPtr       -> h::find_ptr_args_list t
    | Def.UnknownType -> h::find_ptr_args_list t
    | _               -> find_ptr_args_list t


let rec assign_var_is_null var = function
    []-> false
  | h::t-> match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.Ident (Ast_c.RegularName("NULL",ii2))), typ1), ii1))), typ), ii)) ->
      if (Def.compare_exps var e1) then true
      else assign_var_is_null var t
    |_-> assign_var_is_null var t


let rec args_list_contain_id id = function
    []-> false
  | h::t-> match h with
      (((Ast_c.Unary (e, Ast_c.GetRef)), typ), ii)-> if (Def.compare_exps id e) then true
      else args_list_contain_id id t
    |_-> args_list_contain_id id t


let rec remove_string_args = function
    []-> []
  | h::t-> if(Def.string_exists_in_explist [h]) then
      remove_string_args t
    else h::(remove_string_args t)


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

let filter_null_out =
  let filter_null_out_aux acc expr =
    match expr with
      (((Ast_c.Ident (Ast_c.RegularName("NULL", _))), _), _) -> acc
    | _ -> expr::acc
  in
  List.fold_left filter_null_out_aux []

let rec find_recent_id_values_paths_inner_second id id_value  =  function
    [] -> id_value
  | h::t ->
    match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
      if Def.compare_exps id e1
      then find_recent_id_values_paths_inner_second id (Some e2) t
      else find_recent_id_values_paths_inner_second id id_value  t
    | _ -> find_recent_id_values_paths_inner_second id id_value  t


let rec find_recent_id_values_paths_second id values = function
    []   -> values
  | h::t ->
    let id_value = find_recent_id_values_paths_inner_second id None h in
    match id_value with
      None   -> find_recent_id_values_paths_second id values t
    | Some a -> find_recent_id_values_paths_second id (a::values) t



(* Return the list of last assigned valeus for each path when only one argument
 * has a list *)
let find_idvalues_list exe_paths_list id_values args_list =
  let find_idvalues_list_aux el =
    find_recent_id_values_paths_second el [] exe_paths_list in
  let all_id_values = List.map find_idvalues_list_aux args_list in
  let tmp = Def.filter_empty_list_out all_id_values in
  match tmp with
    [id_values] -> id_values
  | _           -> []


let rec find_recent_id_values_paths id values = function
    []   -> values
  | h::t ->
    let id_value =
      find_recent_id_values_paths_inner id None false false h in
    match id_value with
      None   -> find_recent_id_values_paths id values      t
    | Some a -> find_recent_id_values_paths id (a::values) t

(* FYI the empty list is used as None because reasons ... -_-' *)
let get_identifier_values c_function b offset args_list =
  let fin_lineno = Block.extract_branch_start b in
  let exe_paths_list = generate_exe_paths_simple (fin_lineno + offset)
      [] c_function in

  let id_values =
    match args_list with
      [h] -> find_recent_id_values_paths h [] exe_paths_list
    | _   -> find_idvalues_list exe_paths_list [] args_list
  in

  let id_values = filter_null_out id_values in
  (* TODO
     let id_values =
      if same_id_values id_values &&
         List.length id_values != 0
      then [(List.hd id_values)]
      else [] in
  *)
  id_values


let rec refine_ref_list args_list = function
    []       -> []
  | (a,b)::t ->
    if Def.compare_exps a (List.hd args_list)
    then (a,b)::(refine_ref_list args_list t)
    else refine_ref_list args_list t

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

let rec any_ref_var_access_stmt args_list = function
    []-> false
  | h::t->
    match h with
      (((Ast_c.Ident (Ast_c.RegularName("NULL",ii2))), typ), ii)-> any_ref_var_access_stmt args_list t
    |_->
      if (Def.exp_exists_in_list h args_list) then (true)
      else (any_ref_var_access_stmt args_list t)

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

let rec any_var_access_stmt args_list = function
    [] -> false
  | (var,ref_vars)::t->
    if (Def.exp_exists_in_list var args_list)
    then (ref_var_mark:= false; true)
    else if any_ref_var_access_stmt args_list ref_vars
    then (ref_var_mark:= true;  true)
    else any_var_access_stmt args_list t

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


let rec rr_in_exe_paths_new rls c_function last_func = function
    [] -> false
  | block::t->
    if Block.does_block_contains_statement block rls
    then
      let blk_strtlineno = Block.extract_branch_start block in
      let exe_paths_list = generate_exe_paths_simple (blk_strtlineno-1) []
          c_function in
      if stmt_exists_in_any_list last_func exe_paths_list
      then true
      else rr_in_exe_paths_new rls c_function last_func t
    else rr_in_exe_paths_new rls c_function last_func t


let check_each_path c_function alloc rr args args_list block final_return
    errblk_list =
  let branch_lineno = Block.extract_branch_start block in
  let test_case = Block.extract_test_case block in
  let stmtlist = Block.extract_statements block in
  let exe_paths_lists = generate_exe_paths_simple branch_lineno
      [] c_function in
  let rec check_each_path_aux = function
      [] -> false
    | path::rest ->
      let t_ref_vars = gather_all_ref_var path in
      let ref_vars = refine_ref_list args t_ref_vars in
      let last_access =
        find_last_access (List.hd args) ref_vars None path in
      let tmp =
        if Def.exp_exists_in_stmt (Some (List.hd args)) (Ast_c.ExprStatement (Some test_case), [])
        then
          match test_case with
            (((Ast_c.Binary ((((Ast_c.ParenExpr (((Ast_c.Assignment _),
                                                  _), _)), _), _), _, _)),
              _), _) ->
            let follow = is_following_code_access_exp
                (((List.hd args_list),[])::ref_vars ) branch_lineno false
                final_return c_function in
            not follow
          | _ -> false
        else false in
      if tmp then
        false
      else
        match last_access with
          None ->
          (List.length rest) = 0 ||
          check_each_path_aux rest
        | Some access ->
          (match Ast_c.unwrap access with
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  _), _), _)) ->
             alloc = access ||
             not (Def.compare_stmts rr access ||
                  stmt_exists_in_list access stmtlist) &&
             (rr_in_exe_paths_new rr c_function access errblk_list ||
              rr_in_exe_paths_init_lbl rr c_function access ||
              check_each_path_aux rest)
           | Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, _,
                                                            ((Ast_c.FunCall
                                                                _, _), _))),
                                         _), _)) ->
             not (stmt_exists_in_list access stmtlist) &&
             ((Def.exp_exists_in_stmt (Some e1)
                 (Ast_c.ExprStatement (Some test_case), []) &&
               is_following_code_access_exp
                 ((List.hd args, [])::ref_vars) branch_lineno false
                 final_return c_function) ||
              rr_in_exe_paths_new rr c_function access errblk_list ||
              check_each_path_aux rest)
           | Ast_c.ExprStatement (Some (((Ast_c.Assignment (_, _, (((Ast_c.CondExpr ((((Ast_c.FunCall ((((Ast_c.Ident (Ast_c.RegularName ("IS_ERR", _))), _), _), es10)), _), _), _, _)), _), _))), _), _)) ->
             let args_list10 =
               Def.remove_optionlist (Def.create_argslist [] es10) in
             not (Def.compare_exps (List.hd args_list10) (List.hd args)) &&
             check_each_path_aux rest
           | _ -> check_each_path_aux rest)
  in
  check_each_path_aux exe_paths_lists

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


let rec find_common_rrwa_in_model_path_inner list = function
    []->[]
  | h::t-> if(Def.stmt_exists_in_all_list h list) then
      h::(find_common_rrwa_in_model_path_inner list t)
    else find_common_rrwa_in_model_path_inner list t


let find_common_rrwa_in_model_path = function
    []->[]
  | h::t-> (find_common_rrwa_in_model_path_inner  t h)


let find_possible_alloc_with_given_argument c_function model_block args_list =
  let model_blk_st_no =
    match model_block with
      Some model_blk -> Block.extract_branch_start model_blk
    | None -> 0
  in
  let exe_paths_model = generate_exe_paths_simple
      (model_blk_st_no - 1) [] c_function in
  let all_pos_alloc_can = find_all_poss_alloc_main args_list []
      exe_paths_model in
  find_common_rrwa_in_model_path all_pos_alloc_can



let rec stmt_exists_in_exe_paths_inner stmt = function
    []-> false
  | h::t -> if (Def.compare_stmts h stmt) then true
    else stmt_exists_in_exe_paths_inner stmt t

let rec stmt_exists_in_exe_paths stmt = function
    []-> false
  | h::t-> if stmt_exists_in_exe_paths_inner stmt h then true
    else stmt_exists_in_exe_paths stmt t

let rec find_actual_alloc exe_paths_candidate = function
    []   -> false
  | h::t ->
    if not (stmt_exists_in_exe_paths h exe_paths_candidate)
    then
      true
    else find_actual_alloc exe_paths_candidate t


let is_resource_allocation_the_same c_function model_block release_block args_list =
  let possible_alloc =
    find_possible_alloc_with_given_argument c_function model_block args_list
  in
  let miss_line = Block.extract_branch_start release_block in
  let exe_paths_candidate = generate_exe_paths_simple
      miss_line [] c_function in
  find_actual_alloc exe_paths_candidate possible_alloc

(* Return a list of block starting line that does not release the resource *)
let no_previous_blk_has_rrl miss_block rrl alloc_line =
  let no_previous_blk_has_rrl_aux block =
    Block.compare_branch_start block alloc_line > 0 &&
    Block.compare_branch_end_with_start block miss_block < 0 &&
    not (Block.does_block_contains_statement block rrl)
  in
  List.filter no_previous_blk_has_rrl_aux

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


let is_resource_allocated_properly block error_blocks c_function
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
          error_blocks in
      if List.length unreleasing_blocks > 0
      then
        let model_blk = find_app_model_blk rrl block error_blocks in
        not (
          is_resource_allocation_the_same c_function model_blk block
            args_list)
      else true
    else true
  | _ -> false



let rec find_ptr_args_list = function
    []->[]
  |   h::t-> match (Def.is_pointer h) with
      Def.IsPtr->h::find_ptr_args_list t
    |   Def.UnknownType-> h::find_ptr_args_list t
    |   _-> find_ptr_args_list t

let defined_alloc id =
  if id =~ "__dev_get_by_name" then true
  else if id =~ "wiphy_to_dev" then true
  else if id =~ "container_of" then true
  else if id =~ "i2c_get_adapdata" then true
  else if id =~ "snd_lookup_oss_minor_data" then true
  else if id =~ "snd_lookup_minor_data" then true
  else if id =~ "pci_get_drvdata" then true
  else if id =~ "dev_get_by_name" then true
  else if id =~ "l2cap_load" then true
  else if id =~ "platform_get_irq" then true
  else if id =~ "wpan_phy_find" then true
  else if id =~ "nla_nest_start" then true
  else if id =~ "sx_init_drivers" then true
  else if id =~ "clk_get" then true
  else if id =~ "irq_of_parse_and_map" then true
  else if id =~ "netdev_priv" then true
  else false


let is_defined_alloc alloc =
  match Ast_c.unwrap alloc with
  | Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident
                                                     (Ast_c.RegularName (id,
                                                                         _))),
                                                  _), _), _)), _), _)) ->
    defined_alloc id
  | _-> false


let is_resource_released block error_blocks c_function
    (Resource.Resource (alloc, args, rr)) =
  match Ast_c.unwrap rr with
    Ast_c.ExprStatement (Some (((Ast_c.FunCall  (_, es)), _), _)) ->
    let final_return = find_final_return c_function in
    let args_list =
      find_ptr_args_list (Def.remove_optionlist (Def.create_argslist [] es)) in
    not (
      (match List.hd args_list with
         ((Ast_c.Ident ident, _), _)
       | (((Ast_c.RecordAccess (((Ast_c.Ident ident, _), _), _)), _), _)
       | (((Ast_c.RecordPtAccess (((Ast_c.Ident ident, _), _), _)), _), _) ->
         not (is_locally_main block ident true c_function)
       | _ -> false) ||
      Block.does_block_contains_statement block rr ||
      List.exists (Block.contains_expression block) args ||
      is_defined_alloc alloc) &&
    check_each_path c_function alloc rr args args_list block
      final_return error_blocks
  | _ -> false

let resource_of_release_temp block error_blocks c_function
    (Resource.Resource (alloc, args_list, _)) =

  let id_values1 = get_identifier_values c_function block 0 args_list in

  let alloc_call =
    match alloc with
      (Ast_c.ExprStatement (Some id), _) -> id
  in

  if List.length id_values1 = 1
  then
    match (List.hd id_values1, alloc_call) with
      ((((Ast_c.FunCall (_, _)), _), _) as e1,
       (((Ast_c.Cast (_, ((((Ast_c.FunCall  (_, _)), _), _) as e2))), _),
        _))
    | ((((Ast_c.FunCall (_, _)), _), _) as e1,
       ((((Ast_c.FunCall (_, _)), _), _) as e2))
    | ((((Ast_c.Cast (_, ((((Ast_c.FunCall  (_, _)), _), _) as e1))), _),
        _),
       ((((Ast_c.FunCall (_, _)), _), _) as e2))
    | ((((Ast_c.Cast (_, ((((Ast_c.FunCall (_, _)), _), _) as e1))), _),
        _),
       (((Ast_c.Cast (_, ((((Ast_c.FunCall (_, _)), _), _) as e2))), _), _))
      -> Def.compare_exps e1 e2
    | _ -> false
  else false


let option_map f lst =
  let aux acc x = match f x with
    | Some item -> item :: acc
    | None -> acc in
  List.rev (List.fold_left aux [] lst)

let get_resources c_function error_blocks =

  let get_resource_release acc block = Block.get_resource_release block acc in
  let releases = List.fold_left get_resource_release [] error_blocks in

  (* this -1 offset is really weird ... *)
  let resource_of_release (Resource.Release (args_list, h), model_blk) =
    let id_values = get_identifier_values c_function model_blk (-1) args_list in

    if List.length id_values = 1
    then
      let id = List.hd id_values in
      let new_alloc = (Ast_c.ExprStatement (Some id), []) in
      Some (Resource.Resource (new_alloc, args_list, h))
    else
      None
  in
  option_map resource_of_release releases
