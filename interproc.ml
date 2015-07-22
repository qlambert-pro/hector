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

(* Whether return statement exists in the statement list *)

let rec return_exists_in_list = function
    []-> false
  |  h::t ->  match Ast_c.unwrap h with
      Ast_c.Jump (Ast_c.ReturnExpr e) -> true
    |  Ast_c.Jump (Ast_c.Return ) -> true
    |  _ -> return_exists_in_list t



let rec refine_args_list list  = function
    []->list
  |  h::t-> match h with
      (((Ast_c.Ident ( Ast_c.RegularName (s, ii2))), typ), ii)-> (refine_args_list (list@[s]) t)
    |  ((( Ast_c.RecordAccess   ((((Ast_c.Ident (Ast_c.RegularName (s1, ii3))), typ1), ii1), (Ast_c.RegularName (s2, ii2)))), typ), ii) ->
      (refine_args_list (list@[s1]@[s2]) t)
    |   ((( Ast_c.RecordPtAccess   ((((Ast_c.Ident (Ast_c.RegularName (s1, ii3))), typ1), ii1), (Ast_c.RegularName (s2, ii2)))), typ), ii) ->
      (refine_args_list (list@[s1]@[s2]) t)
    | _->  refine_args_list list t


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


let rec func_contains_rr miss_st args_list = function
    []-> false
  | h::t->
    begin
      match Ast_c.unwrap h with
        Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
        let args_list1 = Def.create_argslist [] es in
        if (refine_args_list [] (Def.remove_optionlist args_list)) =  (refine_args_list [] (Def.remove_optionlist args_list1)) then  (true)
        else ( func_contains_rr miss_st args_list t)
      | _->func_contains_rr miss_st args_list t 
    end



let find_all_interproc_func func_name args_list miss_st =
  let find_all_interproc_func_aux acc (name, paramset, stmts) =
    if not (Def.compare_names name func_name) &&
       (List.length stmts) < 12 
    then 
      if func_contains_rr miss_st args_list stmts
      then (name,paramset,stmts)::acc
      else acc 
    else acc 
  in
  List.fold_left find_all_interproc_func_aux []



let rec find_app_model_blk miss_st diff model model_line miss_blk = function
    []->(model_line, model)
  |   (brnch_strtlineno,test_case,typ,blk_strtlineno,blk_endlineno,stmtlist)::t->
    if(Def.stmt_exists_in_list miss_st stmtlist) then
      if (miss_blk - blk_strtlineno) < 0 && diff = 0 then (blk_strtlineno, stmtlist)
      else if (miss_blk - blk_strtlineno) > diff then find_app_model_blk miss_st (miss_blk - blk_strtlineno) stmtlist blk_strtlineno miss_blk t
      else find_app_model_blk miss_st diff model model_line miss_blk t
    else find_app_model_blk miss_st diff model model_line miss_blk t


let rec name_exists_in_exepaths name exists = function 
    []-> exists
  | h::t-> match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.Ident (ident)), typ), ii)) ->


      if Def.compare_names ident name then  name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t 
    | Ast_c.ExprStatement (Some (((Ast_c.FunCall (((((Ast_c.Ident (ident)), typ1), ii1)), es)), typ), ii)) ->
      let args_list = Def.create_argslist [] es in
      let args_list = List.map (function arg -> (Ast_c.ExprStatement (Some arg),[])) (Def.remove_optionlist args_list) in

      if Def.compare_names ident name then name_exists_in_exepaths name true t
      else if name_exists_in_exepaths name exists args_list then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t
    |  Ast_c.ExprStatement (Some ((((Ast_c.CondExpr (e1, e2, e3))), typ), ii)) ->
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
      else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement e2,[])] then name_exists_in_exepaths name true t
      else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e3),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t

    |  Ast_c.ExprStatement (Some ((((Ast_c.Sequence (e1, e2))), typ), ii)) ->
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
      else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e2),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t  

    |  Ast_c.ExprStatement (Some ((((Ast_c.Assignment (e1, op, e2))), typ), ii)) ->
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
      else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e2),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t  

    |  Ast_c.ExprStatement (Some ((((Ast_c.Postfix  (e, op)), typ), ii)))  -> 
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t 

    |  Ast_c.ExprStatement (Some ((((Ast_c.Infix    (e, op)), typ), ii)))   ->  
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t 

    |  Ast_c.ExprStatement (Some (((Ast_c.Unary    (e, op)), typ), ii)) -> 
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t 

    |  Ast_c.ExprStatement (Some (((Ast_c.Binary   (e1, op, e2), typ), ii)))   -> 
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
      else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e2),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t

    |  Ast_c.ExprStatement (Some (((Ast_c.ArrayAccess    (e1, e2)), typ), ii)) ->
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
      else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e2),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t

    |   Ast_c.ExprStatement (Some (((Ast_c.RecordAccess (e, name1)), typ), ii)) ->
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t

    |  Ast_c.ExprStatement (Some (((Ast_c.RecordPtAccess (e, name2)), typ), ii)) ->
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t  

    |  Ast_c.ExprStatement (Some (((Ast_c.SizeOfExpr  (e) ), typ), ii)) ->
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t


    |  Ast_c.ExprStatement (Some (((Ast_c.ParenExpr (e)), typ), ii)) ->          
      if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
      else name_exists_in_exepaths name exists t 
    | _-> name_exists_in_exepaths name exists t

let rec name_exists_in_exepaths_main name = function
    []-> false 
  | h::t-> if name_exists_in_exepaths name false h then true
    else name_exists_in_exepaths_main name t

let rec locally_defined ident = function
    []-> false
  |  (name,paramset,stmts)::rest -> if (Def.compare_names name ident) then true
    else locally_defined ident rest

let interproc_new func_name all_function miss_lineno errblks prog lbl_list =
  let interproc_new_aux acc (alloc, args, h) =
    match Ast_c.unwrap h with  
      Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident ident), _), _),
                                                  es)), _), _)) ->  
      let args_list = Def.create_argslist [] es in 
      if (args_list = []) &&
         locally_defined ident all_function
      then 
        acc
      else
      if (args_list = []) &&
         (match Ast_c.unwrap alloc with
            Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident
                                                             ident10), _),
                                                         _), es10)), _), _)) ->
            let args_list10 = Def.create_argslist [] es10 in
            (args_list10 = []) && locally_defined ident10 all_function
          | _ -> false)
      then acc
      else
        let interproc_funcs = [] (*find_all_interproc_func func_name args_list h
                                all_function*) in
        if interproc_funcs != []
        then
          let (model_lineno, nearest_model) = find_app_model_blk h 0 [] 0 miss_lineno errblks in 
          let model_exe = Errorhandling.generate_exe_paths_simple model_lineno [] lbl_list prog in
          let candidate_exe = Errorhandling.generate_exe_paths_simple miss_lineno [] lbl_list prog in
          let rec loop = function
              [] -> (alloc, args, h)::acc
            | (name, paramset, stmts)::rest -> 
              if name_exists_in_exepaths_main name candidate_exe &&
                 not (name_exists_in_exepaths_main name model_exe)
              then
                acc 
              else loop rest
          in loop interproc_funcs
        else (alloc, args, h)::acc
    | _ -> acc 
  in
  List.fold_left interproc_new_aux []


let rec gather_all_callin_func line_no lbl_list = function
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
        then gather_all_callin_func line_no lbl_list (Def.create_stmtlist st)
        else
        if line_no > end_lineno &&
           not (Def.boolean_of_option (Def.goto_exists_in_list (Def.create_stmtlist st))) &&
           not(return_exists_in_list (Def.create_stmtlist st))
        then
          gather_all_callin_func line_no lbl_list (Def.create_stmtlist st)
        else
        if line_no < end_lineno
        then []
        else gather_all_callin_func line_no lbl_list t

      |	Ast_c.ExprStatement (Some (((Ast_c.Assignment (_, _, (((Ast_c.FunCall
                                                                  _), _), _))),
                                   _), _))
      | Ast_c.ExprStatement (Some (((Ast_c.FunCall  _), _), _)) ->
        ((Def.find_startline_no (Def.create_stmtlist h)),h)::
        (gather_all_callin_func line_no lbl_list t)
      |	Ast_c.ExprStatement (Some (((Ast_c.Assignment (_, _, e2)), _), _))->
        ((Def.find_startline_no (Def.create_stmtlist h)),
         (Ast_c.ExprStatement (Some e2), []))::
        (gather_all_callin_func line_no  lbl_list t)
      |	Ast_c.Selection (Ast_c.If (e, st1, st2)) ->
        let start_lineno_st1 = Def.find_startline_no (Def.create_stmtlist st1) in
        let end_lineno_st1 = Def.find_endline_no (Def.create_stmtlist st1) in
        let start_lineno_st2 = Def.find_startline_no (Def.create_stmtlist st2) in
        let end_lineno_st2 = Def.find_endline_no (Def.create_stmtlist st2) in
        if (line_no >= start_lineno_st1 && line_no <= end_lineno_st1)
        then
          gather_all_callin_func line_no lbl_list (Def.create_stmtlist st1)
        else
        if (line_no >= start_lineno_st2 && line_no <= end_lineno_st2)
        then
          gather_all_callin_func line_no lbl_list (Def.create_stmtlist st2)
        else gather_all_callin_func line_no lbl_list t

      | Ast_c.Selection (Ast_c.Switch (_, st))
      | Ast_c.Iteration (Ast_c.While (_, st))
      | Ast_c.Iteration (Ast_c.DoWhile (st, _))
      | Ast_c.Iteration (Ast_c.For (Ast_c.ForExp _, _, _, st))
      | Ast_c.Iteration (Ast_c.MacroIteration (_, _, st)) ->
        if (line_no >= start_lineno && line_no <= end_lineno)
        then gather_all_callin_func line_no lbl_list (Def.create_stmtlist st)
        else gather_all_callin_func line_no lbl_list t

      | Ast_c.Iteration (Ast_c.For (Ast_c.ForDecl _, _, _, _)) ->
        failwith "for loop with declaration in first argument not supported"

      |	Ast_c.Jump (Ast_c.Return)
      |	Ast_c.Jump (Ast_c.ReturnExpr _) -> []
      | _ -> gather_all_callin_func line_no lbl_list t
    else gather_all_callin_func line_no lbl_list t


let find_interproc_calls branch all_function miss_lineno lbl_list prog =
  let find_interproc_calls_aux acc (alloc, args, h) =
    match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall (_, es)), _), _)) ->
      let args_list = Def.create_argslist [] es in
      if (args_list = [])
      then (alloc, args, h)::acc
      else
        let all_callin_func =
          (gather_all_callin_func (Def.find_endline_no branch) lbl_list branch)
          @(gather_all_callin_func miss_lineno lbl_list prog) in
        if find_func_frm_all_func_main h args_list all_function all_callin_func
        then
          acc
        else
          (alloc, args, h)::acc
    | _ -> (alloc, args, h)::acc
  in
  List.fold_left find_interproc_calls_aux []
