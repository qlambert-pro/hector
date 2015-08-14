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

let rec goto_jump_toback st labels =
  match (Ast_c.unwrap st, labels) with
    (_, []) -> true
  | (Ast_c.Jump (Ast_c.Goto name), (a,b)::t) ->
    (match (Def.compare_names name a, (List.length b) > 0) with
       (false, _) -> goto_jump_toback st t
     | (_, false) -> true
     | _          -> (Def.find_startline_no b) < (Def.find_startline_no [st]))
  | _ -> true


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


let lbl_list_has_goto name =
  List.exists (function (a, b) -> Def.compare_names a name)

let rec gather_goto_code lbl_list goto_code = function
    []-> goto_code
  | h::t->
    (match Ast_c.unwrap h with
       Ast_c.Jump (Ast_c.Return) -> (goto_code@[h])
     | Ast_c.Jump (Ast_c.ReturnExpr e) -> (goto_code@[h])
     | Ast_c.Jump (Ast_c.Goto name) ->
       if (not(goto_jump_toback h lbl_list))
       then
         let new_goto_code = Def.code_for_goto name lbl_list in

         if (List.length new_goto_code) = 0
         then []
         else
           gather_goto_code lbl_list goto_code new_goto_code
       else []
     | _-> gather_goto_code lbl_list (goto_code@[h]) t
    )



let map_append st = List.map (function x -> x@st)


(* Whether return or goto statement exists in the statement list *)

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
                     (Ast_c.Logical Ast_c.Eq),
                     ((Ast_c.Ident (Ast_c.RegularName("NULL", _)), _), _))), _), _) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, op1, (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), blank_type_info), [])), blank_type_info), [])],
     [mk_expr_st ((Ast_c.Assignment (e1, op1, e2), blank_type_info), [])])

  | (((Ast_c.Binary ((((Ast_c.ParenExpr (((Ast_c.Assignment (e1, op1, e2), _), _))), _), _),
                     (Ast_c.Logical Ast_c.NotEq),
                     ((Ast_c.Ident (Ast_c.RegularName("NULL", _)), _), _))), _), _) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, op1, e2), blank_type_info), [])],
     [mk_expr_st ((Ast_c.Assignment (e1, op1, ((Ast_c.Ident (Ast_c.RegularName("NULL",[])), blank_type_info), [])), blank_type_info), [])])

  | (((Ast_c.Binary ((((Ast_c.ParenExpr (e1)), _), _),
                     (Ast_c.Logical Ast_c.Eq),
                     ((Ast_c.Ident (Ast_c.RegularName("NULL", _)), _), _))), _), _) ->
    ([mk_expr_st (((Ast_c.Assignment (e1, (Ast_c.SimpleAssign), (((Ast_c.Ident (Ast_c.RegularName("NULL",[]))), blank_type_info), []))), blank_type_info), [])],
     [mk_expr_st e1])

  | (((Ast_c.Assignment (e1, op, e2)), _), _) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, op, e2), blank_type_info), [])],
     [mk_expr_st ((Ast_c.Assignment (e1, op, (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), blank_type_info), [])), blank_type_info), [])])

  | (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR",ii3))), typ1), ii1), es) ), typ), ii) ->
    if List.length es > 0
    then ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))),
                                          (Ast_c.SimpleAssign),
                                          ((Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
          [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])

  | (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("dma_mapping_error",ii3))), typ1), ii1), es) ), typ), ii) ->
    if List.length es > 0
    then ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))),
                                          (Ast_c.SimpleAssign),
                                          ((Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
          [mk_expr_st(List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])

  | (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("is_bad",ii3))), typ1), ii1), es) ), typ), ii) ->
    if (List.length es> 0) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st(List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])
  | (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR_OR_NULL",ii3))), typ1), ii1), es) ), typ), ii) ->
    if (List.length es> 0) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])
  | (((Ast_c.Binary   ((((Ast_c.FunCall (e1, es)), typ1), ii1),
                       (Ast_c.Logical Ast_c.Eq),(((Ast_c.Unary (e, Ast_c.UnMinus)), typ4), ii4) )), typ), ii)->
    if (List.length es = 1) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else
      ([mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall (e1, es)), typ1), ii1),
                                       (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (((Ast_c.FunCall (e1, es)), typ1), ii1)])

  | (((Ast_c.Binary   ((((Ast_c.FunCall(e1, es)), typ1), ii1),
                       (Ast_c.Logical Ast_c.NotEq),(((Ast_c.Unary (e, Ast_c.UnMinus)), typ4), ii4) )), typ), ii)->
    if (List.length es = 1) then
      [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))],
      [mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))),
                                      (Ast_c.SimpleAssign),
                                      (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])]
    else
      [mk_expr_st (((Ast_c.FunCall(e1, es)), typ1), ii1)],
      [mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall(e1, es)), typ1), ii1), (Ast_c.SimpleAssign),
                                      (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])]
  | ((Ast_c.FunCall (((Ast_c.Ident (Ast_c.RegularName("unlikely",ii3)), typ1), ii1), es), typ), ii) ->
    let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
    invert_st (List.hd args_list)
  | (((Ast_c.Unary    ((((Ast_c.ParenExpr ((((Ast_c.FunCall ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR",ii1))), typ4), ii4), es)), typ3), ii3))), typ2), ii2), Ast_c.Not)), typ), ii) ->
    if (List.length es = 1) then
      ([mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))],
       [mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])
    else ([nst],[nst])
  | (((Ast_c.Unary ((((Ast_c.FunCall  (e, es)), typ1), ii1), Ast_c.Not)), typ), ii)->
    let ptr_args = Def.find_ptr_args_list ((Def.remove_optionlist (Def.create_argslist [] es))) in
    if (List.length ptr_args > 0) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd ptr_args), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else
      ([mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall  (e, es)), typ1), ii1), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [(Ast_c.ExprStatement (None),[])])
  | (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.OrLog), e2)), typ), ii)->
    let (norm_exp1, opp_exp1)  = invert_st e1 in
    let (norm_exp2, opp_exp2)  = invert_st e2 in
    ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
  | (((Ast_c.FunCall  (e, es)), typ1), ii1)->
    if (List.length es =0) then
      (
        [mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall  (e, es)), typ1), ii1), (Ast_c.SimpleAssign),
                                        (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ1), [])), typ1), [])], [mk_expr_st(((Ast_c.FunCall  (e, es)), typ1), ii1)])
    else if (List.length es >0) then
      ([(Ast_c.ExprStatement (None),[])],
       [(Ast_c.ExprStatement (None),[])])
    else ([nst],[nst])
  | (((Ast_c.Unary    ((((Ast_c.ParenExpr (((Ast_c.FunCall ((e, es)), typ3), ii3))), typ2), ii2), Ast_c.Not)), typ), ii) ->
    if (List.length es = 1) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else
      ([mk_expr_st ((Ast_c.Assignment (((Ast_c.FunCall ((e, es)), typ3), ii3), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
       [mk_expr_st ((Ast_c.FunCall ((e, es)), typ3), ii3)])
  | (((Ast_c.Binary ((((Ast_c.FunCall  (e, es)), typ2), ii2),
                     (Ast_c.Logical Ast_c.Eq),(((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii) ->
    if (List.length es = 1) then
      ([mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ1), [])), typ), [])],
       [mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))])
    else ([nst],[nst])
  | (((Ast_c.Binary ((((Ast_c.FunCall  (e, es)), typ2), ii2),
                     (Ast_c.Logical Ast_c.NotEq),(((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii) ->
    if (List.length es = 1) then
      ([mk_expr_st (List.hd (Def.remove_optionlist (Def.create_argslist [] es)))],
       [mk_expr_st ((Ast_c.Assignment ((List.hd (Def.remove_optionlist (Def.create_argslist [] es))), (Ast_c.SimpleAssign),
                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ1), [])), typ), [])])
    else ([nst],[nst])

  | (((Ast_c.Unary ((((Ast_c.ParenExpr ((((Ast_c.Assignment (e1, (Ast_c.SimpleAssign), e2)), typ2), ii2))), typ1), ii1), Ast_c.Not)), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
     [mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign), e2), typ), [])])

  | (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.Eq),(((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])], [mk_expr_st e11])

  | (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.NotEq),(((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii) ->
    ([mk_expr_st e11],[mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])

  | (((Ast_c.Binary ((((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1),(Ast_c.Logical Ast_c.Eq),e11)), typ), ii) ->
    ([mk_expr_st((Ast_c.Assignment (e11, (Ast_c.SimpleAssign), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
     [mk_expr_st e11])
  | (((Ast_c.Binary ((((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1),(Ast_c.Logical Ast_c.NotEq),e11)), typ), ii) ->
    ([mk_expr_st e11],[mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])

  | (((Ast_c.Binary   (((((Ast_c.FunCall  (e, es)), typ2), ii2)),
                       (Ast_c.Logical Ast_c.Eq), ((Ast_c.Constant (Ast_c.Int ("0", _)), typ1), ii1))), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e, (Ast_c.SimpleAssign),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],
     [(Ast_c.ExprStatement (None),[])])
  | (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.Eq), ((Ast_c.Constant (Ast_c.Int ("0", _)), typ1), ii1))), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],[mk_expr_st e1])

  | (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.Inf), ((Ast_c.Constant (Ast_c.Int ("0", _)), typ1), ii1))), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],[mk_expr_st e1])

  | (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.NotEq), ((Ast_c.Constant (Ast_c.Int ("0", _)), typ1), ii1))), typ), ii) ->
    ([mk_expr_st e1], [mk_expr_st ((Ast_c.Assignment (e1, (Ast_c.SimpleAssign), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])
  | (((Ast_c.ParenExpr ((((Ast_c.Unary (e11, Ast_c.Not)), typ), ii))), typ1), ii1) ->
    ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])], [mk_expr_st e11])
  | (((Ast_c.Unary ((((Ast_c.ParenExpr (e11)), typ1), ii1), Ast_c.Not)), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                     (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ1), [])), typ1), [])],[mk_expr_st e11])

  | (((Ast_c.Unary (e11, Ast_c.Not)), typ), ii)  ->
    (match (Def.is_pointer e11) with
       Def.IsPtr  ->
       ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                        (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],[mk_expr_st e11])
     | Def.UnknownType ->

       ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                        (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])],[mk_expr_st e11])

     | Def.IsntPtr->
       ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                        (( Ast_c.Ident (Ast_c.RegularName("0",[])), typ), [])), typ), [])],[mk_expr_st e11]))

  | (((Ast_c.Binary ((((Ast_c.FunCall  (e, es)), typ1), ii1), (Ast_c.Logical Ast_c.Eq),e22)), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment ((((Ast_c.FunCall  (e, es)), typ1), ii1), (Ast_c.SimpleAssign),e22), typ), [])],
     [])

  | (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.Eq),e22)), typ), ii) ->
    ([mk_expr_st ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),e22), typ), [])],[mk_expr_st e11])
  | ((exp, typ), ii)->
    ([mk_expr_st ((exp, typ), ii)],
     [mk_expr_st ((Ast_c.Assignment (((exp, typ), ii), (Ast_c.SimpleAssign), (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])])


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


let rec has_multiple_st = function
    []-> false
  | h::t -> (match Ast_c.unwrap h with
      | Ast_c.ExprStatement (None) -> false
      | _ -> true
    )


let any_ref_var_access_stmt stmt ref_vars =
  List.exists (fun x -> Def.exp_exists_in_stmt (Some x) stmt) ref_vars


let rec any_var_access_stmt stmt = function
    []-> false
  | (var, ref_vars)::t ->
    if Def.exp_exists_in_stmt (Some var) stmt
    then true
    else
    if any_ref_var_access_stmt stmt ref_vars
    then true
    else any_var_access_stmt stmt t


let rec is_following_code_access_exp exp point_lineno follow final_return = function
    []-> follow
  | h::t->
    if final_return > (Def.find_startline_no (Def.create_stmtlist h)) then
      (
        match Ast_c.unwrap h with

        |  Ast_c.Labeled (Ast_c.Label (name, st)) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow final_return  (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow  final_return  t
        |  Ast_c.Labeled (Ast_c.Case  (e, st)) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow  final_return  (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow final_return   t
        |  Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow  final_return  (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow final_return   t
        |  Ast_c.Labeled (Ast_c.Default st) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow  final_return  (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow final_return   t
        |  Ast_c.Compound statxs ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow  final_return  (Def.create_stmtlist h) in
          is_following_code_access_exp exp point_lineno new_follow final_return   t
        |  Ast_c.ExprStatement (Some e) ->
          if (any_var_access_stmt h exp) && (point_lineno < (Def.find_startline_no (Def.create_stmtlist h))) then (

            is_following_code_access_exp exp point_lineno true final_return  t)

          else is_following_code_access_exp exp point_lineno follow final_return   t
        |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
          let new_follow1 =
            is_following_code_access_exp exp point_lineno follow final_return   (Def.create_stmtlist st1) in
          let new_follow2 =
            is_following_code_access_exp exp point_lineno new_follow1 final_return   (Def.create_stmtlist st2) in
          let new_follow3 =
            is_following_code_access_exp exp point_lineno new_follow2 final_return   ([(Ast_c.ExprStatement (Some e),[])]) in
          is_following_code_access_exp exp point_lineno new_follow3 final_return   t
        |  Ast_c.Selection  (Ast_c.Switch (e, st)) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow final_return   (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow final_return   t
        |  Ast_c.Iteration  (Ast_c.While (e, st)) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow final_return   (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow  final_return  t
        |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow  final_return  (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow  final_return  t
        | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
          failwith "for loop with declaration in first argument not supported"
        |  Ast_c.Iteration  (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow  final_return  (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow  final_return  t
        |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) ->
          let new_follow =
            is_following_code_access_exp exp point_lineno follow  final_return  (Def.create_stmtlist st) in
          is_following_code_access_exp exp point_lineno new_follow final_return   t
        |  Ast_c.Jump (Ast_c.Goto name) ->
          is_following_code_access_exp exp point_lineno follow final_return   t
        |  Ast_c.Jump (Ast_c.Return) -> is_following_code_access_exp exp point_lineno follow  final_return  t

        |  Ast_c.Jump (Ast_c.ReturnExpr e) ->
          if (any_var_access_stmt h exp) && (point_lineno < (Def.find_startline_no (Def.create_stmtlist h))) then (

            is_following_code_access_exp exp point_lineno true final_return  t)

          else is_following_code_access_exp exp point_lineno follow final_return   t

        |  Ast_c.Jump (Ast_c.GotoComputed e) ->
          is_following_code_access_exp exp point_lineno follow  final_return  t
        |  Ast_c.Decl decl ->
          is_following_code_access_exp exp point_lineno follow final_return   t
        |  Ast_c.Asm asmbody ->
          is_following_code_access_exp exp point_lineno follow  final_return  t
        |  Ast_c.NestedFunc def ->
          is_following_code_access_exp exp point_lineno follow  final_return  t
        |  Ast_c.MacroStmt ->
          is_following_code_access_exp exp point_lineno follow  final_return  t
        | _-> is_following_code_access_exp exp point_lineno follow  final_return  t

      )
    else follow

let rec generate_exe_paths_simple fin_lineno exe_paths lbl_list = function
    []-> exe_paths
  | h::t-> let start_line = Def.find_startline_no (Def.create_stmtlist h) in
    let end_line = Def.find_endline_no (Def.create_stmtlist h) in
    if(fin_lineno>= start_line && fin_lineno<=end_line) || fin_lineno> end_line then
      match Ast_c.unwrap h with
      |   Ast_c.Labeled (Ast_c.Label (name, st)) ->
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else generate_exe_paths_simple fin_lineno exe_paths lbl_list t

      |  Ast_c.Labeled (Ast_c.Case  (e, st)) ->
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else generate_exe_paths_simple fin_lineno exe_paths lbl_list t

      |  Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else generate_exe_paths_simple fin_lineno exe_paths lbl_list t

      |  Ast_c.Labeled (Ast_c.Default st) ->
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else generate_exe_paths_simple fin_lineno exe_paths lbl_list t

      |  Ast_c.Compound statxs ->
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist h) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist h))) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist h) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else generate_exe_paths_simple fin_lineno exe_paths lbl_list t
      |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
        let (norm_exp,opp_exp) = (
          match e with
            (((Ast_c.Binary (e1, (Ast_c.Logical Ast_c.OrLog), e2)), typex), iiex) ->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          | (((Ast_c.Binary (e1, (Ast_c.Logical Ast_c.AndLog), e2)), typex), iiex) ->
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
          let goto_code = gather_goto_code lbl_list [] (Def.create_stmtlist st1) in
          let exe_paths1 =   if (List.length exe_paths1) = 0 then (exe_paths1@[goto_code])
            else (map_append goto_code exe_paths1)
          in
          exe_paths1)


        else if fin_lineno = start_line_inner2 then
          let goto_code = gather_goto_code lbl_list [] (Def.create_stmtlist st2) in
          let exe_paths2 =   if (List.length exe_paths2) = 0 then (exe_paths2@[goto_code])
            else (map_append goto_code exe_paths2)
          in
          exe_paths2


        else if (fin_lineno>= start_line_inner1 && fin_lineno <= end_line_inner1) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list (Def.create_stmtlist st1) in
          generate_exe_paths_simple fin_lineno new_exe_paths1 lbl_list t
        else if (fin_lineno>= start_line_inner2 && fin_lineno <= end_line_inner2) then(
          let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths2 lbl_list (Def.create_stmtlist st2) in
          generate_exe_paths_simple fin_lineno new_exe_paths2 lbl_list t)

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st1))) && (not(List.exists is_return_or_goto (Def.create_stmtlist st2))) && ((List.length st2_new) >0)then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list (Def.create_stmtlist st1) in
          generate_exe_paths_simple fin_lineno (new_exe_paths1) lbl_list t



        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st1))) && (not(List.exists is_return_or_goto (Def.create_stmtlist st2))) && ((List.length st2_new) =0)then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list (Def.create_stmtlist st1) in
          generate_exe_paths_simple fin_lineno (new_exe_paths1) lbl_list t


        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st1))) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list (Def.create_stmtlist st1) in
          generate_exe_paths_simple fin_lineno new_exe_paths1 lbl_list t

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st2))) && ((List.length st2_new) >0)  then(
          let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths2 lbl_list (Def.create_stmtlist st2) in
          generate_exe_paths_simple fin_lineno new_exe_paths2 lbl_list t)

        else generate_exe_paths_simple fin_lineno exe_paths2 lbl_list t


      |  Ast_c.Selection  (Ast_c.Switch (e, st)) ->
        let exe_paths1 = if (List.length exe_paths) = 0 then (exe_paths@[[((Ast_c.ExprStatement (Some e)),[])]])
          else (map_append [((Ast_c.ExprStatement (Some e)),[])] exe_paths)
        in
        let start_line_inner = Def.find_startline_no (Def.create_stmtlist st) in
        let end_line_inner =   Def.find_startline_no (Def.create_stmtlist st) in
        if(fin_lineno>= start_line_inner && fin_lineno <= end_line_inner) then
          let case_stmts = find_case_stmts fin_lineno []  (Def.create_stmtlist st) in
          let new_exe_paths  = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list case_stmts  in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let switch_st_compound = create_compound_st_for_switch e [] [] (Def.create_stmtlist st) in

          let new_exe_paths = generate_exe_paths_simple fin_lineno []  lbl_list switch_st_compound in
          let switch_exe = if(List.length new_exe_paths =0) then exe_paths1
            else exe_path_for_switch [] new_exe_paths exe_paths1
          in

          generate_exe_paths_simple fin_lineno switch_exe  lbl_list t
        else generate_exe_paths_simple fin_lineno exe_paths1 lbl_list t
      |  Ast_c.Iteration  (Ast_c.While (e, st)) ->

        let (norm_exp,opp_exp) = (
          match e with
            (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.OrLog), e2)), typex), iiex)->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          |  (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.AndLog), e2)), typex), iiex)->
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
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno (new_exe_paths1) lbl_list t
        else ( let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths2  lbl_list [] in
               generate_exe_paths_simple fin_lineno new_exe_paths2 lbl_list t
             )
      |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->
        let (norm_exp,opp_exp) = (
          match e with
            (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.OrLog), e2)), typex), iiex)->
            let (norm_exp1,opp_exp1) = invert_st e1 in
            let (norm_exp2,opp_exp2) = invert_st e2 in
            ((norm_exp1@norm_exp2),(opp_exp1@opp_exp2))
          |  (((Ast_c.Binary   (e1, (Ast_c.Logical Ast_c.AndLog), e2)), typex), iiex)->
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
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t

        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths1 lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno (new_exe_paths1) lbl_list t
        else ( let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths2  lbl_list [] in
               generate_exe_paths_simple fin_lineno new_exe_paths2 lbl_list t
             )


      | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
        failwith "for loop with declaration in first argument not supported"
      |  Ast_c.Iteration  (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno (new_exe_paths1) lbl_list t
        else ( let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths  lbl_list [] in
               generate_exe_paths_simple fin_lineno new_exe_paths2 lbl_list t
             )
      |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) ->
        if (fin_lineno>= start_line && fin_lineno<=end_line) then
          let new_exe_paths = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno new_exe_paths lbl_list t
        else if (not(List.exists is_return_or_goto (Def.create_stmtlist st))) then
          let new_exe_paths1 = generate_exe_paths_simple fin_lineno exe_paths lbl_list (Def.create_stmtlist st) in
          generate_exe_paths_simple fin_lineno (new_exe_paths1) lbl_list t
        else ( let new_exe_paths2 = generate_exe_paths_simple fin_lineno exe_paths  lbl_list [] in
               generate_exe_paths_simple fin_lineno new_exe_paths2 lbl_list t
             )

      |   Ast_c.Jump (Ast_c.Goto name) ->
        generate_exe_paths_simple fin_lineno exe_paths lbl_list t

      |   Ast_c.Jump (Ast_c.Return) -> exe_paths
      |   Ast_c.Jump (Ast_c.ReturnExpr e) -> exe_paths
      |   Ast_c.Jump (Ast_c.GotoComputed e) -> generate_exe_paths_simple fin_lineno exe_paths lbl_list t
      | _->
        if(List.length exe_paths) = 0 then
          generate_exe_paths_simple fin_lineno (exe_paths@[[h]]) lbl_list t
        else
          generate_exe_paths_simple fin_lineno (map_append [h] exe_paths) lbl_list t

    else exe_paths



let rec is_error_code_test ee =
  match ee with

  | (((Ast_c.Binary (e1, Ast_c.Logical Ast_c.NotEq, (((Ast_c.Constant (Ast_c.Int ("0", _))), typ2), ii2))), typ), ii) ->
    (match (Def.is_pointer e1) with
       Def.IsntPtr -> Some true
     | _           -> None)

  | (((Ast_c.Binary (e1, Ast_c.Logical Ast_c.SupEq, (((Ast_c.Constant (Ast_c.Int ("0", _))), typ2), ii2))), typ), ii)
  |  (((Ast_c.Binary (e1, Ast_c.Logical Ast_c.Eq, (((Ast_c.Constant (Ast_c.Int ("0", _))), typ2), ii2))), typ), ii) ->
    (match (Def.is_pointer e1) with
       Def.IsntPtr -> None
     | _           -> Some true)

  | (((Ast_c.Binary (e1, (Ast_c.Logical Ast_c.OrLog), e2)), typ), ii) ->
    (match (is_error_code_test e1, is_error_code_test e2) with
       (Some true, _)
     | (_, Some true) -> Some true
     | (None, _)
     | (_, None)      -> None
     | _              -> Some false)

  | (((Ast_c.Binary (e1, (Ast_c.Logical Ast_c.AndLog), e2)), typ), ii) ->
    (match (is_error_code_test e1, is_error_code_test e2) with
       (Some true, Some true) -> Some true
     | (None, _)
     | (_, None)      -> None
     | _              -> Some false)

  | (((Ast_c.Binary (e1, (Ast_c.Arith Ast_c.And), e2)), typ), ii)
  | (((Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Or), e2)), typ), ii) -> None

  | (((Ast_c.FunCall ((((Ast_c.Ident (Ast_c.RegularName ("IS_ERR", _))), _), _),
                      _)), _), _)
  | (((Ast_c.Binary (_, Ast_c.Logical Ast_c.Eq , (((Ast_c.Unary (_, Ast_c.Not)),
                                                   _), _))), _), _)
  | (((Ast_c.Binary (_, Ast_c.Logical Ast_c.Eq , (((Ast_c.Unary (_,
                                                                 Ast_c.UnMinus)),
                                                   _), _))), _), _) -> Some true

  | (((Ast_c.Unary  (e1, Ast_c.Not)), typ), ii) ->
    (match (Def.is_pointer e1) with
       Def.IsntPtr -> Some false
     | _           -> Some true)

  | (((Ast_c.Binary (_, (Ast_c.Logical Ast_c.Eq), ((Ast_c.Ident
                                                      (Ast_c.RegularName("NULL",_)),
                                                    _), _))), _), _)
  | (((Ast_c.Binary (_, Ast_c.Logical Ast_c.Inf, (((Ast_c.Constant (Ast_c.Int
                                                                      ("0",
                                                                       _))), _),
                                                  _))), _), _) -> Some true

  | (((Ast_c.Ident (ident)), typ), ii) ->
    (match Def.is_pointer (((Ast_c.Ident (ident)), typ), ii) with
       Def.IsntPtr -> Some true
     | _           -> None)

  | _ ->  Some false


let find_op_type = function
    (Ast_c.Logical Ast_c.Inf) -> true
  | (Ast_c.Logical Ast_c.Sup) -> true
  | (Ast_c.Logical Ast_c.InfEq) -> true
  | (Ast_c.Logical Ast_c.SupEq)-> true
  | (Ast_c.Logical Ast_c.Eq) -> true
  | (Ast_c.Logical Ast_c.NotEq)-> true
  | (Ast_c.Logical Ast_c.AndLog) -> true
  | (Ast_c.Logical Ast_c.OrLog)-> true
  | _-> false



let rec return_error e typ = function
    []   -> false
  | h::t ->
    let is_if_error_code =
      let error_code_test = is_error_code_test e in
      (match typ with
         Def.Then ->
         (match error_code_test with
            Some true -> true
          | _         -> false)
       | Def.Else ->
         (match error_code_test with
            None -> true
          | _    -> false))
    in
    match Ast_c.unwrap h with
      Ast_c.Jump Ast_c.Return
    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Constant (Ast_c.MultiString _)), _),
                                    _))
    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.CondExpr _), _), _))
    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Constant (Ast_c.String _)), _), _))
    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.RecordAccess ((((Ast_c.Ident _), _),
                                                           _), _)), _), _))
    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.RecordPtAccess ((((Ast_c.Ident _),
                                                              _), _), _)), _), _)) ->
      is_if_error_code

    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident _), _), _)) ->
      true

    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Binary (_, opr, _)), _), _)) ->
      if find_op_type opr
      then is_if_error_code
      else false

    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Assignment (_, _, e2)), _), _))->
      is_error_return_code e2

    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Unary (e, Ast_c.Not)), _), _)) ->
      let error_code_test = is_error_code_test e in
      (match typ with
         Def.Then ->
         (match error_code_test with
            Some true -> true
          | _         -> false)
       | Def.Else ->
         (match error_code_test with
            None -> true
          | _    -> false))

    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.ParenExpr ((((Ast_c.Ident _), _),
                                                        _))), _), _))
    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Cast (_, (((Ast_c.ParenExpr
                                                          ((((Ast_c.Ident _),
                                                             _), _))), _), _))),
                                     _), _))
    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Cast (_, (((((Ast_c.Ident _))), _),
                                                      _))), _), _)) -> true

    | Ast_c.Jump (Ast_c.ReturnExpr e1) -> is_error_return_code e1
    | _ ->  return_error e typ t


let rec remove_string_args = function
    []-> []
  | h::t-> if(Def.string_exists_in_explist [h]) then
      remove_string_args t
    else h::(remove_string_args t)


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


let rec find_ptr_args_list = function
    []->[]
  | h::t->
    match (Def.is_pointer h) with
      Def.IsPtr       -> h::find_ptr_args_list t
    | Def.UnknownType -> h::find_ptr_args_list t
    | _               -> find_ptr_args_list t

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


let rec list_of_id_values exe_paths_list =
  List.map (fun id -> (id, find_recent_id_values_paths id [] exe_paths_list))


let return_built_in_constant id =
  if id =~ "NULL" then false
  else if id =~ "FAILED" then false
  else if id =~ "FALSE" then false
  else if id =~ "HTTP_INTERNAL_SERVER_ERROR" then false
  else if id =~ "DECLINED" then true
  else if id =~ "HTTP_GATEWAY_TIME_OUT" then false
  else if id =~ "HTTP_FORBIDDEN" then false
  else if id =~ "HTTP_UNAUTHORIZED" then false
  else if id =~ "HTTP_BAD_REQUEST" then false
  else if id =~ "HTTP_REQUEST_ENTITY_TOO_LARGE" then false
  else if id =~ "HTTP_METHOD_NOT_ALLOWED" then false
  else if id =~ "HTTP_NOT_FOUND" then false
  else if id =~ "HTTP_NOT_IMPLEMENTED" then false
  else if id =~ "HTTP_TEMPORARY_REDIRECT" then false
  else if id =~ "HTTP_MOVED_TEMPORARILY" then false
  else if id =~ "HTTP_PRECONDITION_FAILED" then false
  else if id =~ "HTTP_NOT_MODIFIED" then false
  else if id =~ "HTTP_NO_CONTENT" then false
  else if id =~ "HTTP_BAD_GATEWAY" then false
  else if id =~ "MODSSL_ERROR_HTTP_ON_HTTPS" then false
  else if id =~ "HTTP_UPGRADE_REQUIRED" then false
  else if id =~ "HTTP_REQUEST_TIME_OUT" then false
  else if id =~ "HTTP_SERVICE_UNAVAILABLE" then true
  else if id =~ "HTTP_MOVED_PERMANENTLY" then false
  else if id =~ "HTTP_EXPECTATION_FAILED" then false
  else if id =~ "HTTP_MULTIPLE_CHOICES" then false
  else if id =~ "HTTP_NOT_ACCEPTABLE" then false
  else if id =~ "HTTP_VARIANT_ALSO_VARIES" then false
  else if id =~ "AUTHZ_DENIED_NO_USER" then false
  else if id =~ "AUTHZ_DENIED" then false
  else if id =~ "AUTHZ_GENERAL_ERROR" then false
  else if id =~ "AUTHZ_USER_NOT_FOUND" then false
  else if id =~ "AUTH_DENIED_NO_USER" then false
  else if id =~ "AUTH_DENIED" then false
  else if id =~ "AUTH_GENERAL_ERROR" then false
  else if id =~ "AUTH_USER_NOT_FOUND" then false
  else if id =~ "APR_EABOVEROOT" then false
  else if id =~ "APR_EABSOLUTE" then false
  else if id =~ "APR_EACCES" then false
  else if id =~ "APR_EAGAIN" then false
  else if id =~ "APR_EBADDATE" then false
  else if id =~ "APR_EBADF" then false
  else if id =~ "APR_EBADIP" then false
  else if id =~ "APR_EBADMASK" then false
  else if id =~ "APR_EBADPATH" then false
  else if id =~ "APR_EBUSY" then false
  else if id =~ "APR_ECONNABORTED" then false
  else if id =~ "APR_ECONNREFUSED" then false
  else if id =~ "APR_ECONNRESET" then false
  else if id =~ "APR_EDSOOPEN" then false
  else if id =~ "APR_EEXIST" then false
  else if id =~ "APR_EFTYPE" then false
  else if id =~ "APR_EGENERAL" then false
  else if id =~ "APR_EHOSTUNREACH" then false
  else if id =~ "APR_EINCOMPLETE" then false
  else if id =~ "APR_EINIT" then false
  else if id =~ "APR_EINPROGRESS" then false
  else if id =~ "APR_EINTR" then false
  else if id =~ "APR_EINVAL" then false
  else if id =~ "APR_EINVALSOCK" then false
  else if id =~ "APR_EMFILE" then false
  else if id =~ "APR_EMISMATCH" then false
  else if id =~ "APR_ENAMETOOLONG" then false
  else if id =~ "APR_END" then false
  else if id =~ "APR_ENETUNREACH" then false
  else if id =~ "APR_ENFILE" then false
  else if id =~ "APR_ENODIR" then false
  else if id =~ "APR_ENOENT" then false
  else if id =~ "APR_ENOLOCK" then false
  else if id =~ "APR_ENOMEM" then false
  else if id =~ "APR_ENOPOLL" then false
  else if id =~ "APR_ENOPOOL" then false
  else if id =~ "APR_ENOPROC" then false
  else if id =~ "APR_ENOSHMAVAIL" then false
  else if id =~ "APR_ENOSOCKET" then false
  else if id =~ "APR_ENOSPC" then false
  else if id =~ "APR_ENOSTAT" then false
  else if id =~ "APR_ENOTDIR" then false
  else if id =~ "APR_ENOTEMPTY" then false
  else if id =~ "APR_ENOTHDKEY" then false
  else if id =~ "APR_ENOTHREAD" then false
  else if id =~ "APR_ENOTIME" then false
  else if id =~ "APR_ENOTIMPL" then false
  else if id =~ "APR_ENOTSOCK" then false
  else if id =~ "APR_EOF" then false
  else if id =~ "APR_EPATHWILD" then false
  else if id =~ "APR_EPIPE" then false
  else if id =~ "APR_EPROC_UNKNOWN" then false
  else if id =~ "APR_ERELATIVE" then false
  else if id =~ "APR_ESPIPE" then false
  else if id =~ "APR_ESYMNOTFOUND" then false
  else if id =~ "APR_ETIMEDOUT" then false
  else if id =~ "APR_EXDEV" then false
  else if id =~ "TCL_ERROR" then false
  else if id =~ "DB_NOTFOUND" then false
  else if id =~ "DB_TIMEOUT" then false
  else if id =~ "WRC_Prune" then false
  else if id =~ "DB_TIMEOUT" then false
  else if id =~ "errno" then false
  else if id =~ "EABOVEROOT" then false
  else if id =~ "EABSOLUTE" then false
  else if id =~ "EACCES" then false
  else if id =~ "EAGAIN" then false
  else if id =~ "EBADDATE" then false
  else if id =~ "EBADF" then false
  else if id =~ "EBADIP" then false
  else if id =~ "EBADMASK" then false
  else if id =~ "EBADPATH" then false
  else if id =~ "EBUSY" then false
  else if id =~ "ERANGE" then false
  else if id =~ "EPERM" then false
  else if id =~ "ECONNABORTED" then false
  else if id =~ "ECONNREFUSED" then false
  else if id =~ "ECONNRESET" then false
  else if id =~ "EDSOOPEN" then false
  else if id =~ "EEXIST" then false
  else if id =~ "EFTYPE" then false
  else if id =~ "EGENERAL" then false
  else if id =~ "EHOSTUNREACH" then false
  else if id =~ "EINCOMPLETE" then false
  else if id =~ "EINIT" then false
  else if id =~ "EINPROGRESS" then false
  else if id =~ "EINTR" then false
  else if id =~ "EINVAL" then false
  else if id =~ "EINVALSOCK" then false
  else if id =~ "EMFILE" then false
  else if id =~ "EMISMATCH" then false
  else if id =~ "ENAMETOOLONG" then false
  else if id =~ "END" then false
  else if id =~ "ENETUNREACH" then false
  else if id =~ "ENFILE" then false
  else if id =~ "ENODIR" then false
  else if id =~ "ENOENT" then false
  else if id =~ "ENOLOCK" then false
  else if id =~ "ENOMEM" then false
  else if id =~ "ENOPOLL" then false
  else if id =~ "ENOPOOL" then false
  else if id =~ "ENOPROC" then false
  else if id =~ "ENOSHMAVAIL" then false
  else if id =~ "ENOSOCKET" then false
  else if id =~ "ENOSPC" then false
  else if id =~ "ENOSTAT" then false
  else if id =~ "ENOTDIR" then false
  else if id =~ "ENOTEMPTY" then false
  else if id =~ "ENOTHDKEY" then false
  else if id =~ "ENOTHREAD" then false
  else if id =~ "ENOTIME" then false
  else if id =~ "ENOTIMPL" then false
  else if id =~ "ENOTSOCK" then false
  else if id =~ "EOF" then false
  else if id =~ "EPATHWILD" then false
  else if id =~ "EPIPE" then false
  else if id =~ "EPROC_UNKNOWN" then false
  else if id =~ "ERELATIVE" then false
  else if id =~ "ESPIPE" then false
  else if id =~ "ESYMNOTFOUND" then false
  else if id =~ "ETIMEDOUT" then false
  else if id =~ "EXDEV" then false
  else if id =~ "SQLITE_IOERR" then false
  else if id =~ "SQLITE_CANTOPEN" then false
  else if id =~ "SQLITE_FULL" then false
  else if id =~ "SQLITE_ERROR" then false
  else if id =~ "SQLITE_IOERR_NOMEM" then false
  else if id =~ "SQLITE_MISUSE" then false
  else if id =~ "SQLITE_NOMEM" then false
  else if id =~ "SQLITE_LOCKED" then false
  else if id =~ "SQLITE_MISUSE_BKPT" then false
  else if id =~ "SQLITE_BUSY" then false
  else if id =~ "SQLITE_IOERR_LOCK" then false
  else if id =~ "SQLITE_IOERR_FSTAT" then false
  else if id =~ "SQLITE_IOERR_WRITE" then false
  else if id =~ "SQLITE_NOLFS" then false
  else if id =~ "SQLITE_MISUSE_BKPT" then false
  else if id =~ "SQLITE_CORRUPT_BKPT" then false
  else if id =~ "SQLITE_NOTFOUND" then false
  else if id =~ "SQLITE_TOOBIG" then false
  else if id =~ "SQLITE_CONSTRAINT" then false
  else if id =~ "SQLITE_IOERR_ACCESS" then false
  else if id =~ "SQLITE_CORRUPT" then false
  else if id =~ "SQLITE_DENY" then false
  else if id =~ "SQL_INVALID_HANDLE" then false
  else if id =~ "SQL_NO_DATA" then false
  else if id =~ "SQL_ERROR" then false
  else if id =~ "FAILURE" then false
  else if id =~ "False_" then false
  else if id =~ "False" then false
  else if id =~ "false" then false
  else if id =~ "EXIT_FAILURE" then false
  else if id =~ "EOF" then false
  else if id =~ "SIG_ERR" then false
  else if id =~ "HASH_KEY_IS_LONG" then false
  else if id =~ "HASH_KEY_IS_STRING" then false
  else if id =~ "HASH_KEY_NON_EXISTANT" then false
  else if id =~ "ZEND_HASH_APPLY_STOP" then false
  else if id =~ "ZEND_HASH_APPLY_KEEP" then false
  else if id =~ "ZEND_HASH_APPLY_REMOVE" then false
  else if id =~ "NAMESPACE_ERR" then false
  else if id =~ "PHP_ICONV_ERR_UNKNOWN" then false
  else if id =~ "PHP_ICONV_ERR_ALLOC" then false
  else if id =~ "PHP_ICONV_ERR_WRONG_CHARSET" then false
  else if id =~ "PHP_CONV_ERR_NOT_FOUND" then false
  else if id =~ "PHP_CONV_ERR_ALLOC" then false
  else if id =~ "PHP_CONV_ERR_UNEXPECTED_EOS" then false
  else if id =~ "PHP_CONV_ERR_TOO_BIG" then false
  else if id =~ "PSFS_ERR_FATAL" then false
  else if id =~ "PHP_FTP_MOREDATA" then false
  else if id =~ "PHP_FTP_FAILED" then false
  else if id =~ "SSB_FAIL" then false
  else if id =~ "EXIT_FAILURE" then false
  else if id =~ "STATUS_ERROR" then false
  else if id =~ "ECPG_INFORMIX_OUT_OF_MEMORY" then false
  else if id =~ "ECPG_INFORMIX_NUM_OVERFLOW" then false
  else if id =~ "ECPG_INFORMIX_NUM_UNDERFLOW" then false
  else if id =~ "ECPG_ARRAY_ERROR" then false
  else if id =~ "PXE_PGP_CORRUPT_DATA" then false
  else if id =~ "PXE_PGP_NOT_TEXT" then false
  else if id =~ "PXE_BUG" then false
  else if id =~ "PXE_PGP_UNSUPPORTED_COMPR" then false
  else if id =~ "PXE_PGP_COMPRESSION_ERROR" then false
  else if id =~ "PXE_PGP_UNSUPPORTED_CIPHER" then false
  else if id =~ "PXE_PGP_KEYPKT_CORRUPT" then false
  else if id =~ "PXE_PGP_WRONG_KEY" then false
  else if id =~ "PXE_PGP_CORRUPT_ARMOR" then false
  else if id =~ "DTERR_FIELD_OVERFLOW" then false
  else if id =~ "DTERR_BAD_FORMAT" then false
  else if id =~ "DTERR_MD_FIELD_OVERFLOW" then false
  else if id =~ "DTERR_TZDISP_OVERFLOW" then false
  else if id =~ "PGRES_POLLING_FAILED" then false
  else if id =~ "InvalidOid" then false
  else true

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


let exists_error_handling_block c_function exp stmtlist part blk_strtlineno =
  match Def.return_exists_in_list stmtlist with
    None    -> false
  | Some st ->
    let return_expr =
      match Ast_c.unwrap st with
        Ast_c.Jump (Ast_c.ReturnExpr (e, _)) -> Some e
      | _ -> None
    in
    (* Return true if expr has been assigned an error return code or
     * if we are on the error branch of a test_case *)
    let exists_error_handling_block_aux fin_lineno expr =
      let exe_paths_list =
        C_function.generate_exe_paths_simple fin_lineno [] c_function in
      let id_values = find_recent_id_values_paths expr [] exe_paths_list in
      if List.exists is_error_return_code id_values
      then true
      else
        match part with
           Def.Then ->
           (match is_error_code_test exp with
              Some true -> true
            | _         -> false)

         | Def.Else ->
           (match is_error_code_test exp with
              Some _ -> false
            | _      -> true)
    in
    (match return_expr with
       Some ((Ast_c.Ident (Ast_c.RegularName(id, ii))), typ) ->
       if (not (return_built_in_constant id))
       then true
       else
         let fin_lineno = blk_strtlineno in
         let expr = (((Ast_c.Ident (Ast_c.RegularName(id, ii))), typ), []) in
         exists_error_handling_block_aux fin_lineno expr

     | Some ((Ast_c.ParenExpr ((((Ast_c.Ident (Ast_c.RegularName(id, ii))),
                                 _), _))), _) ->
       if (not (return_built_in_constant id))
       then true
       else
         let fin_lineno = Def.find_startline_no (Def.create_stmtlist st) in
         let expr = (((Ast_c.Ident (Ast_c.RegularName(id, ii))),
                      Ast_c.noType ()), []) in
         exists_error_handling_block_aux fin_lineno expr

     | Some ((Ast_c.Cast (_, (((Ast_c.ParenExpr ((((Ast_c.Ident ident),
                                                   _), _))), _), _))), _)
     | Some ((Ast_c.Cast (_, (((Ast_c.Ident ident), _), _))), _) ->
       let fin_lineno = Def.find_startline_no (Def.create_stmtlist st) in
       let expr = (((Ast_c.Ident ident), Ast_c.noType ()), []) in
       exists_error_handling_block_aux fin_lineno expr

     | _ -> true)

let rec find_errorhandling lbl_list = function
    []   -> []
  | h::t ->
    let build_errorhandling_block_list st =
      let outer_EHC = find_errorhandling lbl_list t in
      if Def.inner_block_in_compound_stmt st
      then
        let inner_EHC = find_errorhandling lbl_list (Def.create_stmtlist st) in
        inner_EHC@outer_EHC
      else
        outer_EHC
    in
    match Ast_c.unwrap h with
    | Ast_c.Labeled (Ast_c.Label (_, st))
    | Ast_c.Labeled (Ast_c.CaseRange  (_, _, st))
    | Ast_c.Labeled (Ast_c.Case  (_, st))
    | Ast_c.Selection  (Ast_c.Switch (_, st))
    | Ast_c.Iteration  (Ast_c.While (_, st))
    | Ast_c.Iteration  (Ast_c.DoWhile (st, _))
    | Ast_c.Iteration (Ast_c.For (Ast_c.ForExp _, _, _, st))
    | Ast_c.Iteration (Ast_c.MacroIteration (_, _, st))
    | Ast_c.Labeled (Ast_c.Default st) ->
      build_errorhandling_block_list st

    | Ast_c.Compound _ ->
      build_errorhandling_block_list h

    | Ast_c.Selection (Ast_c.If (e, st1, st2)) ->
      let outer_EHC  = find_errorhandling lbl_list t in
      let st1_normal = Def.create_stmtlist st1 in
      let st2_normal = Def.create_stmtlist st2 in
      let h_normal   = Def.create_stmtlist h in
      let stmt_list_st1 = gather_goto_code lbl_list [] st1_normal in
      let stmt_list_st2 = gather_goto_code lbl_list [] st2_normal in
      let stmt_list_st2 =
        if has_multiple_st stmt_list_st2
        then stmt_list_st2
        else []
      in
      let branch1_st_lineno = Def.find_startline_no st1_normal in
      let branch1_en_lineno = Def.find_endline_no stmt_list_st1 in
      let branch2_st_lineno = Def.find_startline_no st2_normal in
      let branch2_en_lineno = Def.find_endline_no stmt_list_st2 in
      let inner_EHC1 = find_errorhandling lbl_list stmt_list_st1 in
      let inner_EHC2 = find_errorhandling lbl_list stmt_list_st2 in
      let goto1 = Def.goto_exists_in_list st1_normal in
      let goto2 = Def.goto_exists_in_list st2_normal in
      let block1 =
        (Def.find_startline_no h_normal, e, goto1, st1_normal, Def.Then,
         branch1_st_lineno, branch1_en_lineno, stmt_list_st1)
      in
      let block2 =
        (Def.find_startline_no h_normal, e, goto2, st2_normal, Def.Else,
         branch2_st_lineno, branch2_en_lineno, stmt_list_st2)
      in
      let inner_EHC =
        match Def.inner_block_in_compound_stmt st1,
              Def.inner_block_in_compound_stmt st2,
              stmt_list_st1,
              stmt_list_st2,
              return_error e Def.Then stmt_list_st1,
              return_error e Def.Else stmt_list_st2
        with
          (true, true, _, _, _, _) -> inner_EHC1@inner_EHC2
        | (true, _, _, [], _, _    )
        | (true, _, _,  _, _, false) -> inner_EHC1
        | (true, _, _,  _, _, _    ) -> block2::inner_EHC1
        | (_, true, [], _, _    , _)
        | (_, true,  _, _, false, _) -> inner_EHC2
        | (_, true,  _, _, _    , _) -> block1::inner_EHC2
        | (_, _, x::xs, y::ys, true, true) -> block1::[block2]
        | (_, _, x::xs, y::ys, true, _   ) -> [block1]
        | (_, _, x::xs, y::ys, _   , true) -> [block2]
        | (_, _, x::xs, _    , true, _   ) ->
          Var_dec.all_ehc := !Var_dec.all_ehc + 1;
          [block1]
        | (_, _, _    , x::xs, _   , true) ->
          Var_dec.all_ehc := !Var_dec.all_ehc + 1;
          [block2]
        | _ -> []
      in
      inner_EHC@outer_EHC

    | Ast_c.Iteration (Ast_c.For (Ast_c.ForDecl _, _, _, _)) ->
      failwith "for loop with declaration in first argument not supported"

    | _ -> find_errorhandling lbl_list t
