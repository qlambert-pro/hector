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
