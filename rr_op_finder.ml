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

(* Find resource releasing operations from error handling block *)


let rec exp_exists_in_stmtlist exp = function
    []-> false
  | h::t-> match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
      if(Def.exp_exists_in_stmt exp h) then true
      else exp_exists_in_stmtlist exp t

    |_-> exp_exists_in_stmtlist exp t

let rec no_exp_exists_in_stmtlist list  = function
    []-> true
  | h::t-> if (exp_exists_in_stmtlist (Some h) list) then false
    else no_exp_exists_in_stmtlist list t


let exp_exists_in_stmtlist_new exp =
  let exp_exists_in_stmtlist_new_aux statement =
    match Ast_c.unwrap statement with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (_, es)), _), _)) ->
      let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
      Def.exp_exists_in_list exp args_list
    | _ -> false
  in
  List.exists exp_exists_in_stmtlist_new_aux


let all_exp_exists_in_list tail ptr_args_list =
  List.for_all (fun arg -> exp_exists_in_stmtlist_new arg tail) ptr_args_list


let rec unused_ptr_args list = function
    []-> []
  | h::t-> if (exp_exists_in_stmtlist_new h list) then
      unused_ptr_args list t
    else h::(unused_ptr_args list t)


let find_ptr_args_list args =
  let pointer_test p acc =
    match Def.is_pointer p with
      Def.IsPtr
    | Def.UnknownType -> p::acc
    | _ -> acc
  in
  let find_ptr_args_list_aux arg acc =
    match arg with
      (((Ast_c.Cast (_, p)), _), _) ->
      pointer_test p acc
    | _ ->
      pointer_test arg acc
  in
  List.fold_right find_ptr_args_list_aux args []


let rec create_argslist args_list = function
    []   -> args_list
  | h::t ->
    match Ast_c.unwrap h with
      Common.Left ((exp, typ), ii) ->
      create_argslist (args_list@[Some ((exp, typ), ii)]) t
    | _ -> create_argslist args_list t


let args_contains_array = function
    (((Ast_c.ArrayAccess _), _), _) -> true
  | _ -> false

let rec remove_int = function
    []   -> []
  | h::t ->
    match Def.is_pointer h with
      Def.IsntPtr -> remove_int t
    | _ -> h::remove_int t


let first_arg_is_array args_list =
  match remove_int args_list with
    []   -> false
  | h::t -> args_contains_array h


let stack_rr_op_new rr_ops_list statements =
  let stack_rr_op_new_aux (rr_ops_list, tail) statement =
    let ntail =
      if tail = []
      then []
      else List.tl tail
    in
    match Ast_c.unwrap statement with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident (Ast_c.RegularName (fn_n, [ii3]))), typ1), ii1), es)), typ), ii)) ->
      let args_list = Def.remove_optionlist (create_argslist [] es) in
      if (List.length args_list) = 0 &&
         not (Def.stmt_exists_in_list statement (List.map snd rr_ops_list))
      then
        ((([], statement)::rr_ops_list), ntail)
      else
      if not (first_arg_is_array args_list ||
              Def.string_exists_in_explist args_list ||
              Def.stmt_exists_in_list statement (List.map snd rr_ops_list))
      then
        let ptr_args_list = find_ptr_args_list args_list in
        if not (ptr_args_list = [] ||
                all_exp_exists_in_list tail ptr_args_list)
        then
          let unused_args = unused_ptr_args tail ptr_args_list in
          if no_exp_exists_in_stmtlist tail unused_args
          then
            (((unused_args, statement)::rr_ops_list), ntail)
          else
            (rr_ops_list, ntail)
        else
          (rr_ops_list, ntail)
      else
        (rr_ops_list, ntail)
    | _ -> (rr_ops_list, ntail)
  in
  fst (List.fold_left stack_rr_op_new_aux (rr_ops_list, List.tl statements) statements)

(*why is release used as alloc ?*)
let find_missing_rr_ops_new c_function errblk rr_ops_list =
  let updated_errblk_with_goto_code =
    C_function.gather_goto_code_from_block c_function errblk in
  if updated_errblk_with_goto_code = []
  then []
  else
    let rec find_missing_rr_ops_inner = function
        [] -> []
      | (Resource.Release (args, h))::t ->
        if not (Def.stmt_exists_in_list h updated_errblk_with_goto_code)
        then
          (Resource.Resource (h, args, h))::(find_missing_rr_ops_inner t)
        else
          find_missing_rr_ops_inner  t
    in find_missing_rr_ops_inner rr_ops_list
