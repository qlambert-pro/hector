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

(**************Is return statement access the resource  *********)


let rec find_ptr_args_list = function
    []->[]
  |   h::t-> match (Def.is_pointer h) with
      Def.IsPtr->h::find_ptr_args_list t
    |   Def.UnknownType-> h::find_ptr_args_list t
    |   _-> find_ptr_args_list t

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

let is_rrwa_alloc =
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
