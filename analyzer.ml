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

let find_missing_resource_release block error_blocks c_function resources =
  let is_already_in_list args_list releases =
    List.exists
      (fun (Resource.Resource (_, args, _)) -> Def.compare_explists args args_list)
      releases
  in
  let find_missing_resource_release_aux acc = function
      (Resource.Resource (alloc, args, h)) as resource ->
      if not (Block.does_block_contains_statement block h) &&
         not (is_already_in_list args acc) &&
         args != [] &&
         C_function.resource_of_release_temp block error_blocks c_function resource
         &&
         not (Block.return_st_access_resource resource block) &&
         C_function.is_resource_allocated_properly block error_blocks c_function
           resource &&
         C_function.is_resource_released block error_blocks c_function resource

      then resource::acc
      else acc
  in
  List.fold_left find_missing_resource_release_aux [] resources

let all_def = ref []

let analyze_blk c_function rr_ops_list iifunc1 all_def func_name errblks_list
    block =

  let miss_rr_ops_list =
    find_missing_resource_release block errblks_list c_function rr_ops_list in

  let miss_rr_ops_list_new5 =
    C_function.find_interproc_calls all_def block c_function miss_rr_ops_list in

  Report.generate_report_new func_name block iifunc1 miss_rr_ops_list_new5

let analyze_def full_file all_fn ((defbis, iifunc1::_) : Ast_c.definition) =
  let {Ast_c.f_name = name;
       Ast_c.f_body = body;
      } = defbis
  in
  let func_name = fst (Ast_c.get_s_and_ii_of_name name) in

  Var_dec.func_name := func_name;
  Var_dec.file_info := Ast_c.file_of_info iifunc1;

  let clean_ast = Def.remove_stmtElelist body in
  let c_function = C_function.mk_c_function clean_ast in

  let error_blocks = C_function.find_errorhandling c_function in
  let resource_releases = C_function.get_resources c_function error_blocks in
  List.iter (analyze_blk c_function resource_releases iifunc1 !all_def func_name
               error_blocks) error_blocks

let analyze_toplevel file all_fn x =
  match x with
    Ast_c.Definition def -> analyze_def file all_fn def
  | Ast_c.Declaration _
  | Ast_c.CppTop _
  | Ast_c.EmptyDef _
  | Ast_c.NotParsedCorrectly _
  | Ast_c.FinalDef _
  | Ast_c.IfdefTop _
  | Ast_c.MacroTop _ -> ()
  | _ -> ()
