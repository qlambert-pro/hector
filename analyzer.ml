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


let rec create_rr_ops_list rr_ops_list = function
    [] -> rr_ops_list
  | (brnch_strtlineno,test_case,typ,blk_strtlineno,blk_endlineno,stmtlist)::t ->
    let new_rr_ops_list = Rr_op_finder.stack_rr_op_new rr_ops_list stmtlist in
    create_rr_ops_list new_rr_ops_list t


let rec recheck_blks c_function = function
    [] -> []
  | (brnch_strtlineno,test_case,goto,st,typ,blk_strtlineno,blk_endlineno,stmtlist)::t->
    if not (Errorhandling.exists_error_handling_block test_case c_function stmtlist typ blk_strtlineno)
    then recheck_blks c_function t
    else
      (brnch_strtlineno,test_case,goto,st,typ,blk_strtlineno,blk_endlineno,stmtlist)::(recheck_blks
                                                                                         c_function t)


let rec refine_rr_ops_inner args_list = function
    [] -> false
  | (alloc, args, rrl)::t ->
    if (Def.compare_explists args args_list) then true
    else refine_rr_ops_inner args_list t


let rec refine_rr_ops = function
    [] -> []
  | (alloc, args, rrl)::t ->
    if (refine_rr_ops_inner args t) then refine_rr_ops t
    else (alloc, args, rrl)::(refine_rr_ops t)

let analyze_each_blk_new c_function rr_ops_list iifunc1 all_def func_name errblks_list =
  let rec analyze_each_blk_loop_new = function
      [] -> []
    | (brnch_strtlineno,test_case,typ,blk_strtlineno,blk_endlineno,stmtlist)::t ->
      let miss_rr_ops_list = Rr_op_finder.find_missing_rr_ops_new c_function stmtlist rr_ops_list in
      let update_miss_rr_ops_list = refine_rr_ops miss_rr_ops_list in
      let miss_rr_ops_list_new = Rm_true_positives.is_resource_having_same_def_new
          blk_strtlineno c_function errblks_list update_miss_rr_ops_list in
      let miss_rr_ops_list1 = Rm_true_positives.is_rrwa_alloc errblks_list
          blk_strtlineno c_function miss_rr_ops_list_new  in
      let miss_rr_ops_list_new2 = Rm_true_positives.return_resource_new
          c_function stmtlist miss_rr_ops_list1 in
      let miss_rr_ops_list_new3 =
        Rm_true_positives.rls_in_exe_paths blk_strtlineno c_function errblks_list test_case
          stmtlist [] miss_rr_ops_list_new2 in
      let miss_rr_ops_list_new4 =
        C_function.find_interproc_calls stmtlist all_def blk_strtlineno
          c_function miss_rr_ops_list_new3 in
      let miss_rr_ops_list_new5 =
        Rm_true_positives.resource_is_not_allocated_yet errblks_list
          blk_strtlineno c_function (List.rev miss_rr_ops_list_new4) in
      let _ = Report.generate_report_new func_name blk_strtlineno iifunc1
          miss_rr_ops_list_new5
      in
      analyze_each_blk_loop_new t
  in analyze_each_blk_loop_new errblks_list

let rec loop f_basic f_goto list = function
    [] -> (list, f_basic, f_goto)
  | (brnch_strtlineno,test_case,goto,st_normal,typ,blk_strtlineno,blk_endlineno,stmtlist)::t ->
    match goto with
      Some a -> loop f_basic true   ((brnch_strtlineno, test_case, typ, blk_strtlineno, blk_endlineno, stmtlist)::list) t
    |	_      -> loop true    f_goto ((brnch_strtlineno, test_case, typ, blk_strtlineno, blk_endlineno, stmtlist)::list) t


let analyze_def full_file all_fn = function
    (defbis, iifunc1::iifunc2::i1::i2::ifakestart::isto) ->
    let {Ast_c.f_name = name;
         Ast_c.f_body = body;
        } = defbis
    in
    let lbl_list = Def.create_lbl_list [] [] (Def.remove_stmtElelist body) in
    let func_name = fst (Ast_c.get_s_and_ii_of_name name) in

    Var_dec.func_name := func_name;
    Var_dec.file_info := Ast_c.file_of_info iifunc1;

    let clean_ast = Def.remove_stmtElelist body in
    let function_data = C_function.mk_c_function clean_ast in
    let new_errblks_list = C_function.find_errorhandling function_data in
    let updated_blk_list = recheck_blks function_data new_errblks_list in
    let updated_blk_list =
      Rm_true_positives.remove_blks_that_returns_resource function_data updated_blk_list updated_blk_list in
    let (updated_blk_list, f_basic, f_goto) =
      loop false false [] updated_blk_list
    in
    let rr_ops_list_new =
      match updated_blk_list with
        h1::h2::t -> create_rr_ops_list [] updated_blk_list
      | _ -> []
    in
    let _ =
      match (updated_blk_list, rr_ops_list_new) with
        (h1::h2::t, l::u) -> analyze_each_blk_new function_data rr_ops_list_new iifunc1 !all_def func_name updated_blk_list
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
