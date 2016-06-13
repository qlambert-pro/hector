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

module ACFG = Annotated_cfg
module CF   = C_function
module GO   = Graph_operations


let remove_doubles acc c =
  if List.exists
      (fun f ->
         ACFG.resource_equal f.CF.exemplar.CF.res c.CF.exemplar.CF.res &&
         f.CF.block_head.GO.index = c.CF.block_head.GO.index)
      acc
  then
    acc
  else
    c::acc

let analyze_def toplevel definition infos =
  let func_name = fst (Ast_c.get_s_and_ii_of_name definition.Ast_c.f_name) in
  Printf.eprintf "%s\n%!" func_name;

  try
    let cfg =
      (Common.profile_code "cfg"
         (fun () -> Annotated_cfg.of_ast_c toplevel))
    in

    let error_blocks =
      Common.profile_code "find_errorhandling"
        (fun () -> C_function.find_errorhandling cfg)
    in

    let exemplars =
      Common.profile_code "get_exemplars"
        (fun () -> C_function.get_exemplars cfg error_blocks)
    in

    (*TODO get allocations and use them instead of exemplars*)
    let candidates =
      List.fold_left
        (fun acc e ->
           acc @
           C_function.get_faults cfg error_blocks e)
        []
        exemplars
    in

    let faults = List.fold_left remove_doubles [] candidates in

    List.iter (fun c -> Report.generate_report_new func_name infos c) faults

  with
    Control_flow_c_build.Error _
  | Annotated_cfg.NoCFG ->
    ()

let analyze_toplevel x =
  match x with
    Ast_c.Definition (defbis, infos::_) ->
    analyze_def x defbis infos
  | Ast_c.Declaration _
  | Ast_c.CppTop _
  | Ast_c.EmptyDef _
  | Ast_c.NotParsedCorrectly _
  | Ast_c.FinalDef _
  | Ast_c.IfdefTop _
  | Ast_c.MacroTop _ -> ()
  | _ -> ()
