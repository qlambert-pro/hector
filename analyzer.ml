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

module GO    = Graph_operations
module ACFG  = Annotated_cfg
module ACFGO = Acfg_operations
module CF    = C_function
module HC    = Hector_core
module Asto  = Ast_operations

module ACFGOps = GO.Make (ACFG)

type function_data = {
  toplevel: Ast_c.toplevel;
  info: Ast_c.info;
  cfg: Annotated_cfg.t;
}

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

let analyze_omissions data =
  let func_name = Asto.get_name data.toplevel in
  try
    HC.annotate_error_handling data.cfg;
    HC.annotate_resource_handling data.cfg;

    let error_blocks = C_function.find_errorhandling data.cfg in
    let exemplars = C_function.get_exemplars data.cfg error_blocks in

    let candidates =
      List.fold_left
        (fun acc e ->
           acc @
           C_function.get_faults data.cfg error_blocks e)
        []
        exemplars
    in

    let faults = List.fold_left remove_doubles [] candidates in

    List.iter
      (fun c -> Report.generate_report_new data.cfg func_name data.info c)
      faults

  with
    Control_flow_c_build.Error _
  | Annotated_cfg.NoCFG -> ()

let analyze_release data =
  let parameters = ACFGO.get_parameters data.cfg in
  let releases =
    ACFGOps.fold_node data.cfg
      (fun i n a ->
         let node = {GO.index = i; GO.node = n} in
         match HC.get_released_resource data.cfg node with
           Some (ACFG.Resource e) -> Asto.ExpressionSet.add e a
         | _ -> a)
      Asto.ExpressionSet.empty
  in
  not (Asto.ExpressionSet.is_empty
         (Asto.ExpressionSet.inter parameters releases))

let function_data_of_toplevel filename =
  let (program, _) = Parse_c.parse_c_and_cpp false false filename in
  let (functions', _) = Common.unzip program in
  let functions'' = List.tl (List.rev functions') in
  List.fold_left
    (fun acc (toplevel, _) ->
       let cfg =
         Asto.apply_if_function_definition
           (Asto.ToplevelAndInfo
              (fun toplevel info -> Some (ACFGO.of_ast_c toplevel, info)))
           toplevel None
       in
       match cfg with
         Some (cfg, info) -> {toplevel = toplevel; info = info; cfg = cfg}::acc
       | None             -> acc)
    []
    (Type_annoter_c.annotate_program !Type_annoter_c.initial_env functions'')
