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

module GO = Graph_operations
module CFG = Control_flow_c
module ACFG = Annotated_cfg
module Asto = Ast_operations
module KFP = Key_fix_point

module CFGOps = GO.Make (Control_flow_c)
module ACFGOps = GO.Make (ACFG)

module type EqualType =
sig
  type t
  val equal: t -> t -> bool
end

module BoolValue =
struct
  type t = bool
  let equal = (=)
end

module CFG_Fixpoint  (Val: EqualType) = Fix_point.Make (Val) (CFGOps)
module ACFG_Fixpoint (Val: EqualType) = Fix_point.Make (Val) (ACFGOps)

module CFG_Bool_Fixpoint  = CFG_Fixpoint  (BoolValue)
module ACFG_Bool_Fixpoint = ACFG_Fixpoint (BoolValue)

module CFG_KeyFixPoint  = KFP.Make (CFG.KeySet)  (CFGOps)  (CFG_Bool_Fixpoint)
module ACFG_KeyFixPoint = KFP.Make (ACFG.KeySet) (ACFGOps) (ACFG_Bool_Fixpoint)

let mk_node is_error_handling parser_node = {
  ACFG.is_error_handling = is_error_handling;
  ACFG.resource_handling_type = ACFG.Unannotated;
  ACFG.referenced_resources = Asto.ExpressionSet.empty;
  ACFG.parser_node = parser_node
}

let is_after_node node =
  match Control_flow_c.unwrap node with
    Control_flow_c.AfterNode _ -> true
  | _ -> false


let line_number_of_node cfg cn =
  let rec aux cn =
    let st =
      Control_flow_c.extract_fullstatement cn.GO.node.ACFG.parser_node
    in
    let number_of_successors =
      ACFG.KeyEdgeSet.cardinal (cfg#successors cn.GO.index)
    in
    match (st, number_of_successors) with
      (  None, 1) ->
      let (i, _) = ACFG.KeyEdgeSet.choose (cfg#successors cn.GO.index) in
      aux (ACFGOps.complete_node_of cfg i)
    | (Some s, _) ->
      let (_, _, (l, _), _) =
        Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_stmt s)
      in
      l
    | (  None, _) ->
      failwith
        "the block head does not reach a node with line infos before branching."
  in
  aux cn

let get_top_node cfg =
  let top_nodes = ACFGOps.find_all (fun _ n -> ACFG.is_top_node n) cfg in
  if ACFG.KeyMap.cardinal top_nodes = 1
  then
    let (i, n) = ACFG.KeyMap.choose top_nodes in
    {GO.index = i; GO.node = n}
  else
    failwith "malformed control flow graph"

let configurable_is_reference config cfg cn r =
  let nodes = ACFG_Bool_Fixpoint.compute config cfg cn in
  ACFG.KeySet.fold
    (fun i acc ->
       acc &&
       not (ACFG.is_referencing_resource r (ACFG.KeyMap.find i cfg#nodes)))
    nodes
    true

let is_last_reference =
  let just_true = (fun _ _ -> true) in
  configurable_is_reference
    (ACFG_KeyFixPoint.get_basic_forward_config
       just_true ACFG.KeySet.empty)

let is_last_reference_before_killed_reached identifier =
  let kill_reach _ (n, _) =
    not (ACFG.is_killing_reach identifier n.GO.node)
  in
  configurable_is_reference
    (ACFG_KeyFixPoint.get_basic_forward_config kill_reach ACFG.KeySet.empty)

let is_first_reference =
  let just_true = (fun _ _ -> true) in
  configurable_is_reference
    (ACFG_KeyFixPoint.get_basic_backward_config just_true ACFG.KeySet.empty)


let is_return_value_tested cfg cn =
  let assigned_variable = ref None in
  ACFG.apply_side_effect_visitor
    (fun l _ _ -> assigned_variable := Some l)
    cn.GO.node;
  match !assigned_variable with
    None    -> false
  | Some id ->
    let just_true = (fun _ _ -> true) in
    let downstream_nodes =
      ACFG_Bool_Fixpoint.compute
        (ACFG_KeyFixPoint.get_basic_forward_config just_true ACFG.KeySet.empty)
        cfg cn
    in
    ACFG.KeySet.for_all
      (fun i ->
         ACFG.test_if_header
           (fun e -> Asto.is_testing_identifier id e)
           false (ACFG.KeyMap.find i cfg#nodes))
      downstream_nodes


let remove_after_nodes_mutable cfg =
  let remove_pred_arcs i =
    CFGOps.fold_predecessors
      (fun (cn, e) _ -> cfg#del_arc ((cn.GO.index, i), e))
      cfg i ()
  in
  let remove_succ_arcs i =
    CFGOps.fold_successors
      (fun (cn, e) _ -> cfg#del_arc ((i, cn.GO.index), e))
      cfg i ()
  in
  let remove_after_node i node () =
    if is_after_node node
    then
      (remove_pred_arcs i;
       remove_succ_arcs i;
       cfg#del_node i)
    else
      ()
  in
  CFGOps.fold_node cfg remove_after_node ()


let of_ast_c ast =
  let cocci_cfg =
    match Control_flow_c_build.ast_to_control_flow ast with
      Some cfg -> Control_flow_c_build.annotate_loop_nodes cfg
    | None     -> raise ACFG.NoCFG
  in

  remove_after_nodes_mutable cocci_cfg;
  let cfg = new ACFG.G.ograph_mutable in
  let added_nodes = Hashtbl.create 100 in

  let process_node cn =
    let add_node cn =
      if not (Hashtbl.mem added_nodes cn.GO.index)
      then
        let index' = cfg#add_node (mk_node false cn.GO.node) in
        Hashtbl.add added_nodes cn.GO.index index'
      else
        ()
    in

    let add_node_and_arc (index', _) =
      let node' = Control_flow_c.KeyMap.find index' cocci_cfg#nodes in
      add_node {GO.index = index'; GO.node = node'};

      let start_node = Hashtbl.find added_nodes cn.GO.index in
      let end_node   = Hashtbl.find added_nodes index'   in
      let post_dominated =
        CFG_KeyFixPoint.conditional_get_post_dominated
          (fun _ -> true)
          cocci_cfg (CFGOps.complete_node_of cocci_cfg cn.GO.index)
      in

      let edge_type =
        if Control_flow_c.KeySet.mem index' post_dominated
        then ACFG.PostBackedge
        else ACFG.Direct
      in
      let edge =
        {ACFG.start_node = start_node;
         ACFG.end_node   = end_node;
         ACFG.edge_type  = edge_type;}
      in
      cfg#add_arc ((start_node, end_node), edge)
    in

    add_node cn;
    let successors = cocci_cfg#successors cn.GO.index in
    Control_flow_c.KeyEdgeSet.iter add_node_and_arc successors
  in
  let top_node = Control_flow_c.first_node cocci_cfg in
  process_node {GO.index = top_node;
                GO.node = Control_flow_c.KeyMap.find top_node cocci_cfg#nodes };
  CFG_Bool_Fixpoint.compute
    (CFG_Bool_Fixpoint.get_forward_config
       (fun _ _ -> true)
       (fun _ (cn, _) () -> process_node cn)
       (fun _ _ -> true) true ())
    cocci_cfg
    (CFGOps.complete_node_of cocci_cfg top_node);
  cfg

let get_parameters cfg =
  let aux k n acc =
    match Control_flow_c.unwrap n.ACFG.parser_node with
      CFG.FunHeader (definition, _) ->
      let (_, (parameters, _)) = definition.Ast_c.f_type in
      List.fold_left
        (fun acc p ->
           match Asto.expression_of_parameter p with
             Some e -> Asto.ExpressionSet.add e acc
           | _ -> acc)
        acc parameters
    | _ -> acc
  in
  ACFGOps.fold_node cfg aux Asto.ExpressionSet.empty
