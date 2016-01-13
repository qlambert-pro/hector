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

open Graph_operations

type resource_handling =
    Allocation
  | Release
  | None

type node = {
  is_error_handling: bool;
  resource_handling_type: resource_handling;
  parser_node: Control_flow_c.node
}

let mk_node is_error_handling resource_handling_type parser_node =
  {is_error_handling = is_error_handling;
   resource_handling_type = resource_handling_type;
   parser_node = parser_node
  }

type edge = Direct

type t = (node, edge) Ograph_extended.ograph_extended

(**** Unwrapper and boolean function on Control_flow_c ****)
let is_if_statement node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.IfHeader _ -> true
  | _ -> false

let is_after_node node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.AfterNode _ -> true
  | _ -> false

let test_returned_expression predicate node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.ReturnExpr (_, e) -> predicate e
  | _ -> false

let get_error_branch node =
  let {parser_node = parser_node} = node in
  let branch_side = ref Ast_operations.Then in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e ->
         Ast_operations.which_is_the_error_branch
           (fun x -> branch_side := x) e)
  }
  in
  Visitor_c.vk_node visitor parser_node;
  !branch_side

(* **
 * "node" is expected to be a IfHeader
 * and "head" one of the following node
 * *)
let is_on_error_branch cfg node head =
  let {parser_node = parser_node} = head in
  let error_branch_side = get_error_branch node in
  match (error_branch_side, Control_flow_c.unwrap parser_node) with
    (Ast_operations.Then, Control_flow_c.TrueNode _)
  | (Ast_operations.Else, Control_flow_c.FalseNode )
  | (Ast_operations.Else, Control_flow_c.FallThroughNode ) -> true
  | _ -> false

let is_killing_reach identifier node =
  let {parser_node = parser_node} = node in
  let error_assignment = ref false in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e ->
         Ast_operations.apply_on_assignment_left_side
           (fun x -> error_assignment :=
               !error_assignment ||
               Ast_operations.expression_equal identifier x)
           e;
         k e);
    Visitor_c.konedecl =
      (fun (k, visitor) dl ->
         Ast_operations.apply_on_initialised_variable
           (fun x -> error_assignment :=
               !error_assignment ||
               Ast_operations.expression_equal identifier x)
           dl;
         k dl)
  }
  in
  Visitor_c.vk_node visitor parser_node;
  !error_assignment

(*TODO clean / de-obfuscate*)
(* **
 * This function use a hack:
 * apply_on_error_assignment tests whether a variable receive an error value
 * this is why it is being used even if we are not actually using the left hand
 * side of the assignment
 * *)
let is_error_assignment node =
  let {parser_node = parser_node} = node in
  let error_assignment = ref false in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e ->
         Ast_operations.apply_on_error_assignment_left_side
           (fun e -> error_assignment := true)
           e;
         k e);
    Visitor_c.konedecl =
      (fun (k, visitor) dl ->
         Ast_operations.apply_on_error_initialised_variable
           (fun e -> error_assignment := true)
           dl;
         k dl)
  }
  in
  Visitor_c.vk_node visitor parser_node;
  !error_assignment

let identifiers_of_error_assignment node =
  let {parser_node = parser_node} = node in
  let identifiers = ref [] in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e ->
         Ast_operations.apply_on_error_assignment_left_side
           (fun x -> identifiers := x::!identifiers)
           e;
         k e);
    Visitor_c.konedecl =
      (fun (k, visitor) dl ->
         Ast_operations.apply_on_error_initialised_variable
           (fun x -> identifiers := x::!identifiers)
           dl;
         k dl)
  }
  in
  Visitor_c.vk_node visitor parser_node;
  !identifiers
(**********************************************************)

let get_error_assignments cfg =
  find_all
    (fun (_, node) -> is_error_assignment node)
    cfg


let get_reachable_returns cfg error_assignments =
  let get_reachable_returns_aux acc (index, node) =
    let identifiers = identifiers_of_error_assignment node in
    let reachable_nodes_by_identifier =
      List.map
        (fun identifier -> conditional_breadth_first_search
            (fun (idx, node) -> not (is_killing_reach identifier node))
            cfg index)
        identifiers
    in
    let reachable_returns_by_identifier =
      List.map2
        (fun reachable_nodes identifier ->
           (NodeiSet.filter
              (fun index ->
                 test_returned_expression
                   (Ast_operations.expression_equal_unwrap identifier)
                   (cfg#nodes#assoc index))
              reachable_nodes))
        reachable_nodes_by_identifier
        identifiers
    in
    let reachable_returns =
      List.fold_left (fun acc s -> NodeiSet.union acc s)
        NodeiSet.empty reachable_returns_by_identifier in
    NodeiSet.union acc reachable_returns
  in
  List.fold_left get_reachable_returns_aux NodeiSet.empty error_assignments


(* **
 * Returns true if head is in a block leading directly to an error return
 * statement
 * *)
let is_returning_error error_returns cfg head =
  let rec is_returning_error_aux index =
    let node = cfg#nodes#assoc index in
    test_returned_expression Ast_operations.is_error node ||
    NodeiSet.mem index error_returns ||
    (match (cfg#successors index)#tolist with
       [(succ, _)] -> is_returning_error_aux succ
     | _           -> false)
  in
  is_returning_error_aux head

let get_returning_branch_heads cfg node =
  let successors = (cfg#successors node)#tolist in
  List.map fst successors


let annotate_error_handling_block cfg head =
  let rec annotate_error_handling_block_aux cfg node =
    let {parser_node = parser_node} = cfg#nodes#assoc node in
    let ncfg = cfg#replace_node (node, (mk_node true None parser_node)) in
    (match (cfg#successors node)#tolist with
       [(succ, _)] -> annotate_error_handling_block_aux ncfg succ
     | []          -> ncfg
     | _           ->
       failwith
         "annotate_error_handling_block should be called on an error handling block")
  in
  annotate_error_handling_block_aux cfg head


let annotate_error_handling cfg =
  let error_assignments = get_error_assignments cfg in
  let error_returns = get_reachable_returns cfg error_assignments in
  let annotate_error_handling_aux acc (index, node)=
    if is_if_statement node
    then
      let returning_branch_heads = get_returning_branch_heads acc index in

      let annotate_if_error_handling acc branch_head =
        if (is_returning_error error_returns acc branch_head ||
            is_on_error_branch acc node (cfg#nodes#assoc branch_head))
        then
          annotate_error_handling_block acc branch_head
        else
          acc
      in
      List.fold_left annotate_if_error_handling acc returning_branch_heads

    else
      acc
  in
  fold_node annotate_error_handling_aux cfg cfg


(*TODO implement*)
let annotate_resource_handling cfg =
  cfg


(* TODO improve to only one pass *)
let of_ast_c ast =
  let cocci_cfg =
    match Control_flow_c_build.ast_to_control_flow ast with
      Some cfg -> cfg
    | None     -> failwith "unable to build control_flow_c"
  in

  let process_node (g, added_nodes) (index, node) =

    let add_node (g, added_nodes) (index, node) =
      if not (Hashtbl.mem added_nodes index)
      then
        (* **
         * It uses add_nodei so that coccinelle can be used to debug with
         * --control-flow
         * *)
        let (g, index') = g#add_nodei index (mk_node false None node) in
        Hashtbl.add added_nodes index index';
        (g, added_nodes)
      else
        (g, added_nodes)
    in

    let (g', added_nodes') = add_node (g, added_nodes) (index, node) in
    let successors = cocci_cfg#successors index in

    let add_node_and_arc (g, added_node) (index', _) =
      let node' = (cocci_cfg#nodes)#assoc index' in
      let (g', added_nodes') = add_node (g, added_nodes) (index', node') in

      let start_node = Hashtbl.find added_nodes' index  in
      let end_node   = Hashtbl.find added_nodes' index' in
      let g'' =
        (* **
         * Do not copy edges to after_node
         * *)
        if is_after_node ((g'#nodes)#assoc end_node)
        then g'
        else g'#add_arc ((start_node, end_node), Direct)
      in
      (g'', added_nodes')
    in

    List.fold_left add_node_and_arc (g', added_nodes') (successors#tolist)
  in
  let cfg' =
    fst (fold_node process_node
           (new Ograph_extended.ograph_extended, Hashtbl.create 100)
           cocci_cfg)
  in
  let cfg = annotate_error_handling cfg' in
  annotate_resource_handling cfg
