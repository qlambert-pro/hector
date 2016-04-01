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

module Asto = Ast_operations

exception NoCFG

type resource_handling =
    Allocation
  | Release
  | NoResource

type node = {
  is_error_handling: bool;
  resource_handling_type: resource_handling;
  parser_node: Control_flow_c.node
}

let mk_node is_error_handling resource_handling_type parser_node = {
  is_error_handling = is_error_handling;
  resource_handling_type = resource_handling_type;
  parser_node = parser_node
}

type edge = Direct

type t = (node, edge) Ograph_extended.ograph_extended

(**** Unwrapper and boolean function on Control_flow_c ****)
let get_assignment_type_through_alias cfg i e =
  match Asto.get_assignment_type e with
    Asto.Variable e -> Asto.NonError
  | Asto.Value v    -> v

let is_loop node =
  let {parser_node = ((_, info), _)} = node in
  let {Control_flow_c.is_loop = is_loop} = info in
  is_loop

(*TODO actually use the graph rather than node type*)
let is_killing_error_branch node =
  let {parser_node = ((_, info), _) as parser_node} = node in
  let {Control_flow_c.is_loop = is_loop} = info in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.FalseNode
  | Control_flow_c.IfHeader _
  | Control_flow_c.SwitchHeader _
  | Control_flow_c.FallThroughNode -> true
  | _ -> is_loop


let is_after_node node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.AfterNode _ -> true
  | _ -> false


let is_selection node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.IfHeader _ -> true
  | _ -> false


let is_fallthrough node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.FallThroughNode -> true
  | _ -> false


let is_top node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.TopNode -> true
  | _ -> false


let test_returned_expression predicate default node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.ReturnExpr (_, (e, _)) -> predicate e
  | _ -> default


let test_if_header predicate default node =
  let {parser_node = parser_node} = node in
  match Control_flow_c.unwrap parser_node with
    Control_flow_c.IfHeader (_, (e, _)) -> predicate e
  | _ -> default


let get_error_branch cfg (i, node) =
  let {parser_node = parser_node} = node in
  let branch_side = ref Asto.Then in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e ->
         Asto.which_is_the_error_branch
           (get_assignment_type_through_alias cfg i)
           (fun x -> branch_side := x) e)
  }
  in
  Visitor_c.vk_node visitor parser_node;
  !branch_side

(* **
 * "node" is expected to be a IfHeader
 * and "head" one of the following node
 * *)
let is_on_error_branch cfg (i, node) head =
  let {parser_node = parser_node} = head in
  let error_branch_side = get_error_branch cfg (i,node) in
  match (error_branch_side, Control_flow_c.unwrap parser_node) with
    (Asto.Then, Control_flow_c.TrueNode _)
  | (Asto.Else, Control_flow_c.FalseNode )
  | (Asto.Else, Control_flow_c.FallThroughNode ) -> true
  | _ -> false

let base_visitor f = {
  Visitor_c.default_visitor_c with
  Visitor_c.kexpr =
    (fun (k, visitor) e ->
       Asto.apply_on_assignment
         f
         e;
       k e);
  Visitor_c.konedecl =
    (fun (k, visitor) dl ->
       Asto.apply_on_initialisation
         f
         dl;
       k dl)
}

let base_error_visitor cfg i f = {
  Visitor_c.default_visitor_c with
  Visitor_c.kexpr =
    (fun (k, visitor) e ->
       Asto.apply_on_error_assignment
         (get_assignment_type_through_alias cfg i)
         f
         e;
       k e);
  Visitor_c.konedecl =
    (fun (k, visitor) dl ->
       Asto.apply_on_error_initialisation
         (get_assignment_type_through_alias cfg i)
         f
         dl;
       k dl)
}

let is_killing_reach identifier node =
  let {parser_node = parser_node} = node in
  let error_assignment = ref false in
  let visitor = base_visitor
      (fun r l -> error_assignment :=
          !error_assignment ||
          Asto.expression_equal identifier r)
  in
  Visitor_c.vk_node visitor parser_node;
  !error_assignment

let is_error_assignment cfg (i, node) =
  let {parser_node = parser_node} = node in
  let error_assignment = ref false in
  let visitor = base_error_visitor
      cfg i
      (fun l r ->
         error_assignment :=
           match r with
             Some r ->
             (match get_assignment_type_through_alias cfg i r with
                Asto.Error _ -> true
              | _    -> !error_assignment)
           | None -> true)
  in
  Visitor_c.vk_node visitor parser_node;
  !error_assignment

let assignement_type_of_error_assignement cfg identifier (i, node) =
  let {parser_node = parser_node} = node in
  let error_assignment = ref Asto.NonError in
  let visitor = base_error_visitor
      cfg i
      (fun x r ->
         if Asto.expression_equal x identifier
         then
           error_assignment :=
             match r with
               Some r -> get_assignment_type_through_alias cfg i r
             | None   -> Asto.NonError
         else
           ())
  in
  Visitor_c.vk_node visitor parser_node;
  !error_assignment

let identifiers_of_error_assignment cfg (i, node) =
  let {parser_node = parser_node} = node in
  let identifiers = ref [] in
  let visitor = base_error_visitor
      cfg i
      (fun x r -> identifiers := x::!identifiers)
  in
  Visitor_c.vk_node visitor parser_node;
  !identifiers
(**********************************************************)

let get_error_assignments cfg =
  find_all
    (fun n -> is_error_assignment cfg n)
    cfg


let filter_returns cfg identifier nodes =
  NodeiSet.filter
    (fun index ->
       test_returned_expression
         (Asto.expression_equal identifier)
         false
         (cfg#nodes#assoc index))
    nodes


let add_branch_nodes_leading_to_return cfg returns nodes acc =
  NodeiSet.fold
    (fun return acc ->
       let branch_nodes =
         conditional_get_post_dominated
           (fun (index, node) -> NodeiSet.mem index nodes)
           cfg
           return
       in
       NodeiSet.union branch_nodes acc)
    returns acc

let add_post_dominated cfg index acc =
  let post_dominated =
    conditional_get_post_dominated
      (fun (_, node) -> true)
      cfg index
  in
  NodeiSet.union post_dominated acc

let get_nodes_leading_to_error_return cfg error_assignments =
  let get_reachable_nodes acc (index, node) =
    let identifiers = identifiers_of_error_assignment cfg (index, node) in
    List.fold_left
      (fun acc identifier ->
         let assignement_type =
           assignement_type_of_error_assignement cfg identifier (index, node)
         in
         match assignement_type with
           Asto.Error Asto.Clear ->
           let nodes =
             breadth_first_fold
               (get_basic_node_config
                  (fun _ (idx, node) -> not (is_killing_reach identifier node)))
               cfg index
           in
           let reachable_returns = filter_returns cfg identifier nodes in
           if reachable_returns != NodeiSet.empty
           then
             let error_branch_nodes =
               add_branch_nodes_leading_to_return
                 cfg reachable_returns nodes acc
             in
             if NodeiSet.mem index error_branch_nodes
             then
               add_post_dominated cfg index error_branch_nodes
             else
               error_branch_nodes
           else
             acc
         | Asto.Error Asto.Ambiguous ->
           let nodes =
             depth_first_fold
               (get_forward_config
                  (fun _ (idx, node) ->
                     not (is_killing_reach identifier node))
                  (*TODO is_testing_identifier is incorrect*)
                  (* add complex if cases to prove it *)
                  (*TODO use a finer memory of the error branch *)
                  (fun _ i (pred, acc) ->
                     match pred with
                       None -> (Some i, acc)
                     | Some pred ->
                       let is_on_error_branch_result =
                         let head = cfg#nodes#assoc pred in
                         (not (test_if_header
                                 (Asto.is_testing_identifier
                                    identifier) false head) ||
                          is_on_error_branch cfg (pred, head) node)
                       in
                       (Some i, acc || is_on_error_branch_result))
                  (fun (_, acc) i res -> if acc then NodeiSet.add i res else res)
                  (None, false)
                  NodeiSet.empty)
               cfg index
           in
           let reachable_returns = filter_returns cfg identifier nodes in
           if reachable_returns != NodeiSet.empty
           then
             add_branch_nodes_leading_to_return cfg reachable_returns nodes acc
           else
             acc
         | _ -> acc)
      acc identifiers
  in
  List.fold_left get_reachable_nodes NodeiSet.empty error_assignments

(*TODO optimise number of pass*)
(*TODO replace as computed rather than store*)
let annotate_error_handling cfg =
  let error_assignments = get_error_assignments cfg in
  let error_branch_nodes = get_nodes_leading_to_error_return cfg
      error_assignments in
  let error_returns =
    find_all
      (fun (i, node) ->
         let error_type =
           test_returned_expression (get_assignment_type_through_alias cfg i)
             Asto.NonError node
         in
         match error_type with
           Asto.Error Asto.Clear -> true
         | _                     -> false)
      cfg
  in
  let error_return_post_dominated =
    List.fold_left
      (fun acc (index, _) ->
         let nodes =
           conditional_get_post_dominated (fun _ -> true) cfg index
         in
         NodeiSet.union acc nodes)
      NodeiSet.empty error_returns
  in
  fold_node
    (fun cfg (index, node) ->
       if NodeiSet.mem index error_branch_nodes ||
          NodeiSet.mem index error_return_post_dominated
       then
         let {parser_node = parser_node} = node in
         cfg#replace_node (index, (mk_node true NoResource parser_node))
       else
         cfg)
    cfg cfg


let is_head cfg (index, node) =
  let predecessors = cfg#predecessors index in
  let {is_error_handling = is_error_handling} = node in
  is_error_handling &&
  predecessors#exists
    (fun (index, _) ->
       let node = cfg#nodes#assoc index in
       let {is_error_handling = is_error_handling} = node in
       is_selection node &&
       not (is_error_handling))


let filter_heads cfg nodes =
  NodeiSet.filter
    (fun index ->
       let node = cfg#nodes#assoc index in
       is_head cfg (index, node))
    nodes

(*TODO maybe optimise*)
let get_error_handling_branch_head cfg =
  let nodes = find_all (is_head cfg) cfg in
  List.fold_left
    (fun acc (index, _) -> NodeiSet.add index acc) NodeiSet.empty
    nodes


(*TODO implement*)
let annotate_resource_handling cfg =
  cfg


(*TODO improve to only one pass*)
let of_ast_c ast =
  let cocci_cfg =
    match Control_flow_c_build.ast_to_control_flow ast with
      Some cfg -> Control_flow_c_build.annotate_loop_nodes cfg
    | None     -> raise NoCFG
  in

  let process_node (g, added_nodes) (index, node) =

    let add_node (g, added_nodes) (index, node) =
      if not (Hashtbl.mem added_nodes index)
      then
        (* **
         * It uses add_nodei so that coccinelle can be used to debug with
         * --control-flow
         * *)
        let (g, index') = g#add_nodei index (mk_node false NoResource node) in
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
