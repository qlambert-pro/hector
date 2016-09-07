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

module Asto = Ast_operations
module ACFG = Annotated_cfg
module ACFGO = Acfg_operations
module GO = Graph_operations

module ACFGOps = GO.Make (ACFG)

let update_value value cn =
  let side_effects = ref [] in
  ACFG.apply_side_effect_visitor
    (fun l op r ->
       let right_value =
         match (op, r) with
           (Some op, Some r) when Asto.is_simple_assignment op ->
           Some (Asto.get_assignment_type r)
         | (Some op, Some r) ->
           Some (Asto.Value (Asto.NonError))
         | (      _,   None) ->
           None
         | _ -> failwith "unexpected missing operator or right value"
       in
       side_effects := (l, right_value)::!side_effects)
    cn.GO.node;
  List.fold_left
    (fun acc (l, r) ->
       if Asto.ExpressionSet.exists (Asto.expression_equal l) acc
       then
         match r with
           Some (Asto.Variable e) when not (Asto.expression_equal e l) ->
           let nvalue = Asto.ExpressionSet.add e acc in
           Asto.ExpressionSet.remove l nvalue
         | Some (Asto.Variable e) -> acc
         | Some (Asto.Value    _)
         | None -> Asto.ExpressionSet.remove l acc
       else acc)
    value !side_effects

let add_error_type n e acc =
  let temp = ref None in
  ACFG.apply_side_effect_visitor
    (fun l op r ->
       if Asto.expression_equal e l
       then
         match (op, r) with
           (Some op,      _) when
             not (Asto.is_simple_assignment op) ->
           temp := Some Asto.NonError
         | (Some  _, Some r) ->
           (match Asto.get_assignment_type r with
              Asto.Value v    -> temp := Some v
            | Asto.Variable _ -> ())
         | (      _,   None) ->
           temp := Some (Asto.Error Asto.Ambiguous)
         | _ -> failwith "unexpected missing operator or right value"
       else
         ())
    n.GO.node;
  match !temp with
    None   -> acc
  | Some v -> v::acc

let get_assignment_type_through_alias cfg cn id =
  let initial_value = update_value (Asto.ExpressionSet.singleton id) cn in
  let initial_result = add_error_type cn id [] in
  let assignments =
    ACFGOps.breadth_first_fold
      (ACFGOps.get_backward_config
         (fun values (cn, e) ->
            let value' =
              ACFGOps.NodeMap.find e.ACFG.end_node.GO.index values
            in
            let value  = update_value value' cn in
            try
              let old_value = ACFGOps.NodeMap.find cn.GO.index values in
              Asto.ExpressionSet.union old_value value
            with Not_found -> value)
         (fun v (n, e) res ->
            if ACFG.is_top_node n.GO.node
            then
              (Asto.Error Asto.Ambiguous)::res
            else
              Asto.ExpressionSet.fold (add_error_type n)
                (ACFGOps.NodeMap.find e.ACFG.end_node.GO.index v) res)
         (Asto.ExpressionSet.equal)
         (fun v (cn, _) ->
            not (Asto.ExpressionSet.is_empty (ACFGOps.NodeMap.find cn.GO.index v)))
         initial_value initial_result)
      cfg cn
  in

  if List.for_all ((=) (List.hd assignments)) assignments
  then
    List.hd assignments
  else
    Asto.Error Asto.Ambiguous

let get_error_assignments cfg identifiers =
  let t = Hashtbl.create 101 in
  ACFG.KeyMap.iter
    (fun i node ->
       ACFG.apply_side_effect_visitor
         (fun l op r ->
            if List.exists (fun e -> Asto.expression_equal e l) identifiers
            then
              let assignment_type =
                get_assignment_type_through_alias
                  cfg {GO.index = i; GO.node = node} l
              in
              (match assignment_type with
                 Asto.Error err -> Hashtbl.add t ((i, node), l) err
               | _    -> ())
            else
              ())
         node)
    cfg#nodes;
  t


let add_branch_nodes_leading_to_return cfg returns nodes acc =
  let post_dominated index =
    let is_backedge e =
      match e.ACFG.edge_type with
        ACFG.Direct       -> false
      | ACFG.PostBackedge -> true
    in
    let predicate set (n, e) =
      let is_post_dominated =
        ACFGOps.fold_successors
          (fun (cn, e) acc ->
             acc &&
             (is_backedge e ||
              (ACFGOps.NodeMap.mem  cn.GO.index set &&
               ACFGOps.NodeMap.find cn.GO.index set)))
          cfg n.GO.index true
      in
      ACFG.KeySet.mem n.GO.index nodes &&
      is_post_dominated
    in
    let config =
      ACFGOps.get_backward_basic_node_config predicate
        (ACFG.KeySet.singleton index)
    in
    ACFGOps.breadth_first_fold config cfg (ACFGOps.complete_node_of cfg index)
  in

  ACFG.KeySet.fold
    (fun return acc ->
       let branch_nodes = post_dominated return in
       ACFG.KeySet.union branch_nodes acc)
    returns acc


let add_post_dominated cfg index acc =
  let post_dominated =
    ACFGOps.conditional_get_post_dominated
      (fun _ -> true)
      cfg (ACFGOps.complete_node_of cfg index)
  in
  ACFG.KeySet.union post_dominated acc


let get_nodes_leading_to_error_return cfg error_assignments =
  let get_reachable_nodes ((index, node), identifier) error_type acc =
    let kill_reach _ (cn, _) =
      not (ACFG.is_killing_reach identifier cn.GO.node)
    in
    match error_type with
      Asto.Clear ->
      let nodes =
        ACFGOps.breadth_first_fold
          (ACFGOps.get_basic_node_config
             kill_reach
             (ACFG.KeySet.singleton index))
          cfg (ACFGOps.complete_node_of cfg index)
      in
      let reachable_returns = ACFG.filter_returns cfg identifier nodes in
      if reachable_returns != ACFG.KeySet.empty
      then
        let error_branch_nodes =
          add_branch_nodes_leading_to_return
            cfg reachable_returns nodes acc
        in
        if ACFG.KeySet.mem index error_branch_nodes
        then
          add_post_dominated cfg index error_branch_nodes
        else
          error_branch_nodes
      else
        acc
    | Asto.Ambiguous ->
      let heads =
        ACFGOps.breadth_first_fold
          (ACFGOps.get_forward_config
             (fun _ _ -> true)

             (*TODO is_testing_identifier is incorrect*)
             (* add complex if cases to prove it *)
             (fun _ (cn, e) res ->
                let pred = e.ACFG.start_node in
                let head = cn.GO.node in
                let is_correct_branch =
                  ACFG.is_on_error_branch
                    get_assignment_type_through_alias
                    cfg pred head
                in
                let is_testing_identifier =
                  ACFG.test_if_header
                    (Asto.is_testing_identifier identifier)
                    false pred.GO.node
                in
                if is_correct_branch && is_testing_identifier
                then ACFG.KeySet.add cn.GO.index res
                else res)

             (=) kill_reach true ACFG.KeySet.empty)
          cfg (ACFGOps.complete_node_of cfg index)
      in
      let nodes =
        ACFG.KeySet.fold
          (fun index s ->
             ACFG.KeySet.union s
               (ACFGOps.breadth_first_fold
                  (ACFGOps.get_basic_node_config
                     kill_reach (ACFG.KeySet.singleton index))
                  cfg (ACFGOps.complete_node_of cfg index)))
          heads ACFG.KeySet.empty
      in
      let reachable_returns = ACFG.filter_returns cfg identifier nodes in
      if reachable_returns != ACFG.KeySet.empty
      then
        add_branch_nodes_leading_to_return cfg reachable_returns nodes acc
      else
        acc
  in
  Hashtbl.fold get_reachable_nodes error_assignments ACFG.KeySet.empty


let is_head cfg index node =
  let predecessors = cfg#predecessors index in
  node.ACFG.is_error_handling &&
  ACFG.KeyEdgeSet.exists
    (fun (index, _) ->
       let node = ACFG.KeyMap.find index cfg#nodes in
       ACFG.is_selection node &&
       not (node.ACFG.is_error_handling))
    predecessors


(*TODO optimise number of pass*)
(*TODO replace as computed rather than store*)
let annotate_error_handling cfg =
  let error_returns, identifiers =

    ACFGOps.fold_node cfg
      (fun i node (error_returns, id_set) ->
         let error_type =
           ACFG.test_returned_expression Asto.get_assignment_type
             (Asto.Value Asto.NonError) node
         in
         match error_type with
           Asto.Value (Asto.Error Asto.Clear) ->
           ((i,node)::error_returns, id_set)
         | Asto.Variable e ->
           (error_returns, e::id_set)
         | _ -> (error_returns, id_set))
      ([], [])
  in
  let error_assignments = get_error_assignments cfg identifiers in
  let error_branch_nodes =
    get_nodes_leading_to_error_return cfg error_assignments
  in
  let error_return_post_dominated =
    List.fold_left
      (fun acc (index, _) ->
         let nodes =
           ACFGOps.conditional_get_post_dominated (fun _ -> true) cfg
             (ACFGOps.complete_node_of cfg index)
         in
         ACFG.KeySet.union acc nodes)
      ACFG.KeySet.empty error_returns
  in
  ACFGOps.fold_node cfg
    (fun index node () ->
       if ACFG.KeySet.mem index error_branch_nodes ||
          ACFG.KeySet.mem index error_return_post_dominated
       then
         cfg#replace_node
           (index, {node with ACFG.is_error_handling = true})
       else
         ())
    ()

(*TODO implement*)
let is_interprocedural c = false


let get_released_resource cfg cn =
  let arguments = ACFG.get_arguments cn.GO.node in
  let resources = Asto.resources_of_arguments arguments in
  let should_ignore r = List.exists Asto.is_string r in
  match (resources, arguments) with
    (  _, Some   []) -> Some (ACFG.Void None)
  | ( [], Some  [r]) when not (Asto.is_string   r) -> Some (ACFG.Void (Some r))
  | ([r], Some args) when not (should_ignore args) ->

    if (ACFGO.is_last_reference_before_killed_reached r
          cfg cn (ACFG.Resource r) &&
        not (ACFGO.is_return_value_tested cfg cn)) ||
       is_interprocedural cn
    then
      Some (ACFG.Resource r)
    else
      None
  | _           -> None


let annotate_if_release cfg cn =
  let released_resource = get_released_resource cfg cn in
  match released_resource with
    None   -> None
  | Some r ->
    let resource =
      match r with
        ACFG.Void _ -> None
      | ACFG.Resource r -> Some r
    in
    ACFG.annotate_resource cfg cn (ACFG.Release r);
    resource


let get_resource cfg relevant_resources cn =
  let arguments = ACFG.get_arguments cn.GO.node in
  let resources = Asto.resources_of_arguments arguments in
  let should_ignore arguments = List.exists Asto.is_string arguments in
  let assigned_variable = ref None in
  ACFG.apply_assignment_visitor
    (fun l _ _ ->
       if Asto.is_pointer l
       then assigned_variable := Some l
       else ())
    cn.GO.node;

  match (!assigned_variable, resources, arguments) with
  | (  None,   _, Some   []) ->
    ACFG.Allocation (ACFG.Void None)
  | (  None,  [], Some  [r]) when not (Asto.is_string r) ->
    ACFG.Allocation (ACFG.Void (Some r))
  | (Some r,  _, Some args) when
      List.exists (Asto.expression_equal r) relevant_resources &&
      not (should_ignore args) ->
    ACFG.Allocation (ACFG.Resource r)
  | (     _, [r], Some args) when
      List.exists (Asto.expression_equal r) relevant_resources &&
      ACFGO.is_last_reference cfg cn (ACFG.Resource r) ->
    ACFG.Release (ACFG.Resource r)
  | (     _,  rs, Some args) when
      List.exists
        (fun e -> List.exists (Asto.expression_equal e)
            relevant_resources)
        rs ->
    let current_resources =
      List.filter
        (fun e -> List.exists (Asto.expression_equal e)
            relevant_resources)
        rs
    in
    ACFG.Computation current_resources
  | _ ->
    let test = ACFG.test_if_header
        (fun e ->
           List.find_all
             (fun id -> Asto.is_testing_identifier id e) relevant_resources)
        [] cn.GO.node
    in
    if test <> []
    then ACFG.Test test
    else
      match ACFG.get_assignment cn.GO.node with
        Some a ->
        let is_left =
          List.exists
            (Asto.expression_equal a.ACFG.left_value)
            relevant_resources
        in
        let is_right =
          match a.ACFG.right_value with
            Asto.Variable v when
              List.exists
                (Asto.expression_equal v)
                relevant_resources -> true
          | Asto.Value _
          | Asto.Variable _ -> false
        in
        if is_left || is_right
        then
          ACFG.Assignment a
        else
          ACFG.Unannotated
      | _ -> ACFG.Unannotated


let annotate_resource_handling cfg =
  let relevant_resources' =
    ACFGOps.fold_node cfg
      (fun i n acc ->
         if n.ACFG.is_error_handling
         then
           let resource =
             annotate_if_release cfg {GO.index = i; GO.node = n}
           in
           match resource with
             None   -> acc
           | Some r -> r::acc
         else
           acc)
      []
  in

  let relevant_resources =
    ACFGOps.fold_node cfg
      (fun _ n acc ->
         let assignement = ACFG.get_assignment n in
         match assignement with
           Some a ->
           if List.exists (Asto.expression_equal a.ACFG.left_value) acc
           then
             match a.ACFG.right_value with
               Asto.Variable e ->
               if not (List.exists (Asto.expression_equal e) acc)
               then e::acc
               else acc
             | _ -> acc
           else
             acc
         | _ -> acc)
      relevant_resources'
  in

  ACFGOps.fold_node cfg
    (fun i n () ->
       let cn = {GO.index = i; GO.node = n} in
       let rs =
         List.find_all
           (fun e -> ACFG.is_referencing_resource (ACFG.Resource e) cn.GO.node)
           relevant_resources
       in
       let nnode = {cn.GO.node with ACFG.referenced_resources = rs} in
       cfg#replace_node (cn.GO.index, nnode);
       ACFG.annotate_resource cfg {cn with GO.node = nnode}
         (get_resource cfg relevant_resources cn))
    ();

  let annotate_if_allocation r cfg cn =
    if ACFGO.is_first_reference cfg cn (ACFG.Resource r) &&
       not (ACFG.is_assigning_variable cn)
    then
      ACFG.annotate_resource cfg cn (ACFG.Allocation (ACFG.Resource r))
    else
      ()
  in

  let config r =
    ACFGOps.get_forward_config
      (fun _ _ -> true)
      (fun _ (cn, _) res ->
         match cn.GO.node.ACFG.resource_handling_type with
         | ACFG.Computation [cr] when Asto.expression_equal r cr ->
           annotate_if_allocation cr cfg cn
         | ACFG.Assignment _
         | ACFG.Computation _
         | ACFG.Test _
         | ACFG.Allocation _
         | ACFG.Release _
         | ACFG.Unannotated -> ())
      (=)
      (fun _ (cn, _) ->
         not (ACFG.is_referencing_resource (ACFG.Resource r) cn.GO.node))
      true ()
  in
  List.iter
    (fun r ->
       ACFGOps.breadth_first_fold (config r) cfg (ACFGO.get_top_node cfg))
    relevant_resources


(*TODO maybe optimise*)
let get_error_handling_branch_head cfg =
  let nodes = ACFGOps.find_all (is_head cfg) cfg in
  ACFG.KeyMap.fold
    (fun i n acc -> {GO.node = n; GO.index = i}::acc) nodes []
