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
module KFP = Key_fix_point

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

module ACFGOps = GO.Make (ACFG)
module ACFG_Fixpoint (Val: EqualType) = Fix_point.Make (Val) (ACFGOps)
module ACFG_Bool_Fixpoint = ACFG_Fixpoint (BoolValue)
module ACFG_KeyFixPoint = KFP.Make (ACFG.KeySet) (ACFGOps) (ACFG_Bool_Fixpoint)

module ACFG_ExpressionSet_Fixpoint = ACFG_Fixpoint (Asto.ExpressionSet)

let local_releases = ref Asto.StringSet.empty
let set_local_releases s = local_releases := s

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
    ACFG_ExpressionSet_Fixpoint.compute
      (ACFG_ExpressionSet_Fixpoint.get_backward_config
         (fun values (cn, e) ->
            let value' =
              ACFG_ExpressionSet_Fixpoint.NodeMap.find e.ACFG.end_node values
            in
            let value  = update_value value' cn in
            try
              let old_value = ACFG_ExpressionSet_Fixpoint.NodeMap.find
                  cn.GO.index values
              in
              Asto.ExpressionSet.union old_value value
            with Not_found -> value)
         (fun v (n, e) res ->
            if ACFG.is_top_node n.GO.node
            then
              (Asto.Error Asto.Ambiguous)::res
            else
              Asto.ExpressionSet.fold (add_error_type n)
                (ACFG_ExpressionSet_Fixpoint.NodeMap.find e.ACFG.end_node v) res)
         (fun v (cn, _) ->
            not (Asto.ExpressionSet.is_empty
                   (ACFG_ExpressionSet_Fixpoint.NodeMap.find cn.GO.index v)))
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


let add_branch_nodes_leading_to_return cfg return nodes =
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
              (ACFG_Bool_Fixpoint.NodeMap.mem  cn.GO.index set &&
               ACFG_Bool_Fixpoint.NodeMap.find cn.GO.index set)))
          cfg n.GO.index true
      in
      ACFG.KeySet.mem n.GO.index nodes &&
      is_post_dominated
    in
    let config =
      ACFG_KeyFixPoint.get_basic_backward_config predicate
        (ACFG.KeySet.singleton index)
    in
    ACFG_Bool_Fixpoint.compute config cfg (ACFGOps.complete_node_of cfg index)
  in

  post_dominated return


let add_post_dominated cfg index acc =
  let post_dominated =
    ACFG_KeyFixPoint.conditional_get_post_dominated
      (fun _ -> true)
      cfg (ACFGOps.complete_node_of cfg index)
  in
  ACFG.KeySet.union post_dominated acc


let get_subgraph_nodes cfg error_assignments subgraphs =
  let get_reachable_nodes ((index, node), identifier) error_type acc =
    let kill_reach _ (cn, _) =
      not (ACFG.is_killing_reach identifier cn.GO.node)
    in
    let nodes =
      match error_type with
        Asto.Clear ->
        let nodes =
          ACFG_Bool_Fixpoint.compute
            (ACFG_KeyFixPoint.get_basic_forward_config
               kill_reach
               (ACFG.KeySet.singleton index))
            cfg (ACFGOps.complete_node_of cfg index)
        in
        add_post_dominated cfg index nodes

      | Asto.Ambiguous ->
        let heads =
          ACFG_Bool_Fixpoint.compute
            (ACFG_Bool_Fixpoint.get_forward_config
               (fun _ _ -> true)

               (*TODO is_testing_identifier is incorrect*)
               (* add complex if cases to prove it *)
               (fun _ (cn, e) res ->
                  let pred = ACFGOps.complete_node_of cfg e.ACFG.start_node in
                  let head = cn.GO.node in
                  let is_correct_branch =
                    ACFG.is_on_error_branch
                      get_assignment_type_through_alias cfg pred head
                  in
                  let is_testing_identifier =
                    ACFG.test_if_header
                      (Asto.is_testing_identifier identifier)
                      false pred.GO.node
                  in
                  if is_correct_branch && is_testing_identifier
                  then ACFG.KeySet.add cn.GO.index res
                  else res)

               kill_reach true ACFG.KeySet.empty)
            cfg (ACFGOps.complete_node_of cfg index)
        in
        ACFG.KeySet.fold
          (fun index s ->
             ACFG.KeySet.union s
               (ACFG_Bool_Fixpoint.compute
                  (ACFG_KeyFixPoint.get_basic_forward_config
                     kill_reach (ACFG.KeySet.singleton index))
                  cfg (ACFGOps.complete_node_of cfg index)))
          heads ACFG.KeySet.empty
    in
    let reachable_returns = ACFG.filter_returns cfg identifier nodes in
    ACFG.KeySet.fold
      (fun k a ->
         let new_nodes' =
           if ACFG.KeyMap.mem k a
           then ACFG.KeyMap.find k a
           else ACFG.KeySet.empty
         in
         let new_nodes = ACFG.KeySet.union new_nodes' nodes in
         ACFG.KeyMap.add k new_nodes a)
      reachable_returns
      acc
  in
  Hashtbl.fold get_reachable_nodes error_assignments subgraphs


let is_head cfg index node =
  let predecessors = cfg#predecessors index in
  node.ACFG.is_error_handling &&
  ACFG.KeyEdgeSet.exists
    (fun (index, _) ->
       let node = ACFG.KeyMap.find index cfg#nodes in
       ACFG.is_selection node &&
       not (node.ACFG.is_error_handling))
    predecessors

let annotate_error_handling cfg =
  let all_nodes =
    ACFGOps.fold_node cfg (fun k _ acc -> ACFG.KeySet.add k acc)
      ACFG.KeySet.empty
  in

  let error_returns, identifiers =
    ACFGOps.fold_node cfg
      (fun i node (error_returns, id_set) ->
         let error_type =
           ACFG.test_returned_expression Asto.get_assignment_type
             (Asto.Value Asto.NonError) node
         in
         match error_type with
           Asto.Value (Asto.Error Asto.Clear) ->
           (ACFG.KeySet.add i error_returns, id_set)
         | Asto.Variable e ->
           (error_returns, e::id_set)
         | _ -> (error_returns, id_set))
      (ACFG.KeySet.empty, [])
  in
  let subgraphs' =
    ACFG.KeySet.fold (fun k a -> ACFG.KeyMap.add k all_nodes a) error_returns
      ACFG.KeyMap.empty
  in
  let error_assignments = get_error_assignments cfg identifiers in
  let subgraphs = get_subgraph_nodes cfg error_assignments subgraphs' in

  ACFG.KeyMap.iter
    (fun r n ->
       let nodes = add_branch_nodes_leading_to_return cfg r n in
       ACFG.KeySet.iter
         (fun index ->
            let node = ACFG.KeyMap.find index cfg#nodes in
            cfg#replace_node (index,
                              {node with ACFG.is_error_handling = true}))
         nodes)
    subgraphs

let is_interprocedural c =
  let name = ACFG.get_function_call_name c in
  match name with
    Some n -> Asto.StringSet.mem n !local_releases
  | None   -> false


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
       then assigned_variable := Some (Asto.unify_array_access l)
       else ())
    cn.GO.node;

  match (!assigned_variable, resources, arguments) with
  | (  None,   _, Some   []) ->
    ACFG.Allocation (ACFG.Void None)
  | (  None,  [], Some  [r]) when not (Asto.is_string r) ->
    ACFG.Allocation (ACFG.Void (Some r))
  | (Some r,  _, Some args) when
      Asto.ExpressionSet.exists (Asto.expression_equal r) relevant_resources &&
      not (should_ignore args) ->
    ACFG.Allocation (ACFG.Resource r)
  | (     _, [r], Some args) when
      Asto.ExpressionSet.exists (Asto.expression_equal r) relevant_resources &&
      ACFGO.is_last_reference cfg cn (ACFG.Resource r) ->
    ACFG.Release (ACFG.Resource r)
  | (     _,  rs, Some args) when
      List.exists
        (fun e -> Asto.ExpressionSet.exists (Asto.expression_equal e)
            relevant_resources)
        rs ->
    let current_resources =
      List.fold_left
        (fun acc e ->
           if Asto.ExpressionSet.exists (Asto.expression_equal e)
               relevant_resources
           then Asto.ExpressionSet.add e acc
           else acc)
        Asto.ExpressionSet.empty rs
    in
    ACFG.Computation current_resources
  | _ ->
    let test = ACFG.test_if_header
        (fun e ->
           Asto.ExpressionSet.fold
             (fun id acc ->
                if Asto.is_testing_identifier id e
                then Asto.ExpressionSet.add id acc
                else acc)
             relevant_resources Asto.ExpressionSet.empty)
        Asto.ExpressionSet.empty cn.GO.node
    in
    if not (Asto.ExpressionSet.is_empty test)
    then ACFG.Test test
    else
      match ACFG.get_assignment cn.GO.node with
        Some a ->
        let is_left =
          Asto.ExpressionSet.exists
            (Asto.expression_equal a.ACFG.left_value)
            relevant_resources
        in
        let is_right =
          match a.ACFG.right_value with
            Asto.Variable v when
              Asto.ExpressionSet.exists
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


let get_relevant_resources cfg =
  let relevant_resources =
    ACFGOps.fold_node cfg
      (fun i n acc ->
         if n.ACFG.is_error_handling
         then
           let resource =
             annotate_if_release cfg {GO.index = i; GO.node = n}
           in
           match resource with
             None   -> acc
           | Some r -> Asto.ExpressionSet.add r acc
         else
           acc)
      Asto.ExpressionSet.empty
  in

  ACFGOps.fold_node cfg
    (fun _ n acc ->
       let assignement = ACFG.get_assignment n in
       match assignement with
         Some a ->
         if Asto.ExpressionSet.exists
             (Asto.expression_equal a.ACFG.left_value) acc
         then
           match a.ACFG.right_value with
             Asto.Variable e ->
             if not (Asto.ExpressionSet.exists (Asto.expression_equal e) acc)
             then Asto.ExpressionSet.add e acc
             else acc
           | _ -> acc
         else
           acc
       | _ -> acc)
    relevant_resources

let annotate_resource cfg relevant_resources =
  ACFGOps.fold_node cfg
    (fun i n () ->
       let cn = {GO.index = i; GO.node = n} in
       let rs =
         Asto.ExpressionSet.filter
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
    ACFG_Bool_Fixpoint.get_forward_config
      (fun _ _ -> true)
      (fun _ (cn, _) res ->
         match cn.GO.node.ACFG.resource_handling_type with
         | ACFG.Computation cr when
             Asto.ExpressionSet.cardinal cr == 1 &&
             Asto.expression_equal r (Asto.ExpressionSet.choose cr) ->
           annotate_if_allocation (Asto.ExpressionSet.choose cr) cfg cn
         | ACFG.Assignment _
         | ACFG.Computation _
         | ACFG.Test _
         | ACFG.Allocation _
         | ACFG.Release _
         | ACFG.Unannotated -> ())
      (fun _ (cn, _) ->
         not (ACFG.is_referencing_resource (ACFG.Resource r) cn.GO.node))
      true ()
  in
  Asto.ExpressionSet.iter
    (fun r ->
       ACFG_Bool_Fixpoint.compute (config r) cfg (ACFGO.get_top_node cfg))
    relevant_resources

let annotate_resource_handling cfg =

  let relevant_resources = get_relevant_resources cfg in
  annotate_resource cfg relevant_resources;

  let allocation_return_variable =
    ACFGOps.fold_node cfg
      (fun k n acc ->
         match n.ACFG.resource_handling_type with
         | ACFG.Allocation (ACFG.Resource e) ->
           let assignement = ACFG.get_assignment n in
           (match assignement with
              Some a ->
              if not (Asto.expression_equal e a.ACFG.left_value)
              then Asto.ExpressionSet.add a.ACFG.left_value acc
              else acc
            | _ -> acc)
         | ACFG.Allocation (ACFG.Void _) ->
           let assignement = ACFG.get_assignment n in
           (match assignement with
              Some a -> Asto.ExpressionSet.add a.ACFG.left_value acc
            | _     -> acc)
         | ACFG.Assignment _
         | ACFG.Computation _
         | ACFG.Test _
         | ACFG.Release _
         | ACFG.Unannotated -> acc)
      Asto.ExpressionSet.empty
  in

  ACFGOps.fold_node cfg
    (fun i n () ->
       let tested_identifiers =
         Asto.ExpressionSet.filter
           (fun e -> ACFG.test_if_header (Asto.is_testing_identifier e) false n)
           allocation_return_variable
       in
       if not (Asto.ExpressionSet.is_empty tested_identifiers)
       then
         ACFG.annotate_resource cfg {GO.index = i; GO.node = n}
           (ACFG.Test tested_identifiers)
       else ())
    ()

(*TODO maybe optimise*)
let get_error_handling_branch_head cfg =
  let nodes = ACFGOps.find_all (is_head cfg) cfg in
  ACFG.KeyMap.fold
    (fun i n acc -> {GO.node = n; GO.index = i}::acc) nodes []
