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
module GO = Graph_operations

let get_assignment_type_through_alias cfg =
  let visited_node = ref GO.NodeiSet.empty in
  let rec aux n e =
    visited_node := GO.NodeiSet.add n.GO.index !visited_node;
    match Asto.get_assignment_type e with
      Asto.Variable e ->
      let assignements =
        GO.breadth_first_fold
          (GO.get_backward_config
             (fun _ (n, _) -> not (ACFG.is_killing_reach e n.GO.node))
             (fun _ (n, _) res ->
                if ACFG.is_top_node n.GO.node
                then
                  (Asto.Error Asto.Ambiguous)::res
                else
                  let temp = ref None in
                  ACFG.apply_side_effect_visitor
                    (fun l r ->
                       if Asto.expression_equal e l
                       then
                         match r with
                           Some r ->
                           temp :=
                             if GO.NodeiSet.mem n.GO.index !visited_node
                             then
                               Some (Asto.Error Asto.Ambiguous)
                             else
                               Some (aux n r)
                         | None   -> temp := Some (Asto.Error Asto.Ambiguous)
                       else
                         ())
                    n.GO.node;
                  match !temp with
                    None   -> res
                  | Some v -> v::res)
             [])
          cfg (GO.complete_node_of cfg n.GO.index)
      in
      if List.for_all ((=) (List.hd assignements)) assignements
      then
        List.hd assignements
      else
        Asto.Error Asto.Ambiguous
    | Asto.Value v    -> v
  in
  aux


let get_error_assignments cfg identifiers =
  let t = Hashtbl.create 101 in
  cfg#nodes#iter
    (fun (i, node) ->
       ACFG.apply_side_effect_visitor
         (fun l r ->
            if List.exists (fun e -> Asto.expression_equal e l) identifiers
            then
              match r with
                Some r ->
                let assignment_type =
                  get_assignment_type_through_alias
                    cfg {GO.index = i; GO.node = node} r
                in
                (match assignment_type with
                   Asto.Error err -> Hashtbl.add t ((i, node), l) err
                 | _    -> ())
              | None ->  Hashtbl.add t ((i, node), l) Asto.Ambiguous
            else
              ())
         node);
  t


let add_branch_nodes_leading_to_return cfg returns nodes acc =
  let post_dominated index =
    let is_backedge e =
      match e with
        ACFG.Direct       _ -> false
      | ACFG.PostBackedge _ -> true
    in
    let predicate set (n, e) =
      let is_post_dominated =
        GO.fold_successors
          (fun acc (cn, e) ->
             acc &&
             (is_backedge e || GO.NodeiSet.mem cn.GO.index set))
          true cfg n.GO.index
      in
      GO.NodeiSet.mem n.GO.index nodes &&
      is_post_dominated
    in
    let config = GO.get_backward_basic_node_config predicate in
    GO.breadth_first_fold config cfg (GO.complete_node_of cfg index)
  in

  GO.NodeiSet.fold
    (fun return acc ->
       let branch_nodes = post_dominated return in
       GO.NodeiSet.union branch_nodes acc)
    returns acc


let add_post_dominated cfg index acc =
  let post_dominated =
    GO.conditional_get_post_dominated
      (fun _ -> true)
      cfg (GO.complete_node_of cfg index)
  in
  GO.NodeiSet.union post_dominated acc


let get_nodes_leading_to_error_return cfg error_assignments =
  let get_reachable_nodes ((index, node), identifier) error_type acc =
    match error_type with
      Asto.Clear ->
      (Common.profile_code "clear" (fun () ->
           let nodes =
             GO.breadth_first_fold
               (GO.get_basic_node_config
                  (fun _ (cn, _) ->
                     not (ACFG.is_killing_reach identifier cn.GO.node)))
               cfg (GO.complete_node_of cfg index)
           in
           let reachable_returns = ACFG.filter_returns cfg identifier nodes in
           if reachable_returns != GO.NodeiSet.empty
           then
             let error_branch_nodes =
               add_branch_nodes_leading_to_return
                 cfg reachable_returns nodes acc
             in
             if GO.NodeiSet.mem index error_branch_nodes
             then
               add_post_dominated cfg index error_branch_nodes
             else
               error_branch_nodes
           else
             acc))
    | Asto.Ambiguous ->
      (Common.profile_code "ambiguous" (fun () ->
           let heads =
             GO.breadth_first_fold
               (GO.get_forward_config
                  (fun _ (cn, _) ->
                     not (ACFG.is_killing_reach identifier cn.GO.node))
                  (*TODO is_testing_identifier is incorrect*)
                  (* add complex if cases to prove it *)
                  (fun _ (cn, e) res ->
                     match e with
                       ACFG.PostBackedge (pred, _)
                     | ACFG.Direct (pred, _) ->
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
                       then GO.NodeiSet.add cn.GO.index res
                       else res)
                  GO.NodeiSet.empty)
               cfg (GO.complete_node_of cfg index)
           in
           let nodes =
             GO.NodeiSet.fold
               (fun index s ->
                  GO.NodeiSet.union s
                    (GO.breadth_first_fold
                       (GO.get_basic_node_config
                          (fun _ (cn, _) ->
                             not (ACFG.is_killing_reach identifier cn.GO.node)))
                       cfg (GO.complete_node_of cfg index)))
               heads GO.NodeiSet.empty
           in
           let reachable_returns = ACFG.filter_returns cfg identifier nodes in
           if reachable_returns != GO.NodeiSet.empty
           then
             add_branch_nodes_leading_to_return cfg reachable_returns nodes acc
           else
             acc
         ))
  in
  Hashtbl.fold get_reachable_nodes error_assignments GO.NodeiSet.empty


let is_head cfg (index, node) =
  let predecessors = cfg#predecessors index in
  node.ACFG.is_error_handling &&
  predecessors#exists
    (fun (index, _) ->
       let node = cfg#nodes#assoc index in
       ACFG.is_selection node &&
       not (node.ACFG.is_error_handling))


(*TODO optimise number of pass*)
(*TODO replace as computed rather than store*)
let annotate_error_handling cfg =
  let error_returns, identifiers =

    (Common.profile_code "get_returns" (fun () ->
         GO.fold_node
           (fun (error_returns, id_set) (i, node) ->
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
           ([], []) cfg))
  in
  let error_assignments =
    (Common.profile_code "get_assignement" (fun () ->
         get_error_assignments cfg identifiers)) in
  let error_branch_nodes =
    (Common.profile_code "get_nodes" (fun () ->
         get_nodes_leading_to_error_return cfg error_assignments
       ))
  in
  let error_return_post_dominated =
    (Common.profile_code "get_postdominated" (fun () ->
         List.fold_left
           (fun acc (index, _) ->
              let nodes =
                GO.conditional_get_post_dominated (fun _ -> true) cfg
                  (GO.complete_node_of cfg index)
              in
             GO.NodeiSet.union acc nodes)
           GO.NodeiSet.empty error_returns
       ))
  in
  (Common.profile_code "annotate_error_handling" (fun () ->
       GO.fold_node
         (fun () (index, node) ->
            if GO.NodeiSet.mem index error_branch_nodes ||
               GO.NodeiSet.mem index error_return_post_dominated
            then
              cfg#replace_node
                (index, {node with ACFG.is_error_handling = true})
            else
              ())
         () cfg
     ))


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

    if (ACFG.is_last_reference_before_killed_reached r
          cfg cn (ACFG.Resource r) &&
        not (ACFG.is_return_value_tested cfg cn)) ||
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
    (fun l _ ->
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
      ACFG.is_last_reference cfg cn (ACFG.Resource r) ->
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
      begin
      match ACFG.get_assignment cn.GO.node with
        Some a when
          List.exists
            (Asto.expression_equal a.ACFG.left_value)
            relevant_resources ->
        ACFG.Assignment a
      | _ -> ACFG.Unannotated
      end


let annotate_resource_handling cfg =
  let relevant_resources =
    (Common.profile_code "find_resource" (fun () ->
         GO.fold_node
           (fun acc (i, n) ->
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
           [] cfg
       ))
  in

  GO.fold_node
    (fun () (i, n) ->
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
    () cfg;

  let annotate_if_allocation r cfg cn =
    if ACFG.is_first_reference cfg cn (ACFG.Resource r) &&
      not (ACFG.is_assigning_variable cn)
    then
      ACFG.annotate_resource cfg cn (ACFG.Allocation (ACFG.Resource r))
    else
      ()
  in

  let config r =
    GO.get_forward_config
      (fun _ (cn, _) ->
         not (ACFG.is_referencing_resource (ACFG.Resource r) cn.GO.node))
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
      ()
  in
  List.iter
    (fun r -> GO.breadth_first_fold (config r) cfg (ACFG.get_top_node cfg))
    relevant_resources


(*TODO maybe optimise*)
let get_error_handling_branch_head cfg =
  let nodes = GO.find_all (is_head cfg) cfg in
  List.fold_left
    (fun acc (i, n) -> {GO.node = n; GO.index = i}::acc) [] nodes
