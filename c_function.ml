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

module GO = Graph_operations
module ACFG = Annotated_cfg
module Asto = Ast_operations
module HC = Hector_core

type exemplar = {
  alloc: ACFG.node GO.complete_node;
  alloc_name: string;
  release: ACFG.node GO.complete_node;
  release_name: string;
  res: ACFG.resource;
}

type resource_usage = {
  node: ACFG.node GO.complete_node;
  resource: ACFG.resource;
}
type fault = {
  exemplar: exemplar;
  block_head: ACFG.node GO.complete_node;
}

let find_errorhandling cfg = HC.get_error_handling_branch_head cfg

let get_resource_release cfg block_head acc =
  GO.breadth_first_fold
    (GO.get_forward_config
       (fun _ _ -> true)
       (fun _ (cn, _) res ->
          match cn.GO.node.Annotated_cfg.resource_handling_type with
            ACFG.Release r ->
            ({node = cn; resource = r}, block_head)::res
          | _         -> res)
       (fun _ _ -> true)
       true acc)
    cfg block_head


let update_value_backward value cn =
  match cn.GO.node.ACFG.resource_handling_type with
    ACFG.Assignment a ->
    let is_relevant = Asto.ExpressionSet.mem a.ACFG.left_value value in
    (match (is_relevant, a.ACFG.operator, a.ACFG.right_value) with
       (true, ACFG.Simple, Asto.Variable e) ->
       let value' = Asto.ExpressionSet.add e value in
       Asto.ExpressionSet.remove a.ACFG.left_value value'
     | _ -> value)
  | ACFG.Computation _
  | ACFG.Allocation _
  | ACFG.Release _
  | ACFG.Test _
  | ACFG.Unannotated -> value


let update_value_forward value cn =
  match cn.GO.node.ACFG.resource_handling_type with
    ACFG.Assignment a ->
    (match (a.ACFG.operator, a.ACFG.right_value) with
       (ACFG.Simple, Asto.Variable e) ->
       if Asto.ExpressionSet.mem e value
       then Asto.ExpressionSet.add    a.ACFG.left_value value
       else Asto.ExpressionSet.remove a.ACFG.left_value value
     | _ -> value)
  | ACFG.Computation _
  | ACFG.Allocation _
  | ACFG.Release _
  | ACFG.Test _
  | ACFG.Unannotated -> value


let maintain_value update_value get_previous_value values (cn, e) =
  let value' = get_previous_value values in
  let value  = update_value value' cn in
  try
    let old_value = GO.NodeMap.find cn.GO.index values in
    Asto.ExpressionSet.union old_value value
  with Not_found -> value

let get_allocs cfg block_head release =
  let initial_value =
    match release.resource with
      ACFG.Resource e -> Asto.ExpressionSet.singleton e
    | _               -> Asto.ExpressionSet.empty
  in
  GO.breadth_first_fold
    (GO.get_backward_config
       (fun v (cn, e) ->
          maintain_value
            update_value_backward
            (GO.NodeMap.find e.ACFG.end_node.GO.index) v (cn, e))


       (fun values (cn, _) allocs ->
          match cn.GO.node.ACFG.resource_handling_type with
            ACFG.Allocation r
            when Asto.ExpressionSet.exists
                (fun e -> ACFG.resource_equal r (ACFG.Resource e))
                (GO.NodeMap.find cn.GO.index values) ->
            if List.exists (fun n -> (=) cn.GO.index n.GO.index) allocs
            then allocs
            else cn::allocs
          | ACFG.Computation _
          | ACFG.Allocation _
          | ACFG.Assignment _
          | ACFG.Release _
          | ACFG.Test _
          | ACFG.Unannotated -> allocs)

         (fun v (cn, _) ->
            not (Asto.ExpressionSet.is_empty (GO.NodeMap.find cn.GO.index v)))
       initial_value [])
    cfg block_head


let get_exemplars cfg error_blocks =
  let get_resource_release acc block =
    get_resource_release cfg block acc
  in
  let releases = List.fold_left get_resource_release [] error_blocks in

  let exemplars_of_release acc (release, model_block) =
    (*TODO may not need model_block*)
    let allocs = get_allocs cfg model_block release in

    List.fold_left (fun acc alloc ->
        let a = ACFG.get_function_call_name alloc in
        let r = ACFG.get_function_call_name release.node in
        let (an, rn) =
          match (a, r) with
            (Some a, Some r) -> (a, r)
          | _ -> failwith "missing function call from alloc or release"
        in
        {alloc        = alloc;
         alloc_name   = an;
         release      = release.node;
         release_name = rn;
         res          = release.resource;
        }::acc) acc allocs
  in
  List.fold_left exemplars_of_release [] releases


let exists_after_block cfg block predicate =
  GO.breadth_first_fold
    (GO.get_forward_config
       (fun _ _ -> true)
       (fun _ (cn, _) res ->
          res || predicate cn)
       (fun _ _ -> true)
       true false)
    cfg block


let is_releasing_resource cfg resources b =
  Asto.ExpressionSet.exists
    (fun resource ->
       exists_after_block cfg b
         (fun b ->
            match b.GO.node.ACFG.resource_handling_type with
              ACFG.Release r
              when ACFG.resource_equal r (ACFG.Resource resource) -> true
            | ACFG.Computation _
            | ACFG.Allocation _
            | ACFG.Release _
            | ACFG.Assignment _
            | ACFG.Test _
            | ACFG.Unannotated -> false))
    resources


let is_returning_resource cfg resources b =
  Asto.ExpressionSet.exists (fun resource ->
      exists_after_block cfg b
        (ACFG.is_returning_resource (ACFG.Resource resource)))
    resources

let get_candidate_blocks cfg error_blocks exemplar =
  let initial_value =
    match exemplar.res with
      ACFG.Resource e -> Asto.ExpressionSet.singleton e
    | _               -> Asto.ExpressionSet.empty
  in
  GO.breadth_first_fold
    (GO.get_forward_config
       (fun v (cn, e) ->
          maintain_value update_value_forward
            (GO.NodeMap.find e.ACFG.start_node.GO.index) v (cn, e))

       (fun values (cn, _) res ->
          try
            let block =
              List.find (fun b -> b.GO.index = cn.GO.index) error_blocks
            in
            let aliases = GO.NodeMap.find cn.GO.index values in

            if not (is_releasing_resource cfg aliases block) &&
               not (is_returning_resource cfg aliases block)
            then block::res
            else res
          with Not_found -> res)

       (fun v (cn, e) ->
          not (List.exists
                 (fun n -> n.GO.index = cn.GO.index)
                 error_blocks) &&
          not (Asto.ExpressionSet.is_empty
                 (GO.NodeMap.find e.ACFG.start_node.GO.index v)))

       initial_value [])
    cfg exemplar.alloc


let filter_faults cfg exemplar blocks =
  List.find_all
    (fun b ->
       GO.breadth_first_fold
         (GO.get_backward_config
            (fun _ _ -> true)
            (fun _ (cn, edge) acc ->
               match cn.GO.node.ACFG.resource_handling_type with
                 ACFG.Allocation r ->
                 acc && not (ACFG.is_similar_statement cn exemplar.alloc)
               | ACFG.Release r ->
                 acc && not (ACFG.resource_equal exemplar.res r)
               | ACFG.Test rs when
                   List.exists
                     (fun e ->
                        ACFG.resource_equal
                          exemplar.res (ACFG.Resource e))
                     rs ->
                 let end_node = edge.ACFG.end_node in
                 acc &&
                 not (ACFG.is_on_error_branch
                        HC.get_assignment_type_through_alias
                        cfg cn end_node.GO.node)
               | ACFG.Computation rs when
                   List.exists
                     (fun e ->
                        ACFG.resource_equal
                          exemplar.res (ACFG.Resource e))
                     rs ->
                 acc && not (ACFG.is_similar_statement exemplar.release cn)
               | ACFG.Assignment _
               | ACFG.Test _
               | ACFG.Unannotated
               | ACFG.Computation _ -> acc)
            (fun _ (cn, _) ->
               not (List.exists
                      (fun e ->
                         ACFG.resource_equal exemplar.res (ACFG.Resource e))
                      cn.GO.node.ACFG.referenced_resources))
            true true)
         cfg b)
    blocks


let get_faults cfg error_blocks exemplar =
  let blocks' = get_candidate_blocks cfg error_blocks exemplar in
  let blocks = filter_faults cfg exemplar blocks' in

  List.map (fun b -> {exemplar = exemplar; block_head = b}) blocks
