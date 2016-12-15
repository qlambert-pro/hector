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

module ACFG_ESF = ACFG_Fixpoint (Asto.ExpressionSet)

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
  ACFG_Bool_Fixpoint.compute
    (ACFG_Bool_Fixpoint.get_forward_config
       (fun _ _ -> true)
       (fun _ (cn, _) res ->
          match cn.GO.node.ACFG.resource_handling_type with
            ACFG.Release r ->
            ({node = cn; resource = r}, block_head)::res
          | _         -> res)
       (fun _ _ -> true) true acc)
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
    let old_value =
      ACFG_ESF.NodeMap.find cn.GO.index values
    in
    Asto.ExpressionSet.union old_value value
  with Not_found -> value

let get_allocs cfg block_head release =
  let initial_value =
    match release.resource with
      ACFG.Resource e -> Asto.ExpressionSet.singleton e
    | _               -> Asto.ExpressionSet.empty
  in
  ACFG_ESF.compute
    (ACFG_ESF.get_backward_config
       (fun v (cn, e) ->
          maintain_value
            update_value_backward
            (ACFG_ESF.NodeMap.find e.ACFG.end_node) v (cn, e))

       (fun values (cn, _) allocs ->
          match cn.GO.node.ACFG.resource_handling_type with
            ACFG.Allocation r
            when Asto.ExpressionSet.exists
                (fun e -> ACFG.resource_equal r (ACFG.Resource e))
                (ACFG_ESF.NodeMap.find cn.GO.index values) ->
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
            not (Asto.ExpressionSet.is_empty
                 (ACFG_ESF.NodeMap.find cn.GO.index v)))
       initial_value [])
    cfg block_head


let get_exemplars cfg error_blocks =
  let get_resource_release acc block =
    get_resource_release cfg block acc
  in
  let releases = List.fold_left get_resource_release [] error_blocks in

  let exemplars_of_release acc (release, model_block) =
    let allocs = get_allocs cfg model_block release in

    List.fold_left
      (fun acc alloc ->
         if not (List.exists (fun a -> alloc.GO.index = a.alloc.GO.index) acc)
         then
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
           }::acc
         else acc)
      acc allocs
  in
  List.fold_left exemplars_of_release [] releases


let exists_after_block cfg block predicate =
  ACFG_Bool_Fixpoint.compute
    (ACFG_Bool_Fixpoint.get_forward_config
       (fun _ _ -> true)
       (fun _ (cn, _) res ->
          res || predicate cn)
       (fun _ _ -> true) true false)
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

let is_handling_allocation_failure cfg aliases assignment (cn, edge) =
  let identifier =
    match assignment with
      Some a -> Some a.ACFG.left_value
    | None   -> None
  in

  let start_node = ACFG.KeyMap.find edge.ACFG.start_node cfg#nodes in

  match (start_node.ACFG.resource_handling_type, identifier) with
    (ACFG.Test rs, Some id) when
      Asto.ExpressionSet.exists (Asto.expression_equal id) rs ->
    ACFG.is_on_error_branch HC.get_assignment_type_through_alias cfg
      {GO.index = edge.ACFG.start_node; GO.node = start_node} cn.GO.node

  | (ACFG.Test rs,       _) when
      not (Asto.ExpressionSet.is_empty
             (Asto.ExpressionSet.inter rs aliases)) ->
    ACFG.is_on_error_branch HC.get_assignment_type_through_alias cfg
      {GO.index = edge.ACFG.start_node; GO.node = start_node} cn.GO.node

  | (ACFG.Test        _, _)
  | (ACFG.Allocation  _, _)
  | (ACFG.Assignment  _, _)
  | (ACFG.Computation _, _)
  | (ACFG.Unannotated  , _)
  | (ACFG.Release     _, _) -> false


let killing_assignment assignment cn =
  let new_assignement = ACFG.get_assignment cn.GO.node in
  match (assignment, new_assignement) with
    (Some a1, Some a2) ->
    Asto.expression_equal a1.ACFG.left_value a2.ACFG.left_value
  | (Some  _, None   )
  | (None   , Some  _)
  | (None   , None   ) -> false

let has_higher_scope cfg e = Asto.is_global e

let is_assigning_to_higher_scope cfg aliases cn =
  match cn.GO.node.ACFG.resource_handling_type with
    ACFG.Assignment  a ->
    let is_relevant_assignment =
      match a.ACFG.right_value with
        Asto.Value    _ -> false
      | Asto.Variable e ->
        Asto.ExpressionSet.exists (Asto.expression_equal e) aliases
    in
    a.ACFG.operator = ACFG.Simple &&
    is_relevant_assignment &&
    has_higher_scope cfg a.ACFG.left_value
  | ACFG.Test        _
  | ACFG.Allocation  _
  | ACFG.Computation _
  | ACFG.Unannotated
  | ACFG.Release     _ -> false


let get_candidate_blocks cfg error_blocks exemplar =
  let assignment = ref (ACFG.get_assignment exemplar.alloc.GO.node) in
  let initial_value =
    match exemplar.alloc.GO.node.ACFG.resource_handling_type with
      ACFG.Allocation (ACFG.Resource e) -> Asto.ExpressionSet.singleton e
    | _                                 -> Asto.ExpressionSet.empty
  in

  ACFG_ESF.compute
    (ACFG_ESF.get_forward_config
       (fun v (cn, e) ->
          if killing_assignment !assignment cn then assignment := None;
          maintain_value update_value_forward
            (ACFG_ESF.NodeMap.find e.ACFG.start_node) v (cn, e))

       (fun values (cn, e) res ->
          try
            let aliases = ACFG_ESF.NodeMap.find cn.GO.index values in

            if List.exists (fun b -> b.GO.index = cn.GO.index) error_blocks &&
               not (e.ACFG.start_node == exemplar.alloc.GO.index) &&
               not (is_handling_allocation_failure cfg
                      aliases !assignment (cn, e)) &&
               not (is_releasing_resource cfg aliases cn) &&
               not (is_returning_resource cfg aliases cn)
            then cn::res
            else res
          with Not_found -> res)

       (fun v (cn, e) ->
          let aliases = ACFG_ESF.NodeMap.find cn.GO.index v in
          not (List.exists
                 (fun n -> n.GO.index = cn.GO.index)
                 error_blocks) &&
          not (Asto.ExpressionSet.is_empty
                 (ACFG_ESF.NodeMap.find e.ACFG.start_node v)) &&
          not (is_handling_allocation_failure cfg aliases !assignment
                 (cn, e)) &&
          not (is_assigning_to_higher_scope cfg aliases cn))

       initial_value [])
    cfg exemplar.alloc

let filter_faults cfg exemplar blocks =
  List.find_all
    (fun b ->
       ACFG_Bool_Fixpoint.compute
         (ACFG_Bool_Fixpoint.get_backward_config
            (fun _ _ -> true)
            (fun v (cn, edge) acc ->
               match cn.GO.node.ACFG.resource_handling_type with
               | ACFG.Release r ->
                 acc && not (ACFG.resource_equal exemplar.res r)
               | ACFG.Computation rs when
                   Asto.ExpressionSet.exists
                     (fun e ->
                        ACFG.resource_equal
                          exemplar.res (ACFG.Resource e))
                     rs ->
                 acc && not (ACFG.is_similar_statement exemplar.release cn)
               | ACFG.Assignment _
               | ACFG.Allocation _
               | ACFG.Test _
               | ACFG.Unannotated
               | ACFG.Computation _ -> acc)
            (fun _ (cn, _) ->
               not (Asto.ExpressionSet.exists
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
