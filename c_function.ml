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

(*TODO fixed point*)
let get_allocs cfg block_head release =
  let visited_node = ref GO.NodeiSet.empty in
  let rec aux b resource resources =
    visited_node := GO.NodeiSet.add b.GO.index !visited_node;
    GO.breadth_first_fold
      (GO.get_backward_config
         (fun _ _ -> true)
         (fun _ (cn, _) allocs ->
            match cn.GO.node.ACFG.resource_handling_type with
              ACFG.Allocation r
              when ACFG.resource_equal r resource ->
              cn::allocs
            | ACFG.Assignment a when
                not (ACFG.resource_equal
                       (ACFG.Resource a.ACFG.left_value) resource) ->
              let nr = (ACFG.Resource a.ACFG.left_value) in

              if GO.NodeiSet.mem cn.GO.index !visited_node
              then allocs
              else
                let nallocs = aux cn nr (nr::resources) in
                List.fold_left
                  (fun acc cn ->
                     if List.exists ((=) cn) acc
                     then acc
                     else cn::acc)
                  allocs nallocs
            | ACFG.Computation _
            | ACFG.Allocation _
            | ACFG.Assignment _
            | ACFG.Release _
            | ACFG.Test _
            | ACFG.Unannotated -> allocs)
         (fun _ (cn, _) ->
            match cn.GO.node.ACFG.resource_handling_type with
              ACFG.Allocation r ->
              not (ACFG.resource_equal r resource)
            | ACFG.Assignment a ->
              not (ACFG.resource_equal
                     (ACFG.Resource a.ACFG.left_value) resource)
            | ACFG.Computation _
            | ACFG.Release _
            | ACFG.Test _
            | ACFG.Unannotated -> true)
         true [])
      cfg b
  in
  aux block_head release.resource [release.resource]


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
    List.exists
      (fun resource ->
        exists_after_block cfg b
          (fun b ->
             match b.GO.node.ACFG.resource_handling_type with
               ACFG.Release r when ACFG.resource_equal r resource -> true
             | ACFG.Computation _
             | ACFG.Allocation _
             | ACFG.Release _
             | ACFG.Assignment _
             | ACFG.Test _
             | ACFG.Unannotated -> false))
      resources


let is_returning_resource cfg resources b =
    List.exists (fun resource ->
        exists_after_block cfg b (ACFG.is_returning_resource resource))
      resources

let update_aliases cn aliases =
  match cn.GO.node.ACFG.resource_handling_type with
    ACFG.Assignment a ->
    let resource_assignment = ACFG.Resource a.ACFG.left_value in
    (match a.ACFG.right_value with
       Asto.Variable v ->
       let resource_variable    = ACFG.Resource v in
       if List.exists (ACFG.resource_equal resource_variable) aliases &&
          not (List.exists
                 (ACFG.resource_equal resource_assignment)
                 aliases)
       then resource_assignment::aliases
       else aliases
     | Asto.Value _ ->
       List.find_all
         (fun r -> not (ACFG.resource_equal resource_assignment r))
         aliases)
  | ACFG.Computation _
  | ACFG.Allocation _
  | ACFG.Release _
  | ACFG.Test _
  | ACFG.Unannotated -> aliases


(*TODO fixed point*)
let get_candidate_blocks cfg error_blocks exemplar =
  fst
    (GO.breadth_first_fold
       (GO.get_forward_config
          (fun _ _ -> true)
          (fun _ (cn, _) (res, aliases) ->
             let naliases = update_aliases cn aliases in
             try
               let block =
                 List.find (fun b -> b.GO.index = cn.GO.index) error_blocks
               in
               if not (is_releasing_resource cfg naliases block) &&
                  not (is_returning_resource cfg naliases block)
               then
                 (block::res, naliases)
               else
                 (       res, naliases)
             with Not_found ->
               (res, naliases))
          (fun _ (cn, _) ->
             not (List.exists
                    (fun n -> n.GO.index = cn.GO.index)
                    error_blocks))
           true ([], [exemplar.res]))
       cfg exemplar.alloc)


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
