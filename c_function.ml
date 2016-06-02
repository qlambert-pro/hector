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
module R = Resource
module ACFG = Annotated_cfg

type fault = {
  exemplar: R.exemplar;
  block_head: ACFG.node GO.complete_node;
}

let find_errorhandling cfg = Annotated_cfg.get_error_handling_branch_head cfg

(*TODO study trough aliases*)
let get_previous_statements cfg block_head release =
  GO.depth_first_fold
    (GO.get_backward_config
       (fun _ (cn, _) ->
          match cn.GO.node.ACFG.resource_handling_type with
            ACFG.Allocation r
            when ACFG.resource_equal r release.R.resource ->
            false
          | ACFG.Allocation _
          | ACFG.Release _
          | ACFG.Computation _
          | ACFG.Unannotated -> true)

       (fun _ (cn, _) acc ->
          match cn.GO.node.ACFG.resource_handling_type with
            ACFG.Computation rs
            when List.exists
                (fun r ->
                   ACFG.resource_equal
                     release.R.resource (ACFG.Resource r))
                rs ->
            cn::acc
          | ACFG.Allocation _
          | ACFG.Release _
          | ACFG.Computation _
          | ACFG.Unannotated -> acc)

       (fun acc (cn, _) res ->
          match cn.GO.node.ACFG.resource_handling_type with
            ACFG.Allocation r
            when ACFG.resource_equal r release.R.resource ->
            (cn, acc)::res
          | ACFG.Allocation _
          | ACFG.Release _
          | ACFG.Computation _
          | ACFG.Unannotated -> res)
       [] [])
    cfg block_head


let get_exemplars cfg error_blocks =

  let get_resource_release acc block =
    Block.get_resource_release cfg block acc
  in

  let releases = List.fold_left get_resource_release [] error_blocks in

  let exemplars_of_release acc (release, model_block) =
    (*TODO may not need model_block*)
    let allocs = get_previous_statements cfg model_block release in

    List.fold_left (fun acc (alloc, computations) ->
        let a = ACFG.get_function_call_name alloc in
        let r = ACFG.get_function_call_name release.R.node in
        let (an, rn) =
          match (a, r) with
            (Some a, Some r) -> (a, r)
          | _ -> failwith "missing function call from alloc or release"
        in
        {R.alloc        = alloc;
         R.alloc_name   = an;
         R.computations = computations;
         R.release      = release.R.node;
         R.release_name = rn;
         R.res          = release.R.resource;
        }::acc) acc allocs
  in
  List.fold_left exemplars_of_release [] releases

(*TODO should be only release instead of computations*)
let get_faults cfg error_blocks exemplar =
  GO.depth_first_fold
    (GO.get_forward_config
       (fun _ (cn, _) ->
          List.exists
            (fun n -> n.GO.index = cn.GO.index)
            error_blocks)

       (fun _ (cn, _) (acc, computations) ->
          match (cn.GO.node.ACFG.resource_handling_type, computations) with
            (ACFG.Computation rs, e::t)
            when List.exists
                (fun r ->
                   ACFG.resource_equal
                     exemplar.R.res (ACFG.Resource r))
                rs ->
            if ACFG.is_similar_statement cn e
            then (acc, t)
            else (false, [])
          | (ACFG.Allocation  _, _)
          | (ACFG.Release     _, _)
          | (ACFG.Computation _, _)
          | (ACFG.Unannotated  , _) -> (acc, computations))

       (fun (acc, computations) (cn, _) res ->
          try
            let block =
              List.find (fun b -> b.GO.index = cn.GO.index) error_blocks
            in
            (*TODO
             * does not release the resource
             * does not return  the resource
             * *)
            if acc && computations = []
            then
              {exemplar = exemplar; block_head = block}::res
            else
              res
          with Not_found -> res)
       (true, exemplar.R.computations) [])
    cfg exemplar.R.alloc
