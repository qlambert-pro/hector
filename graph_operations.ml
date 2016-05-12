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

open Ograph_extended

type ('node, 'edge, 'g) readable_graph =
  < nodes: (nodei, 'node) Oassoc.oassoc;

    successors:   nodei -> (nodei * 'edge) Oset.oset;
    predecessors: nodei -> (nodei * 'edge) Oset.oset;
    ..
  > as 'g

let tuple_of g index =
  (index, g#nodes#assoc index)

let fold_node f acc g =
  (g#nodes: (nodei, 'node) Oassoc.oassoc)#fold f acc

let fold_predecessors f acc g node =
  ((g#predecessors node): (nodei * 'edge) Oset.oset)#fold
    (fun a (i, _) -> f a (tuple_of g i))
    acc

let fold_successors f acc g node =
  ((g#successors node): (nodei * 'edge) Oset.oset)#fold
    (fun a (i, _) -> f a (tuple_of g i))
    acc

let find_all f g =
  (g#nodes#filter f)#tolist

module OrderedNodei =
struct
  type t = nodei
  let compare = compare
end

module NodeiSet = Set.Make(OrderedNodei)

let add_successors   cfg index acc =
  fold_successors   (fun acc (s, _) -> NodeiSet.add s acc) acc cfg index

let add_predecessors cfg index acc =
  fold_predecessors (fun acc (s, _) -> NodeiSet.add s acc) acc cfg index

type ('node, 'edge, 'g, 'acc, 'res) fold_configuration =
  {get_next_nodes:
     ('node, 'edge, 'g) readable_graph -> nodei -> NodeiSet.t -> NodeiSet.t;

   predicate: NodeiSet.t -> nodei * 'node -> bool;
   (* **
    * The predicate is called with visited_nodes as first argument and
    * the algorithm only fold on the node if predicate returns true
    * *)

   compute_local_value: NodeiSet.t -> nodei -> 'acc -> 'acc;
   (* **
    * This function compute the new local value from visited_nodes,
    * the current node and the current local value
    * *)

   compute_result: 'acc -> nodei -> 'res -> 'res;
   (* **
    * This function compute the new result from visited_nodes,
    * the current node and the current result value
    * *)

   initial_local_value: 'acc;
   initial_result: 'res;
  }

type ('a, 'b) breadth_first_fold_data =
  {visited_nodes:    NodeiSet.t;
   last_added_nodes: NodeiSet.t;
   local_value:      'b;
   result:           'a}

let breadth_first_fold config g index =
  let {get_next_nodes      = get_next_nodes;
       predicate           = p;
       compute_result      = compute_result;
       compute_local_value = compute_local_value;
       initial_result      = initial_result;
       initial_local_value = initial_local_value;
      } = config
  in
  let nodes = g#nodes in
  let rec breadth_first_fold_aux
      {visited_nodes    = visited_nodes;
       last_added_nodes = last_added_nodes;
       result           = result} =

    (*TODO maybe don't add nodes that are already visited*)
    let next_nodes =
      NodeiSet.fold (get_next_nodes g) last_added_nodes NodeiSet.empty
    in

    let add_to_visited_nodes s visited_nodes =
      let node = nodes#assoc s in
      if p visited_nodes (s, node)
      then NodeiSet.add s visited_nodes
      else visited_nodes
    in

    let new_visited_nodes =
      NodeiSet.fold add_to_visited_nodes next_nodes visited_nodes
    in

    let added_nodes = NodeiSet.inter next_nodes new_visited_nodes in

    let (new_result, _) =
      NodeiSet.fold
        (fun index (result, local_value) ->
           let new_local_value =
             compute_local_value new_visited_nodes index local_value
           in
           (compute_result new_local_value index result, new_local_value))
        next_nodes (result, initial_local_value)
    in

    if new_visited_nodes = visited_nodes
    then new_result
    else breadth_first_fold_aux
        {visited_nodes    = new_visited_nodes;
         last_added_nodes = added_nodes;
         local_value      = initial_local_value;
         result           = new_result}
  in
  let initial_set = NodeiSet.singleton index in
  breadth_first_fold_aux
    {visited_nodes    = initial_set;
     last_added_nodes = initial_set;
     local_value      = initial_local_value;
     result           = initial_result}

type ('a, 'b) depth_first_fold_data =
  {visited_nodes: NodeiSet.t;
   current_node:  nodei;
   local_value:   'b;
   result:        'a;}

let depth_first_fold config g index =
  let {get_next_nodes      = get_next_nodes;
       predicate           = p;
       compute_result      = compute_result;
       compute_local_value = compute_local_value;
       initial_result      = initial_result;
       initial_local_value = initial_local_value;
      } = config
  in
  let nodes = g#nodes in
  let rec depth_first_fold_aux
      {visited_nodes = visited_nodes;
       current_node  = s;
       local_value   = local_value;
       result        = result} =
    let node = nodes#assoc s in

    let new_local_value =
      compute_local_value visited_nodes s local_value
    in
    let new_result =
      compute_result new_local_value s result
    in

    let new_visited_nodes = NodeiSet.add s visited_nodes in
    if (p visited_nodes (s, node)) &&
       new_visited_nodes <> visited_nodes
    then
      let next_nodes = get_next_nodes g s NodeiSet.empty in

      NodeiSet.fold
        (fun index result ->
           depth_first_fold_aux
             {visited_nodes = new_visited_nodes;
              current_node  = index;
              local_value   = new_local_value;
              result        = result})
        next_nodes new_result
    else
      new_result
  in
  let initial_set = NodeiSet.singleton index in
  let next_nodes = get_next_nodes g index NodeiSet.empty in
  NodeiSet.fold
    (fun index result ->
       depth_first_fold_aux
         {visited_nodes = initial_set;
          current_node  = index;
          local_value   = initial_local_value;
          result        = result})
    next_nodes initial_result


let get_forward_config predicate compute_local_value compute_result
    initial_local_value initial_result =
  {get_next_nodes      = add_successors;
   predicate           = predicate;
   compute_result      = compute_result;
   compute_local_value = compute_local_value;
   initial_result      = initial_result;
   initial_local_value = initial_local_value;}


let get_backward_config predicate compute_local_value compute_result
    initial_local_value initial_result =
  {get_next_nodes      = add_predecessors;
   predicate           = predicate;
   compute_result      = compute_result;
   compute_local_value = compute_local_value;
   initial_result      = initial_result;
   initial_local_value = initial_local_value;}

let get_basic_node_config predicate =
  get_forward_config predicate (fun v _ _ -> v) (fun v _ _ -> v)
    NodeiSet.empty NodeiSet.empty

let get_backward_basic_node_config predicate =
  get_backward_config predicate (fun v _ _ -> v) (fun v _ _ -> v)
    NodeiSet.empty NodeiSet.empty

let conditional_get_post_dominated p g =
  let predicate set (index, node) =
    p (index, node) &&
    fold_successors
      (fun acc (s, _) -> acc && (NodeiSet.mem s set)) true g index
  in
  let config = get_backward_basic_node_config predicate in
  breadth_first_fold config g
