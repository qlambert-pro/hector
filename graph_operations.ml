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

type 'node complete_node =
  {index: nodei;
   node:  'node;
  }

let complete_node_of g index =
  {index = index;
   node  = g#nodes#assoc index;
  }

let fold_node f acc g =
  (g#nodes: (nodei, 'node) Oassoc.oassoc)#fold f acc

let fold_predecessors f acc g node =
  ((g#predecessors node): (nodei * 'edge) Oset.oset)#fold
    (fun a (i, e) -> f a (complete_node_of g i, e))
    acc

let fold_successors f acc g node =
  ((g#successors node): (nodei * 'edge) Oset.oset)#fold
    (fun a (i, e) -> f a (complete_node_of g i, e))
    acc

let find_all f g =
  (g#nodes#filter f)#tolist

module OrderedNodei =
struct
  type t = nodei
  let compare = compare
end

module NodeiSet = Set.Make(OrderedNodei)

let add_successors   cfg acc n =
  fold_successors   (fun acc cn -> cn::acc) acc cfg n.index

let add_predecessors cfg acc n =
  fold_predecessors (fun acc cn -> cn::acc) acc cfg n.index

type ('node, 'edge, 'g, 'acc, 'res) fold_configuration =
  {get_next_nodes:
     ('node, 'edge, 'g) readable_graph -> ('node complete_node * 'edge) list ->
     'node complete_node -> ('node complete_node * 'edge) list;

   predicate: 'acc -> NodeiSet.t -> ('node complete_node * 'edge) -> bool;

   compute_local_value:
     NodeiSet.t -> ('node complete_node * 'edge) -> 'acc -> 'acc;

   compute_result: 'acc -> ('node complete_node * 'edge) -> 'res -> 'res;

   initial_local_value: 'acc;
   initial_result: 'res;
  }


type ('a, 'b, 'node, 'edge) breadth_first_fold_data =
  {visited_nodes:    NodeiSet.t;
   last_added_nodes: 'node complete_node list;
   local_value:      'b;
   result:           'a}

let breadth_first_fold config g cn =
  let rec breadth_first_fold_aux
      {visited_nodes    = visited_nodes;
       last_added_nodes = last_added_nodes;
       result           = result} =

    (*TODO maybe don't add nodes that are already visited*)
    let next_nodes =
      List.fold_left (config.get_next_nodes g) [] last_added_nodes
    in

    let add_to_visited_nodes (visited_nodes, added_nodes) (s, e) =
      if config.predicate config.initial_local_value visited_nodes (s, e) &&
         not (NodeiSet.mem s.index visited_nodes)
      then
        (NodeiSet.add s.index visited_nodes, s::added_nodes)
      else
        (visited_nodes, added_nodes)
    in

    let new_visited_nodes, added_nodes =
      List.fold_left add_to_visited_nodes (visited_nodes, []) next_nodes
    in

    let (new_result, _) =
      List.fold_left
        (fun (result, local_value) cn ->
           let new_local_value =
             config.compute_local_value new_visited_nodes cn local_value
           in
           (config.compute_result new_local_value cn result, new_local_value))
        (result, config.initial_local_value) next_nodes
    in

    if new_visited_nodes = visited_nodes
    then new_result
    else breadth_first_fold_aux
        {visited_nodes    = new_visited_nodes;
         last_added_nodes = added_nodes;
         local_value      = config.initial_local_value;
         result           = new_result}
  in
  let initial_set = NodeiSet.singleton cn.index in
  breadth_first_fold_aux
    {visited_nodes    = initial_set;
     last_added_nodes = [cn];
     local_value      = config.initial_local_value;
     result           = config.initial_result}

type ('a, 'b, 'node, 'edge) depth_first_fold_data =
  {visited_nodes: NodeiSet.t;
   current_node:  ('node complete_node * 'edge);
   local_value:   'b;
   result:        'a;}

let depth_first_fold config g cn =
  let rec depth_first_fold_aux
      {visited_nodes = visited_nodes;
       current_node  = (s, e);
       local_value   = local_value;
       result        = result} =
    let new_local_value =
      config.compute_local_value visited_nodes (s, e) local_value
    in
    let new_result =
      config.compute_result new_local_value (s, e) result
    in

    let new_visited_nodes = NodeiSet.add s.index visited_nodes in
    if (config.predicate new_local_value visited_nodes (s, e)) &&
       new_visited_nodes <> visited_nodes
    then
      let next_nodes = config.get_next_nodes g [] s in

      List.fold_left
        (fun result cn ->
           depth_first_fold_aux
             {visited_nodes = new_visited_nodes;
              current_node  = cn;
              local_value   = new_local_value;
              result        = result})
        new_result next_nodes
    else
      new_result
  in
  let initial_set = NodeiSet.singleton cn.index in
  let next_nodes = config.get_next_nodes g [] cn in
  List.fold_left
    (fun result cn ->
       depth_first_fold_aux
         {visited_nodes = initial_set;
          current_node  = cn;
          local_value   = config.initial_local_value;
          result        = result})
    config.initial_result next_nodes


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
  let predicate _ set (n, e) =
    p (n, e) &&
    fold_successors
      (fun acc (cn, _) -> acc && (NodeiSet.mem cn.index set)) true g n.index
  in
  let config = get_backward_basic_node_config predicate in
  breadth_first_fold config g
