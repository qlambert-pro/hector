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
module NodeMap  = Map.Make(OrderedNodei)

let add_successors   cfg acc n =
  fold_successors   (fun acc cn -> cn::acc) acc cfg n.index

let add_predecessors cfg acc n =
  fold_predecessors (fun acc cn -> cn::acc) acc cfg n.index

type ('node, 'edge, 'g, 'value, 'res) fold_configuration =
  {get_next_nodes:
     ('node, 'edge, 'g) readable_graph -> ('node complete_node * 'edge) list ->
     'node complete_node -> ('node complete_node * 'edge) list;

   update_value_for_fixed_point:
     'value NodeMap.t -> ('node complete_node * 'edge) -> 'value;

   compute_result:
     'value NodeMap.t -> ('node complete_node * 'edge) -> 'res -> 'res;

   predicate: 'value NodeMap.t -> ('node complete_node * 'edge) -> bool;

   initial_value:  'value;
   initial_result: 'res;
  }


type ('value, 'a, 'node, 'edge) breadth_first_fold_data =
  {visited_nodes:    'value NodeMap.t;
   last_added_nodes: 'node complete_node list;
   result:           'a}


let breadth_first_fold config g cn =
  let rec breadth_first_fold_aux
      {visited_nodes    = visited_nodes;
       last_added_nodes = last_added_nodes;
       result           = result} =

    let next_nodes =
      List.fold_left (config.get_next_nodes g) [] last_added_nodes
    in

    let new_visited_nodes =
      List.fold_left
        (fun visited_nodes (s, e) ->
           let new_value =
             config.update_value_for_fixed_point visited_nodes (s, e)
           in
           NodeMap.add s.index new_value visited_nodes)
        visited_nodes next_nodes
    in

    let new_result =
      List.fold_left
        (fun result cn -> config.compute_result new_visited_nodes cn result)
        result next_nodes
    in

    let add_to_visited_nodes added_nodes (s, e) =
      let old_value =
        try Some (NodeMap.find s.index visited_nodes)
        with Not_found -> None
      in
      let new_value = NodeMap.find s.index new_visited_nodes in
      let should_visit = config.predicate new_visited_nodes (s, e) in

      match (should_visit, old_value, new_value) with
        ( true,    None,  v) ->
        s::added_nodes
      | ( true, Some ov, nv) when ov <> nv ->
        s::added_nodes
      | _ ->
        added_nodes
    in

    let added_nodes =
      List.fold_left add_to_visited_nodes [] next_nodes
    in

    if NodeMap.equal (=) new_visited_nodes visited_nodes
    then new_result
    else breadth_first_fold_aux
        {visited_nodes    = new_visited_nodes;
         last_added_nodes = added_nodes;
         result           = new_result}
  in
  let initial_set = NodeMap.singleton cn.index config.initial_value in
  breadth_first_fold_aux
    {visited_nodes    = initial_set;
     last_added_nodes = [cn];
     result           = config.initial_result}


let get_forward_config update_value compute_result predicate initial_value
    initial_result =
  {get_next_nodes                 = add_successors;
   update_value_for_fixed_point   = update_value;
   predicate                      = predicate;
   compute_result                 = compute_result;
   initial_value                  = initial_value;
   initial_result                 = initial_result;}


let get_backward_config update_value compute_result predicate initial_value
   initial_result =
  {get_next_nodes                 = add_predecessors;
   update_value_for_fixed_point   = update_value;
   predicate                      = predicate;
   compute_result                 = compute_result;
   initial_value                  = initial_value;
   initial_result                 = initial_result;}


let get_basic_node_config predicate initial_result =
  get_forward_config predicate
    (fun visited_nodes (s, _) res ->
       if NodeMap.mem  s.index visited_nodes &&
          NodeMap.find s.index visited_nodes
       then
         NodeiSet.add s.index res
       else
         res)
    predicate true initial_result

let get_backward_basic_node_config predicate initial_result =
  get_backward_config predicate
    (fun visited_nodes (s, _) res ->
       if NodeMap.mem  s.index visited_nodes &&
          NodeMap.find s.index visited_nodes
       then
         NodeiSet.add s.index res
       else
         res)
    predicate true initial_result

let conditional_get_post_dominated p g cn =
  let predicate set (n, e) =
    p (n, e) &&
    fold_successors
      (fun acc (cn, _) -> acc &&
                          (NodeMap.mem  cn.index set &&
                           NodeMap.find cn.index set))
      true g n.index
  in
  let config =
    get_backward_basic_node_config predicate (NodeiSet.singleton cn.index)
  in
  breadth_first_fold config g cn
