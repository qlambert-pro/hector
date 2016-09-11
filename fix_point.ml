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

module GO = Graph_operations

module type EqualType =
sig
  type t
  val equal: t -> t -> bool
end

module type S =
sig
  type key = GO.key
  type edge
  type edges
  type 'node keymap

  type ('node, 'g) readable_graph =
    < nodes: 'node keymap;
      successors:   key -> edges;
      predecessors: key -> edges;
      ..
    > as 'g


  val fold_node:
    ('node, 'g) readable_graph ->
    (key -> 'node -> 'a -> 'a) ->
    'a ->  'a

  val complete_node_of:
    ('node, 'g) readable_graph -> key -> 'node GO.complete_node

  val fold_predecessors:
    (('node GO.complete_node * edge) -> 'a -> 'a) ->
    ('node, 'g) readable_graph -> key -> 'a -> 'a

  val fold_successors:
    (('node GO.complete_node * edge) -> 'a -> 'a) ->
    ('node, 'g) readable_graph -> key -> 'a -> 'a

  val find_all:
    (key -> 'node -> bool) -> ('node, 'g) readable_graph -> 'node keymap
end

module Make (Val: EqualType) (Ops : S) =
struct
  type key = GO.key
  type value = Val.t
  type edge = Ops.edge
  type edges = Ops.edges
  type 'node keymap = 'node Ops.keymap

  type ('node, 'g) readable_graph =
    < nodes: 'node keymap;
      successors:   key -> edges;
      predecessors: key -> edges;
      ..
    > as 'g


  module NodeMap =
    Map.Make(struct type t = Ops.key let compare = compare end)

  let add_successors   cfg acc n =
    Ops.fold_successors   (fun cn acc -> cn::acc) cfg n.GO.index acc

  let add_predecessors cfg acc n =
    Ops.fold_predecessors (fun cn acc -> cn::acc) cfg n.GO.index acc

  type ('node, 'g, 'res) configuration =
    {get_next_nodes:
       ('node, 'g) Ops.readable_graph ->
       ('node GO.complete_node * Ops.edge) list ->
       'node GO.complete_node -> ('node GO.complete_node * Ops.edge) list;

     update_value:
       value NodeMap.t -> ('node GO.complete_node * Ops.edge) -> value;

     compute_result:
       value NodeMap.t -> ('node GO.complete_node * Ops.edge) ->
       'res -> 'res;

     predicate: value NodeMap.t -> ('node GO.complete_node * Ops.edge) ->
       bool;

     initial_value:  value;
     initial_result: 'res;
    }

  type ('a, 'node, 'edge) breadth_first_fold_data =
    {visited_nodes:    value NodeMap.t;
     last_added_nodes: 'node GO.complete_node list;
     result:           'a}


  let compute config g cn =
    let rec aux data =
      let next_nodes =
        List.fold_left (config.get_next_nodes g) [] data.last_added_nodes
      in

      let new_visited_nodes =
        List.fold_left
          (fun visited_nodes (s, e) ->
             let new_value =
               config.update_value visited_nodes (s, e)
             in
             NodeMap.add s.GO.index new_value visited_nodes)
          data.visited_nodes next_nodes
      in

      let new_result =
        List.fold_left
          (fun result cn -> config.compute_result new_visited_nodes cn result)
          data.result next_nodes
      in

      let add_no_double cn l =
        if List.exists (fun n -> (=) cn.GO.index n.GO.index) l
        then l
        else cn::l
      in

      let add_to_visited_nodes added_nodes (s, e) =
        let old_value =
          try Some (NodeMap.find s.GO.index data.visited_nodes)
          with Not_found -> None
        in
        let new_value = NodeMap.find s.GO.index new_visited_nodes in
        let should_visit = config.predicate new_visited_nodes (s, e) in

        match (should_visit, old_value, new_value) with
          ( true,    None,  v) ->
          add_no_double s added_nodes
        | ( true, Some ov, nv) when not (Val.equal ov nv) ->
          add_no_double s added_nodes
        | _ ->
          added_nodes
      in

      let added_nodes =
        List.fold_left add_to_visited_nodes [] next_nodes
      in

      if NodeMap.equal (fun v1 v2 -> Val.equal v1 v2)
          new_visited_nodes data.visited_nodes
      then new_result
      else aux
          {visited_nodes    = new_visited_nodes;
           last_added_nodes = added_nodes;
           result           = new_result}
    in
    let initial_set =
      NodeMap.singleton cn.GO.index config.initial_value
    in
    aux
      {visited_nodes    = initial_set;
       last_added_nodes = [cn];
       result           = config.initial_result}


  let get_forward_config update_value compute_result predicate
      initial_value initial_result =
    {get_next_nodes = add_successors;
     update_value   = update_value;
     compute_result = compute_result;
     predicate      = predicate;
     initial_value  = initial_value;
     initial_result = initial_result;}


  let get_backward_config update_value compute_result predicate
      initial_value initial_result =
    {get_next_nodes = add_predecessors;
     update_value   = update_value;
     compute_result = compute_result;
     predicate      = predicate;
     initial_value  = initial_value;
     initial_result = initial_result;}
end
