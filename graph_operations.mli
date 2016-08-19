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

module NodeiSet: Set.S with type elt = nodei
module NodeMap : Map.S with type key = nodei

type ('node, 'edge, 'g) readable_graph =
  < nodes: (nodei, 'node) Oassoc.oassoc;

    successors:   nodei -> (nodei * 'edge) Oset.oset;
    predecessors: nodei -> (nodei * 'edge) Oset.oset;
    ..
  > as 'g

val fold_node:
  ('a -> (nodei * 'node) -> 'a) ->
  'a -> ('node, 'edge, 'g) readable_graph -> 'a

type 'node complete_node =
  {index: nodei;
   node:  'node;
  }

val complete_node_of:
  ('node, 'edge, 'g) readable_graph ->
  nodei -> 'node complete_node

val fold_predecessors:
  ('a -> ('node complete_node * 'edge) -> 'a) ->
  'a -> ('node, 'edge, 'g) readable_graph -> nodei -> 'a

val fold_successors:
  ('a -> ('node complete_node * 'edge) -> 'a) ->
  'a -> ('node, 'edge, 'g) readable_graph -> nodei -> 'a

val find_all:
  (nodei * 'node -> bool) -> ('node, 'edge, 'g) readable_graph ->
  (nodei * 'node) list

type ('node, 'edge, 'g, 'value, 'res) fold_configuration =
  {get_next_nodes:
     ('node, 'edge, 'g) readable_graph -> ('node complete_node * 'edge) list ->
     'node complete_node -> ('node complete_node * 'edge) list;

   update_value_for_fixed_point:
     'value NodeMap.t -> ('node complete_node * 'edge) -> 'value;

   compute_result:
     'value NodeMap.t -> ('node complete_node * 'edge) -> 'res -> 'res;
   (* **
    * This function compute the new result from visited_nodes,
    * the current node and the current result value
    * *)

   equal_value: 'value -> 'value -> bool;

   predicate: 'value NodeMap.t -> ('node complete_node * 'edge) -> bool;
   (* **
    * The predicate is called with visited_nodes as first argument and
    * the algorithm only fold on the node if predicate returns true
    * *)

   initial_value:  'value;
   initial_result: 'res;
  }

val breadth_first_fold:
  ('node, 'edge, 'g, 'value, 'res) fold_configuration ->
  ('node, 'edge, 'g) readable_graph ->
  'node complete_node -> 'res

val get_forward_config:
  ('value NodeMap.t -> ('node complete_node * 'edge) -> 'value) ->
  ('value NodeMap.t -> ('node complete_node * 'edge) -> 'res -> 'res) ->
  ('value -> 'value -> bool) ->
  ('value NodeMap.t -> ('node complete_node * 'edge) -> bool) ->
  'value -> 'res ->
  ('node, 'edge, 'g, 'value, 'res) fold_configuration

val get_backward_config:
  ('value NodeMap.t -> ('node complete_node * 'edge) -> 'value) ->
  ('value NodeMap.t -> ('node complete_node * 'edge) -> 'res -> 'res) ->
  ('value -> 'value -> bool) ->
  ('value NodeMap.t -> ('node complete_node * 'edge) -> bool) ->
  'value -> 'res ->
  ('node, 'edge, 'g, 'value, 'res) fold_configuration

val get_basic_node_config:
  (bool NodeMap.t -> ('node complete_node * 'edge) -> bool) ->
  NodeiSet.t ->
  ('node, 'edge, 'g, bool, NodeiSet.t) fold_configuration

val get_backward_basic_node_config:
  (bool NodeMap.t -> ('node complete_node * 'edge) -> bool) ->
  NodeiSet.t ->
  ('node, 'edge, 'g, bool, NodeiSet.t) fold_configuration

val conditional_get_post_dominated:
  (('node complete_node * 'edge) -> bool) ->
  ('node, 'edge, 'g) readable_graph ->
  'node complete_node -> NodeiSet.t
