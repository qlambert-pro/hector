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

module Make (Val: EqualType) (Ops : S) :
sig
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


  module NodeMap : Map.S with type key = Ops.key

  type ('node, 'g, 'res) configuration =
    {get_next_nodes:
       ('node, 'g) readable_graph ->
       ('node GO.complete_node * edge) list ->
       'node GO.complete_node -> ('node GO.complete_node * edge) list;

     update_value:
       value NodeMap.t -> ('node GO.complete_node * edge) -> value;

     compute_result:
       value NodeMap.t -> ('node GO.complete_node * edge) ->
       'res -> 'res;
     (* **
      * This function compute the new result from visited_nodes,
      * the current node and the current result value
      * *)

     predicate: value NodeMap.t -> ('node GO.complete_node * edge) -> bool;
     (* **
      * The predicate is called with visited_nodes as first argument and
      * the algorithm only fold on the node if predicate returns true
      * *)

     initial_value:  value;
     initial_result: 'res;
    }

  val compute:
    ('node, 'g, 'res) configuration ->
    ('node, 'g) readable_graph ->
    'node GO.complete_node -> 'res

  val get_forward_config:
    (value NodeMap.t -> ('node GO.complete_node * edge) -> value) ->
    (value NodeMap.t -> ('node GO.complete_node * edge) ->
     'res -> 'res) ->
    (value NodeMap.t -> ('node GO.complete_node * edge) -> bool) ->
    value -> 'res ->
    ('node, 'g, 'res) configuration

  val get_backward_config:
    (value NodeMap.t -> ('node GO.complete_node * edge) -> value) ->
    (value NodeMap.t -> ('node GO.complete_node * edge) ->
     'res -> 'res) ->
    (value NodeMap.t -> ('node GO.complete_node * edge) -> bool) ->
    value -> 'res ->
    ('node, 'g, 'res) configuration
end
