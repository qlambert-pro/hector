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

type key = int

module type ReadableGraph =
sig
  module Key: Set.OrderedType with type t = key
  module KeySet : Set.S with type elt = Key.t
  module KeyMap : Map.S with type key = Key.t
  module Edge : Set.OrderedType
  module KeyEdgePair : Set.OrderedType with type t = Key.t * Edge.t
  module KeyEdgeSet : Set.S with type elt = KeyEdgePair.t
end

type 'node complete_node =
  {index: key;
   node:  'node;
  }

module Make (RG : ReadableGraph) :
sig
  type key          = RG.Key.t
  type edge         = RG.Edge.t
  type edges        = RG.KeyEdgeSet.t
  type 'node keymap = 'node RG.KeyMap.t


  type ('node, 'g) readable_graph =
    < nodes: 'node  keymap;
      successors:   key -> edges;
      predecessors: key -> edges;
      ..
    > as 'g

  val fold_node:
    ('node, 'g) readable_graph ->
    (key -> 'node -> 'a -> 'a) ->
    'a ->  'a

  val complete_node_of:
    ('node, 'g) readable_graph -> key -> 'node complete_node

  val fold_predecessors:
    (('node complete_node * edge) -> 'a -> 'a) ->
    ('node, 'g) readable_graph -> key -> 'a -> 'a

  val fold_successors:
    (('node complete_node * edge) -> 'a -> 'a) ->
    ('node, 'g) readable_graph -> key -> 'a -> 'a

  val find_all:
    (key -> 'node -> bool) -> ('node, 'g) readable_graph -> 'node keymap

  module NodeMap : Map.S with type key = key

  type ('node, 'g, 'value, 'res) fold_configuration =
    {get_next_nodes:
       ('node, 'g) readable_graph -> ('node complete_node * RG.Edge.t) list ->
       'node complete_node -> ('node complete_node * RG.Edge.t) list;

     update_value_for_fixed_point:
       'value NodeMap.t -> ('node complete_node * RG.Edge.t) -> 'value;

     compute_result:
       'value NodeMap.t -> ('node complete_node * RG.Edge.t) -> 'res -> 'res;
     (* **
      * This function compute the new result from visited_nodes,
      * the current node and the current result value
      * *)

     equal_value: 'value -> 'value -> bool;

     predicate: 'value NodeMap.t -> ('node complete_node * RG.Edge.t) -> bool;
     (* **
      * The predicate is called with visited_nodes as first argument and
      * the algorithm only fold on the node if predicate returns true
      * *)

     initial_value:  'value;
     initial_result: 'res;
    }

  val breadth_first_fold:
    ('node, 'g, 'value, 'res) fold_configuration ->
    ('node, 'g) readable_graph ->
    'node complete_node -> 'res

  val get_forward_config:
    ('value NodeMap.t -> ('node complete_node * RG.Edge.t) -> 'value) ->
    ('value NodeMap.t -> ('node complete_node * RG.Edge.t) -> 'res -> 'res) ->
    ('value -> 'value -> bool) ->
    ('value NodeMap.t -> ('node complete_node * RG.Edge.t) -> bool) ->
    'value -> 'res ->
    ('node, 'g, 'value, 'res) fold_configuration

  val get_backward_config:
    ('value NodeMap.t -> ('node complete_node * RG.Edge.t) -> 'value) ->
    ('value NodeMap.t -> ('node complete_node * RG.Edge.t) -> 'res -> 'res) ->
    ('value -> 'value -> bool) ->
    ('value NodeMap.t -> ('node complete_node * RG.Edge.t) -> bool) ->
    'value -> 'res ->
    ('node, 'g, 'value, 'res) fold_configuration

  val get_basic_node_config:
    (bool NodeMap.t -> ('node complete_node * RG.Edge.t) -> bool) ->
    RG.KeySet.t ->
    ('node, 'g, bool, RG.KeySet.t) fold_configuration

  val get_backward_basic_node_config:
    (bool NodeMap.t -> ('node complete_node * RG.Edge.t) -> bool) ->
    RG.KeySet.t ->
    ('node, 'g, bool, RG.KeySet.t) fold_configuration

  val conditional_get_post_dominated:
    (('node complete_node * RG.Edge.t) -> bool) ->
    ('node, 'g) readable_graph -> 'node complete_node -> RG.KeySet.t



end
