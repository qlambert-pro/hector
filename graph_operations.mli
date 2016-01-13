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

type ('node, 'edge, 'g) readable_graph =
  < nodes: (nodei, 'node) Oassoc.oassoc;

    successors:   nodei -> (nodei * 'edge) Oset.oset;
    predecessors: nodei -> (nodei * 'edge) Oset.oset;
    ..
  > as 'g

val fold_node:
  ('a -> (nodei * 'node) -> 'a) ->
  'a -> ('node, 'edge, 'g) readable_graph -> 'a

val fold_predecessors:
  ('a -> (nodei * 'node) -> 'a) ->
  'a -> ('node, 'edge, 'g) readable_graph -> nodei -> 'a

val fold_successors:
  ('a -> (nodei * 'node) -> 'a) ->
  'a -> ('node, 'edge, 'g) readable_graph -> nodei -> 'a

val find_all:
  (nodei * 'node -> bool) -> ('node, 'edge, 'g) readable_graph ->
  (nodei * 'node) list

val conditional_breadth_first_search:
  (nodei * 'node -> bool) -> ('node, 'edge, 'g) readable_graph ->
  nodei -> NodeiSet.t
