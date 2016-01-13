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

let fold_node f acc g =
  let nodes = g#nodes#tolist in
  List.fold_left f acc nodes

let fold_predecessors f acc g node =
  let nodes = g#nodes#tolist in
  let predecessors_indices = List.map fst (g#predecessors node)#tolist in
  let (predecessors, _) =
    List.partition
      (fun (index, _) -> List.exists ((=) index) predecessors_indices)
      nodes
  in
  List.fold_left f acc predecessors

let fold_successors f acc g node =
  let nodes = g#nodes#tolist in
  let successors_indices = List.map fst (g#successors node)#tolist in
  let (successors, _) =
    List.partition
      (fun (index, _) -> List.exists ((=) index) successors_indices)
      nodes
  in
  List.fold_left f acc successors

let find_all f g =
  let nodes = g#nodes#tolist in
  List.find_all f nodes

module OrderedNodei =
struct
  type t = nodei
  let compare = compare
end

module NodeiSet = Set.Make(OrderedNodei)

let successors_of_set cfg set =
  NodeiSet.fold
    (fun index acc ->
       fold_successors (fun acc (s, _) -> NodeiSet.add s acc) acc cfg index)
    set
    NodeiSet.empty

let conditional_breadth_first_search p g node =
  let nodes = g#nodes in
  let rec conditional_breadth_first_search_aux visited_nodes =

    let successors = successors_of_set g visited_nodes in

    let add_to_visited_nodes s visited_nodes =
      let node = nodes#assoc s in
      if p (s, node)
      then NodeiSet.add s visited_nodes
      else visited_nodes
    in

    let new_visited_nodes =
      NodeiSet.fold add_to_visited_nodes successors visited_nodes in

    if new_visited_nodes = visited_nodes
    then new_visited_nodes
    else conditional_breadth_first_search_aux new_visited_nodes
  in
  let initial_set = NodeiSet.add node NodeiSet.empty in
  conditional_breadth_first_search_aux initial_set
