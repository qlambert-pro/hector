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

module Make(RG : ReadableGraph) =
struct
  type key          = RG.Key.t
  type keys         = RG.KeySet.t
  type edge         = RG.Edge.t
  type edges        = RG.KeyEdgeSet.t
  type 'node keymap = 'node RG.KeyMap.t


  type ('node, 'g) readable_graph =
    < nodes: 'node RG.KeyMap.t;
      successors:   key -> RG.KeyEdgeSet.t;
      predecessors: key -> RG.KeyEdgeSet.t;
      ..
    > as 'g

  let complete_node_of g index =
    {index = index;
     node  = RG.KeyMap.find index g#nodes;
    }

  let fold_node g f acc = RG.KeyMap.fold f g#nodes acc

  let fold_predecessors f g node acc =
    RG.KeyEdgeSet.fold (fun (i, e) a -> f (complete_node_of g i, e) a)
      (g#predecessors node) acc

  let fold_successors f g node acc =
    RG.KeyEdgeSet.fold (fun (i, e) a -> f (complete_node_of g i, e) a)
      (g#successors node) acc

  let find_all f g = RG.KeyMap.filter f g#nodes
end
