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

  module NodeMap  = Map.Make(RG.Key)

  let add_successors   cfg acc n =
    fold_successors   (fun cn acc -> cn::acc) cfg n.index acc

  let add_predecessors cfg acc n =
    fold_predecessors (fun cn acc -> cn::acc) cfg n.index acc

  type ('node, 'g, 'value, 'res) fold_configuration =
    {get_next_nodes:
       ('node, 'g) readable_graph ->
       ('node complete_node * RG.Edge.t) list ->
       'node complete_node -> ('node complete_node * RG.Edge.t) list;

     update_value_for_fixed_point:
       'value NodeMap.t -> ('node complete_node * RG.Edge.t) -> 'value;

     compute_result:
       'value NodeMap.t -> ('node complete_node * RG.Edge.t) -> 'res -> 'res;

     equal_value: 'value -> 'value -> bool;

     predicate: 'value NodeMap.t -> ('node complete_node * RG.Edge.t) -> bool;

     initial_value:  'value;
     initial_result: 'res;
    }

  type ('value, 'a, 'node, 'edge) breadth_first_fold_data =
    {visited_nodes:    'value NodeMap.t;
     last_added_nodes: 'node complete_node list;
     result:           'a}


  let breadth_first_fold config g cn =
    let rec breadth_first_fold_aux data =
      let next_nodes =
        List.fold_left (config.get_next_nodes g) [] data.last_added_nodes
      in

      let new_visited_nodes =
        List.fold_left
          (fun visited_nodes (s, e) ->
             let new_value =
               config.update_value_for_fixed_point visited_nodes (s, e)
             in
             NodeMap.add s.index new_value visited_nodes)
          data.visited_nodes next_nodes
      in

      let new_result =
        List.fold_left
          (fun result cn -> config.compute_result new_visited_nodes cn result)
          data.result next_nodes
      in

      let add_no_double cn l =
        if List.exists (fun n -> (=) cn.index n.index) l
        then l
        else cn::l
      in

      let add_to_visited_nodes added_nodes (s, e) =
        let old_value =
          try Some (NodeMap.find s.index data.visited_nodes)
          with Not_found -> None
        in
        let new_value = NodeMap.find s.index new_visited_nodes in
        let should_visit = config.predicate new_visited_nodes (s, e) in

        match (should_visit, old_value, new_value) with
          ( true,    None,  v) ->
          add_no_double s added_nodes
        | ( true, Some ov, nv) when not (config.equal_value ov nv) ->
          add_no_double s added_nodes
        | _ ->
          added_nodes
      in

      let added_nodes =
        List.fold_left add_to_visited_nodes [] next_nodes
      in

      if NodeMap.equal config.equal_value new_visited_nodes data.visited_nodes
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


  let get_forward_config update_value compute_result equal_value predicate
      initial_value initial_result =
    {get_next_nodes                 = add_successors;
     update_value_for_fixed_point   = update_value;
     compute_result                 = compute_result;
     equal_value                    = equal_value;
     predicate                      = predicate;
     initial_value                  = initial_value;
     initial_result                 = initial_result;}


  let get_backward_config update_value compute_result equal_value predicate
      initial_value initial_result =
    {get_next_nodes                 = add_predecessors;
     update_value_for_fixed_point   = update_value;
     compute_result                 = compute_result;
     equal_value                    = equal_value;
     predicate                      = predicate;
     initial_value                  = initial_value;
     initial_result                 = initial_result;}


  let get_basic_node_config predicate initial_result =
    get_forward_config predicate
      (fun visited_nodes (s, _) res ->
         if NodeMap.mem  s.index visited_nodes &&
            NodeMap.find s.index visited_nodes
         then
           RG.KeySet.add s.index res
         else
           res)
      (=) predicate true initial_result

  let get_backward_basic_node_config predicate initial_result =
    get_backward_config predicate
      (fun visited_nodes (s, _) res ->
         if NodeMap.mem  s.index visited_nodes &&
            NodeMap.find s.index visited_nodes
         then
           RG.KeySet.add s.index res
         else
           res)
      (=) predicate true initial_result

  let conditional_get_post_dominated p g cn =
    let predicate set (n, e) =
      p (n, e) &&
      fold_successors
        (fun (cn, _) acc -> acc &&
                            (NodeMap.mem  cn.index set &&
                             NodeMap.find cn.index set))
        g n.index true
    in
    let config =
      get_backward_basic_node_config predicate (RG.KeySet.singleton cn.index)
    in
    breadth_first_fold config g cn
end
