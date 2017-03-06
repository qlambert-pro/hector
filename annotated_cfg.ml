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
module Asto = Ast_operations

exception NoCFG

type resource =
    Void     of Ast_c.expression option
  | Resource of Ast_c.expression

let resource_equal r1 r2 =
  match (r1, r2) with
    (Void     None, Void     None)
  | (Void (Some _), Void (Some _)) -> true
  | ( Resource e1',  Resource e2') ->
    let e1 = Asto.unify_contained_field e1' in
    let e2 = Asto.unify_contained_field e2' in
    Asto.expression_equal e1 e2
  | _ -> false

type assignment_operator =
    Simple
  | Algebraic

type assignment = {
  left_value: Ast_c.expression;
  operator: assignment_operator;
  right_value: Asto.assignment;
}

type resource_handling =
    Allocation  of resource
  | Assignment  of assignment
  | Release     of resource
  | Computation of Asto.ExpressionSet.t
  | Test        of Asto.ExpressionSet.t
  | Unannotated

type node = {
  is_error_handling: bool;
  resource_handling_type: resource_handling;
  referenced_resources: Asto.ExpressionSet.t;
  parser_node: Control_flow_c.node
}

let is_similar_statement n1 n2 =
  n1.GO.index = n2.GO.index ||
  match (n1.GO.node.parser_node, n2.GO.node.parser_node) with
    (((Control_flow_c.ExprStatement (st1, _), _), _),
     ((Control_flow_c.ExprStatement (st2, _), _), _)) ->
    Asto.statement_equal st1 st2
  | _ -> false

type edge_type =
    Direct
  | PostBackedge

type edge = {
  start_node: int;
  end_node:   int;
  edge_type:  edge_type;
}

module Key : Set.OrderedType with type t = int = struct
  type t = int
  let compare = compare
end

module KeySet : Set.S with type elt = Key.t = Set.Make (Key)

module KeyMap : Map.S with type key = Key.t = Map.Make (Key)

module Edge : Set.OrderedType with type t = edge = struct
  type t = edge
  let compare = compare
end

module KeyEdgePair : Set.OrderedType with type t = Key.t * Edge.t = struct
  type t = Key.t * Edge.t
  let compare = compare
end

module KeyEdgeSet : Set.S with type elt = KeyEdgePair.t =
  Set.Make (KeyEdgePair)

module G = Ograph_extended.Make (Key) (KeySet) (KeyMap)
  (Edge) (KeyEdgePair) (KeyEdgeSet)

type t = node G.ograph_mutable

(**** Unwrapper and boolean function on Control_flow_c ****)

let is_top_node node =
  match Control_flow_c.unwrap node.parser_node with
    Control_flow_c.TopNode -> true
  | _ -> false


let is_selection node =
  match Control_flow_c.unwrap node.parser_node with
    Control_flow_c.IfHeader _ -> true
  | _ -> false


let test_returned_expression predicate default node =
  match Control_flow_c.unwrap node.parser_node with
    Control_flow_c.ReturnExpr (_, (e, _)) -> predicate e
  | _ -> default


let test_if_header predicate default node =
  match Control_flow_c.unwrap node.parser_node with
    Control_flow_c.IfHeader (_, (e, _)) -> predicate e
  | _ -> default


let side_effect_visitor f =
  (*TODO create a type the makes it explicit that either both the operator and
   * the right value exists or neither*)
  let assignment_f e1 op e2 = f e1 (Some op) (Some e2) in
  let funcall_f e1 = f e1 None None in
  {Visitor_c.default_visitor_c with
   Visitor_c.kexpr =
     (fun (k, visitor) e ->
        Asto.apply_on_assignment
          assignment_f
          e;
        Asto.apply_on_funcall_side_effect
          funcall_f
          e;
        k e);
   Visitor_c.konedecl =
     (fun (k, visitor) dl ->
        Asto.apply_on_initialisation
          assignment_f
          dl;
        k dl)
  }

let assignment_visitor f = {
  Visitor_c.default_visitor_c with
  Visitor_c.kexpr =
    (fun (k, visitor) e ->
       Asto.apply_on_assignment
         f
         e;
       k e);
  Visitor_c.konedecl =
    (fun (k, visitor) dl ->
       Asto.apply_on_initialisation
         f
         dl;
       k dl)
}

let apply_side_effect_visitor f node =
  let visitor = side_effect_visitor f in
  Visitor_c.vk_node visitor node.parser_node

let apply_assignment_visitor f node =
  let visitor = assignment_visitor f in
  Visitor_c.vk_node visitor node.parser_node

let is_killing_reach identifier node =
  let acc = ref false in
  apply_assignment_visitor
    (fun r _ _ -> acc := !acc || Asto.expression_equal identifier r)
    node;
  !acc


let get_arguments n =
  let arguments = ref None in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e -> arguments := Asto.get_arguments e)
  }
  in
  Visitor_c.vk_node visitor n.parser_node;
  !arguments


let get_function_call_name n =
  let name = ref None in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e -> name := Asto.function_name_of_expression e)
  }
  in
  Visitor_c.vk_node visitor n.GO.node.parser_node;
  !name

let is_non_alloc n =
  let name = ref false in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e -> name := Asto.is_non_alloc e)
  }
  in
  Visitor_c.vk_node visitor n.GO.node.parser_node;
  !name

let is_void_resource = function
    Void _ -> true
  | _      -> false

let is_referencing_resource resource n =
  match resource with
    Void _     -> false
  | Resource r ->
    let has_referenced = ref false in
    let visitor = {
      Visitor_c.default_visitor_c with
      Visitor_c.kexpr =
        (fun (k, visitor) e ->
           has_referenced :=
             !has_referenced ||
             Asto.expression_equal e r;
           k e)
    }
    in
    Visitor_c.vk_node visitor n.parser_node;
    !has_referenced


let get_error_branch get_assignment_type cfg n =
  let branch_side = ref Asto.Then in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e ->
         Asto.which_is_the_error_branch
           (get_assignment_type cfg n)
           (fun x -> branch_side := x) e)
  }
  in
  Visitor_c.vk_node visitor n.GO.node.parser_node;
  !branch_side

(* **
 * "node" is expected to be a IfHeader
 * and "head" one of the following node
 * *)
let is_on_error_branch get_assignment_type cfg n head =
  let error_branch_side = get_error_branch get_assignment_type cfg n in
  match (error_branch_side, Control_flow_c.unwrap head.parser_node) with
    (Asto.Then, Control_flow_c.TrueNode _)
  | (Asto.Else, Control_flow_c.FalseNode )
  | (Asto.Else, Control_flow_c.FallThroughNode ) -> true
  | _ -> false
(**********************************************************)

let is_assigning_variable cn =
  match cn.GO.node.resource_handling_type with
    Assignment a ->
    (match a.right_value with
       Asto.Variable _ -> true
     | _ -> false)
  | _ -> false

let filter_returns cfg identifier nodes =
  KeySet.filter
    (fun index ->
       test_returned_expression
         (Asto.expression_equal identifier)
         false
         (KeyMap.find index cfg#nodes))
    nodes

let annotate_resource cfg cn resource =
  match (cn.GO.node.resource_handling_type, resource) with
    (            _,       Test _)
  | (            _,    Release _)
  | (Computation _, Allocation _)
  | (Assignment  _, Allocation _)
  | (Unannotated  ,            _) ->
    cfg#replace_node
      (cn.GO.index, {cn.GO.node with resource_handling_type = resource})
  | _ -> ()

let is_returning_resource resource cn =
  match resource with
    Void _     -> false
  | Resource r ->
    test_returned_expression (Asto.expression_equal r) false cn.GO.node

let get_assignment cn =
  let assignment = ref None in
  let f l' op r' =
    let op =
      if Asto.is_simple_assignment op
      then Simple
      else Algebraic
    in
    let l = Asto.unify_contained_field l' in
    let r = Asto.unify_contained_field r' in
    assignment :=
      Some {left_value = l;
            operator = op;
            right_value = Asto.get_assignment_type r}
  in
  apply_assignment_visitor f cn;
  !assignment
