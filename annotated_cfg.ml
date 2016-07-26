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

open Graph_operations

module Asto = Ast_operations

exception NoCFG

type resource =
    Void     of Ast_c.expression option
  | Resource of Ast_c.expression

let resource_equal r1 r2 =
  match (r1, r2) with
    (Void     None, Void     None)
  | (Void (Some _), Void (Some _)) -> true
  | (   Resource e1,    Resource e2) ->
    Asto.expression_equal e1 e2
  | _ -> false

type assignment = {
  left_value: Ast_c.expression;
  right_value: Asto.assignment;
}

type resource_handling =
    Allocation  of resource
  | Assignment  of assignment
  | Release     of resource
  | Computation of Ast_c.expression list
  | Test        of Ast_c.expression list
  | Unannotated

type node = {
  is_error_handling: bool;
  resource_handling_type: resource_handling;
  referenced_resources: Ast_c.expression list;
  parser_node: Control_flow_c.node
}

let mk_node is_error_handling parser_node = {
  is_error_handling = is_error_handling;
  resource_handling_type = Unannotated;
  referenced_resources = [];
  parser_node = parser_node
}

let is_similar_statement n1 n2 =
  n1.index = n2.index ||
  match (n1.node.parser_node, n2.node.parser_node) with
    (((Control_flow_c.ExprStatement (st1, _), _), _),
     ((Control_flow_c.ExprStatement (st2, _), _), _)) ->
    Asto.statement_equal st1 st2
  | _ -> false

type edge =
    Direct of node complete_node * node complete_node
  | PostBackedge of node complete_node * node complete_node

type t = (node, edge) Ograph_extended.ograph_mutable

(**** Unwrapper and boolean function on Control_flow_c ****)

let is_after_node node =
  match Control_flow_c.unwrap node.parser_node with
    Control_flow_c.AfterNode _ -> true
  | _ -> false


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


let line_number_of_node cfg cn =
  let rec aux cn =
    let st = Control_flow_c.extract_fullstatement cn.node.parser_node in
    let succ = (cfg#successors cn.index)#tolist in
    match (st, succ) with
      (  None, [(i, _)]) -> aux (complete_node_of cfg i)
    | (Some s,        _) ->
      let (_, _, (l, _), _) =
        Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_stmt s)
      in
      l
    | (  None,       xs) ->
      failwith
        "the block head does not reach a node with line infos before branching."
  in
  aux cn

let side_effect_visitor f = {
  Visitor_c.default_visitor_c with
  Visitor_c.kexpr =
    (fun (k, visitor) e ->
       Asto.apply_on_assignment
         f
         e;
       Asto.apply_on_funcall_side_effect
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
  apply_side_effect_visitor
    (fun r l -> acc := !acc || Asto.expression_equal identifier r)
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
  Visitor_c.vk_node visitor n.node.parser_node;
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
  Visitor_c.vk_node visitor n.node.parser_node;
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
  match cn.node.resource_handling_type with
    Assignment a ->
    (match a.right_value with
       Asto.Variable _ -> true
     | _ -> false)
  | _ -> false

let filter_returns cfg identifier nodes =
  NodeiSet.filter
    (fun index ->
       test_returned_expression
         (Asto.expression_equal identifier)
         false
         (cfg#nodes#assoc index))
    nodes


let get_top_node cfg =
  let top_nodes = find_all (fun (_, n) -> is_top_node n) cfg in
  match top_nodes with
    [(i, n)] -> {index = i; node = n}
  | _        -> failwith "malformed control flow graph"

let annotate_resource cfg cn resource =
  match (cn.node.resource_handling_type, resource) with
    (            _,       Test _)
  | (            _,    Release _)
  | (Computation _, Allocation _)
  | (Assignment  _, Allocation _)
  | (Unannotated  ,            _) ->
    cfg#replace_node
      (cn.index, {cn.node with resource_handling_type = resource})
  | _ -> ()

let configurable_is_reference config cfg cn r =
  let nodes' = breadth_first_fold config cfg cn in
  let nodes  = NodeiSet.remove cn.index nodes' in
  NodeiSet.fold
    (fun i acc -> acc && not (is_referencing_resource r (cfg#nodes#assoc i)))
    nodes
    true

let is_last_reference =
  configurable_is_reference (get_basic_node_config (fun _ _ -> true))

let is_last_reference_before_killed_reached identifier =
  configurable_is_reference
    (get_basic_node_config
       (fun _ (n, _) -> not (is_killing_reach identifier n.node)))

let is_first_reference =
  configurable_is_reference (get_backward_basic_node_config (fun _ _ -> true))


let is_return_value_tested cfg cn =
  let assigned_variable = ref None in
  apply_side_effect_visitor
    (fun l r -> assigned_variable := Some l)
    cn.node;
  match !assigned_variable with
    None    -> false
  | Some id ->
    let downstream_nodes =
      breadth_first_fold
        (get_basic_node_config (fun _ _ -> true))
        cfg cn
    in
    NodeiSet.for_all
      (fun i ->
         test_if_header
           (fun e -> Asto.is_testing_identifier id e)
           false (cfg#nodes#assoc i))
      downstream_nodes


let remove_after_nodes_mutable cfg =
  let remove_pred_arcs i =
    fold_predecessors
      (fun _ (cn, e) -> cfg#del_arc ((cn.index, i), e))
      () cfg i
  in
  let remove_succ_arcs i =
    fold_successors
      (fun _ (cn, e) -> cfg#del_arc ((i, cn.index), e))
      () cfg i
  in
  let remove_after_node () (i, node) =
    let n = (mk_node false node) in
    if (is_after_node n)
    then
      (remove_pred_arcs i;
       remove_succ_arcs i;
       cfg#del_node i)
    else
      ()
  in
  fold_node remove_after_node () cfg

(*TODO improve to only one pass*)
let of_ast_c ast =
  let cocci_cfg =
    match Control_flow_c_build.ast_to_control_flow ast with
      Some cfg -> Control_flow_c_build.annotate_loop_nodes cfg
    | None     -> raise NoCFG
  in

  remove_after_nodes_mutable cocci_cfg;
  let cfg = new Ograph_extended.ograph_mutable in
  let added_nodes = Hashtbl.create 100 in

  let process_node cn =
    let add_node cn =
      if not (Hashtbl.mem added_nodes cn.index)
      then
        let index' = cfg#add_node (mk_node false cn.node) in
        Hashtbl.add added_nodes cn.index index'
      else
        ()
    in

    let add_node_and_arc (index', _) =
      let node' = (cocci_cfg#nodes)#assoc index' in
      add_node {index = index'; node = node'};

      let start_node = Hashtbl.find added_nodes cn.index in
      let end_node   = Hashtbl.find added_nodes index'   in
      let post_dominated =
        conditional_get_post_dominated
          (fun _ -> true)
          cocci_cfg (complete_node_of cocci_cfg cn.index)
      in

      let edge =
        if NodeiSet.mem index' post_dominated
        then PostBackedge
            ({index = start_node; node = cfg#nodes#assoc start_node},
             {index = end_node  ; node = cfg#nodes#assoc end_node  })
        else Direct
            ({index = start_node; node = cfg#nodes#assoc start_node},
             {index = end_node  ; node = cfg#nodes#assoc end_node  })
      in
      cfg#add_arc ((start_node, end_node), edge)
    in

    add_node cn;
    let successors = cocci_cfg#successors cn.index in
    successors#iter add_node_and_arc
  in
  let top_node = Control_flow_c.first_node cocci_cfg in
  process_node {index = top_node; node = (cocci_cfg#nodes)#assoc top_node};
  (Common.profile_code "create_cfg" (fun () ->
       breadth_first_fold
         (get_forward_config
            (fun _ _ -> true)
            (fun _ (cn, _) () -> process_node cn)
            ())
         cocci_cfg
         (complete_node_of cocci_cfg top_node)));
  cfg

let is_returning_resource resource cn =
  match resource with
    Void _     -> false
  | Resource r ->
    test_returned_expression (Asto.expression_equal r) false cn.node

let get_assignment cn =
  let assignment = ref None in
  let f l r =
    match r with
      Some r ->
      assignment :=
        Some {left_value = l; right_value = Asto.get_assignment_type r}
    | None   -> ()
  in
  apply_assignment_visitor f cn;
  !assignment
