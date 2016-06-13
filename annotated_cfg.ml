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
    (Void _ , Void _) -> true
  | (   Resource e1,    Resource e2) ->
    Asto.expression_equal e1 e2
  | _ -> false

type resource_handling =
    Allocation  of resource
  | Release     of resource
  | Computation of Ast_c.expression list
  | Unannotated

type node = {
  is_error_handling: bool;
  resource_handling_type: resource_handling;
  parser_node: Control_flow_c.node
}

let mk_node is_error_handling resource_handling_type parser_node = {
  is_error_handling = is_error_handling;
  resource_handling_type = resource_handling_type;
  parser_node = parser_node
}

let is_similar_statement n1 n2 =
  n1.index = n2.index ||
  match (n1.node.parser_node, n2.node.parser_node) with
    (((Control_flow_c.ExprStatement (st1, _), _), _),
     ((Control_flow_c.ExprStatement (st2, _), _), _)) ->
    Asto.statement_equal st1 st2
  | _ -> false

let statement_info_visitor f = {
  Visitor_c.default_visitor_c with
  Visitor_c.kexpr = (fun (k, visitor) (_, i)-> f i);
}

let line_number_of_node cn =
  let line_number = ref 0 in
  let v = statement_info_visitor
      (fun i ->
         if i <> []
         then
           let (_, _, (l,_), _) = Lib_parsing_c.lin_col_by_pos i in
           line_number := l
         else
           ())
  in
  Visitor_c.vk_node v cn.node.parser_node;
  !line_number

type edge =
    Direct
  | PostBackedge

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


let base_visitor f = {
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


let apply_base_visitor f node =
  let visitor = base_visitor f in
  Visitor_c.vk_node visitor node.parser_node


let is_killing_reach identifier node =
  let acc = ref false in
  apply_base_visitor
    (fun r l -> acc := !acc || Asto.expression_equal identifier r)
    node;
  !acc


let get_assignment_type_through_alias cfg =
  let visited_node = ref NodeiSet.empty in
  let rec aux n e =
    visited_node := NodeiSet.add n.index !visited_node;
    match Asto.get_assignment_type e with
      Asto.Variable e ->
      let assignements =
        breadth_first_fold
          (get_backward_config
             (fun _ _ (n, _) -> not (is_killing_reach e n.node))
             (fun _ _ _ -> ())
             (fun _ (n, _) res ->
                if is_top_node n.node
                then
                  (Asto.Error Asto.Ambiguous)::res
                else
                  let temp = ref None in
                  apply_base_visitor
                    (fun l r ->
                       if Asto.expression_equal e l
                       then
                         match r with
                           Some r ->
                           temp :=
                             if NodeiSet.mem n.index !visited_node
                             then
                               Some (Asto.Error Asto.Ambiguous)
                             else
                               Some (aux n r)
                         | None   -> temp := Some (Asto.Error Asto.Ambiguous)
                       else
                         ())
                    n.node;
                  match !temp with
                    None   -> res
                  | Some v -> v::res)
             () [])
          cfg (complete_node_of cfg n.index)
      in
      if List.for_all ((=) (List.hd assignements)) assignements
      then
        List.hd assignements
      else
        Asto.Error Asto.Ambiguous
    | Asto.Value v    -> v
  in
  aux

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


let is_referencing_resource r n =
  let has_referenced = ref true in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e ->
         has_referenced :=
           !has_referenced &&
           Asto.expression_equal e r;
         k e)
  }
  in
  Visitor_c.vk_node visitor n.parser_node;
  !has_referenced


let get_error_branch cfg n =
  let branch_side = ref Asto.Then in
  let visitor = {
    Visitor_c.default_visitor_c with
    Visitor_c.kexpr =
      (fun (k, visitor) e ->
         Asto.which_is_the_error_branch
           (get_assignment_type_through_alias cfg n)
           (fun x -> branch_side := x) e)
  }
  in
  Visitor_c.vk_node visitor n.node.parser_node;
  !branch_side

(* **
 * "node" is expected to be a IfHeader
 * and "head" one of the following node
 * *)
let is_on_error_branch cfg n head =
  let error_branch_side = get_error_branch cfg n in
  match (error_branch_side, Control_flow_c.unwrap head.parser_node) with
    (Asto.Then, Control_flow_c.TrueNode _)
  | (Asto.Else, Control_flow_c.FalseNode )
  | (Asto.Else, Control_flow_c.FallThroughNode ) -> true
  | _ -> false
(**********************************************************)

let get_error_assignments cfg identifiers =
  let t = Hashtbl.create 101 in
  cfg#nodes#iter
    (fun (i, node) ->
       apply_base_visitor
         (fun l r ->
            if List.exists (fun e -> Asto.expression_equal e l) identifiers
            then
              match r with
                Some r ->
                let assignment_type =
                  get_assignment_type_through_alias
                    cfg {index = i; node = node} r
                in
                (match assignment_type with
                   Asto.Error err -> Hashtbl.add t ((i, node), l) err
                 | _    -> ())
              | None ->  Hashtbl.add t ((i, node), l) Asto.Ambiguous
            else
              ())
         node);
  t


let filter_returns cfg identifier nodes =
  NodeiSet.filter
    (fun index ->
       test_returned_expression
         (Asto.expression_equal identifier)
         false
         (cfg#nodes#assoc index))
    nodes


let add_branch_nodes_leading_to_return cfg returns nodes acc =
  let post_dominated index =
    let is_backedge e =
      match e with
        Direct   -> false
      | PostBackedge -> true
    in
    let predicate _ set (n, e) =
      let is_post_dominated =
        fold_successors
          (fun acc (cn, e) ->
             acc &&
             (is_backedge e || NodeiSet.mem cn.index set))
          true cfg n.index
      in
      NodeiSet.mem n.index nodes &&
      is_post_dominated
    in
    let config = get_backward_basic_node_config predicate in
    breadth_first_fold config cfg (complete_node_of cfg index)
  in

  NodeiSet.fold
    (fun return acc ->
       let branch_nodes = post_dominated return in
       NodeiSet.union branch_nodes acc)
    returns acc

let add_post_dominated cfg index acc =
  let post_dominated =
    conditional_get_post_dominated
      (fun _ -> true)
      cfg (complete_node_of cfg index)
  in
  NodeiSet.union post_dominated acc

let get_nodes_leading_to_error_return cfg error_assignments =
  let get_reachable_nodes ((index, node), identifier) error_type acc =
    match error_type with
      Asto.Clear ->
      (Common.profile_code "clear" (fun () ->
      let nodes =
        breadth_first_fold
          (get_basic_node_config
             (fun _ _ (cn, _) -> not (is_killing_reach identifier cn.node)))
          cfg (complete_node_of cfg index)
      in
      let reachable_returns = filter_returns cfg identifier nodes in
      if reachable_returns != NodeiSet.empty
      then
        let error_branch_nodes =
          add_branch_nodes_leading_to_return cfg reachable_returns nodes acc
        in
        if NodeiSet.mem index error_branch_nodes
        then
          add_post_dominated cfg index error_branch_nodes
        else
          error_branch_nodes
      else
        acc))
    | Asto.Ambiguous ->
      (Common.profile_code "ambiguous" (fun () ->
      let nodes =
        depth_first_fold
          (get_forward_config
             (fun _ _ (cn, _) -> not (is_killing_reach identifier cn.node))
             (*TODO is_testing_identifier is incorrect*)
             (* add complex if cases to prove it *)
             (*TODO use a finer memory of the error branch *)
             (fun _ (cn, e) (pred, acc) ->
                match pred with
                  None -> (Some cn, acc)
                | Some pred ->
                  let is_on_error_branch_result =
                    let head = cn.node in
                    let is_correct_if =
                      test_if_header
                        (Asto.is_testing_identifier identifier) false pred.node
                    in
                    let is_correct_branch =
                      is_on_error_branch cfg pred head
                    in
                    is_correct_if && is_correct_branch
                  in
                  (Some cn, acc || is_on_error_branch_result))
             (fun (_, acc) (cn, _) res ->
                if acc
                then NodeiSet.add cn.index res
                else res)
             (None, false)
             NodeiSet.empty)
          cfg (complete_node_of cfg index)
      in
      let reachable_returns = filter_returns cfg identifier nodes in
      if reachable_returns != NodeiSet.empty
      then
        add_branch_nodes_leading_to_return cfg reachable_returns nodes acc
      else
        acc
         ))
  in
  Hashtbl.fold get_reachable_nodes error_assignments NodeiSet.empty


(*TODO optimise number of pass*)
(*TODO replace as computed rather than store*)
let annotate_error_handling cfg =
  let error_returns, identifiers =

  (Common.profile_code "get_returns" (fun () ->
    fold_node
      (fun (error_returns, id_set) (i, node) ->
         let error_type =
           test_returned_expression Asto.get_assignment_type
             (Asto.Value Asto.NonError) node
         in
         match error_type with
           Asto.Value (Asto.Error Asto.Clear) ->
           ((i,node)::error_returns, id_set)
         | Asto.Variable e ->
           (error_returns, e::id_set)
         | _ -> (error_returns, id_set))
      ([], []) cfg))
  in
  let error_assignments =
  (Common.profile_code "get_assignement" (fun () ->
    get_error_assignments cfg identifiers)) in
  let error_branch_nodes =
  (Common.profile_code "get_nodes" (fun () ->
    get_nodes_leading_to_error_return cfg error_assignments
       ))
  in
  let error_return_post_dominated =
  (Common.profile_code "get_postdominated" (fun () ->
    List.fold_left
      (fun acc (index, _) ->
         let nodes =
           conditional_get_post_dominated (fun _ -> true) cfg
             (complete_node_of cfg index)
         in
         NodeiSet.union acc nodes)
      NodeiSet.empty error_returns
       ))
  in
  (Common.profile_code "annotate_error_handling" (fun () ->
  fold_node
    (fun () (index, node) ->
       if NodeiSet.mem index error_branch_nodes ||
          NodeiSet.mem index error_return_post_dominated
       then
         let {parser_node = parser_node} = node in
         cfg#replace_node (index, (mk_node true Unannotated parser_node))
       else
         ())
    () cfg
     ))


let is_head cfg (index, node) =
  let predecessors = cfg#predecessors index in
  node.is_error_handling &&
  predecessors#exists
    (fun (index, _) ->
       let node = cfg#nodes#assoc index in
       is_selection node &&
       not (node.is_error_handling))


(*TODO maybe optimise*)
let get_error_handling_branch_head cfg =
  let nodes = find_all (is_head cfg) cfg in
  List.fold_left
    (fun acc (i, n) -> {node = n; index = i}::acc) [] nodes

let get_top_node cfg =
  let top_nodes = find_all (fun (_, n) -> is_top_node n) cfg in
  match top_nodes with
    [(i, n)] -> {index = i; node = n}
  | _        -> failwith "malformed control flow graph"

let annotate_resource cfg cn resource =
  match (cn.node.resource_handling_type, resource) with
    (            _,     Release _)
  | (Allocation  _, Computation _)
  | (  Unannotated,             _) ->
    cfg#replace_node
      (cn.index, {cn.node with resource_handling_type = resource})
  | _ -> ()

let configurable_is_reference config cfg cn r =
  let downstream_nodes = breadth_first_fold config cfg cn in
  NodeiSet.fold
    (fun i acc -> acc || is_referencing_resource r (cfg#nodes#assoc i))
    downstream_nodes
    false

let is_last_reference =
  configurable_is_reference (get_basic_node_config (fun _ _ _ -> true))


let is_first_reference =
  configurable_is_reference (get_backward_basic_node_config (fun _ _ _ -> true))


let is_return_value_tested cfg cn =
  let assigned_variable = ref None in
  apply_base_visitor
    (fun l r -> assigned_variable := Some l)
    cn.node;
  match !assigned_variable with
    None    -> false
  | Some id ->
    let downstream_nodes =
      breadth_first_fold
        (get_basic_node_config (fun _ _ _ -> true))
        cfg cn
    in
    NodeiSet.for_all
      (fun i ->
         test_if_header
           (fun e -> Asto.is_testing_identifier id e)
           false (cfg#nodes#assoc i))
      downstream_nodes


(*TODO implement*)
let is_interprocedural c = false


let get_released_resource cfg cn =
  let arguments = get_arguments cn.node in
  let resources = Asto.resources_of_arguments arguments in
  let should_ignore r = List.exists Asto.is_string r in
  match (resources, arguments) with
    (  _, Some   []) -> Some (Void None)
  | ( [], Some  [r]) when not (Asto.is_string   r) -> Some (Void (Some r))
  | ([r], Some args) when not (should_ignore args) &&
                          Asto.is_pointer r ->
    if (is_last_reference cfg cn r &&
        not (is_return_value_tested cfg cn)) ||
       is_interprocedural cn
    then
      Some (Resource r)
    else
      None
  | _           -> None


let annotate_if_release cfg cn =
  let released_resource = get_released_resource cfg cn in
  match released_resource with
    None   -> None
  | Some r ->
    let resource =
      match r with
        Void _ -> None
      | Resource r -> Some r
    in
    annotate_resource cfg cn (Release r);
    resource


let get_resource cfg relevant_resources cn =
  let arguments = get_arguments cn.node in
  let resources = Asto.resources_of_arguments arguments in
  let should_ignore arguments = List.exists Asto.is_string arguments in
  let assigned_variable = ref None in
  apply_base_visitor
    (fun l _ -> if Asto.is_pointer l then assigned_variable := Some l)
    cn.node;
  match (!assigned_variable, resources, arguments) with
  | (  None,   _, Some   []) -> Allocation (Void None)
  | (  None,  [], Some  [r]) when not (Asto.is_string   r) ->
    Allocation (Void (Some r))
  | (Some r,  [], Some args)
    when not (should_ignore args) &&
         List.exists (Asto.expression_equal r) relevant_resources ->
    Allocation (Resource r)
  | (     _,  rs, Some args) when
      List.exists
        (fun e -> List.exists (Asto.expression_equal e) relevant_resources)
        rs ->
    let current_resources =
      List.filter
        (fun e ->
           List.exists (Asto.expression_equal e) relevant_resources &&
           not (is_last_reference cfg cn e))
        rs
    in
    Computation current_resources
  | _ -> Unannotated


let annotate_resource_handling cfg =
  let relevant_resources =
    (Common.profile_code "find_resource" (fun () ->
         fold_node
           (fun acc (i, n) ->
              if n.is_error_handling
              then
                let resource = annotate_if_release cfg {index = i; node = n} in
                match resource with
                  None   -> acc
                | Some r -> r::acc
              else
                acc)
           [] cfg
       ))
  in

  fold_node
    (fun () (i, n) ->
       let cn = {index = i; node = n} in
       annotate_resource cfg cn (get_resource cfg relevant_resources cn)) 
    () cfg;

  let config =
    get_forward_config
      (fun acc _ _ -> match acc with [] -> false | _ -> true)
      (fun _ (cn, _) acc ->
         match cn.node.resource_handling_type with
           Computation [r] ->
           if is_first_reference cfg cn r
           then
             begin
               annotate_resource cfg cn (Allocation (Resource r));
               List.find_all (fun x -> not (Asto.expression_equal x r)) acc
             end
           else
             acc
         | Computation rs ->
           List.find_all
             (fun x -> not (List.mem x rs))
             acc
         | Allocation _
         | Release _
         | Unannotated -> acc)
      (fun _ _ _ -> ())
      relevant_resources ()
  in
  depth_first_fold config cfg (get_top_node cfg)


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
    let n = (mk_node false Unannotated node) in
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
        let index' = cfg#add_node (mk_node false Unannotated cn.node) in
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
        else Direct
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
            (fun _ _ _ -> true)
            (fun _ _ _ -> ())
            (fun _ (cn, _) () -> process_node cn)
            () ())
         cocci_cfg
         (complete_node_of cocci_cfg top_node)));
  (Common.profile_code "error_handling" (fun () ->
       annotate_error_handling cfg));
  (Common.profile_code "resource_handling" (fun () ->
       annotate_resource_handling cfg));
  cfg

let is_returning_resource resource cn =
  match resource with
    Void _     -> false
  | Resource r ->
    test_returned_expression (Asto.expression_equal r) false cn.node
