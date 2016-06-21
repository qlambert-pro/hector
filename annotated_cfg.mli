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

exception NoCFG

type resource =
    Void     of Ast_c.expression option
  | Resource of Ast_c.expression

type resource_handling =
    Allocation  of resource
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

type edge =
    Direct of node complete_node * node complete_node
  | PostBackedge of node complete_node * node complete_node



type t = (node, edge) Ograph_extended.ograph_mutable

val of_ast_c: Ast_c.toplevel -> t

val get_error_handling_branch_head:
  t -> (node complete_node) list

val resource_equal: resource -> resource -> bool
val is_void_resource: resource -> bool
val is_similar_statement: node complete_node -> node complete_node -> bool
val is_returning_resource: resource -> node complete_node -> bool
val is_referencing_resource: resource -> node -> bool

val is_on_error_branch: t -> node complete_node -> node -> bool

val get_function_call_name: node complete_node -> string option

val line_number_of_node: t -> node complete_node -> int
