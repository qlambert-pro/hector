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
module ACFG = Annotated_cfg


val of_ast_c: Ast_c.toplevel -> ACFG.t


val line_number_of_node: ACFG.t -> ACFG.node GO.complete_node -> int
val get_top_node: ACFG.t -> ACFG.node GO.complete_node

val is_last_reference:
  ACFG.t -> ACFG.node GO.complete_node -> ACFG.resource -> bool
val is_last_reference_before_killed_reached:
  Ast_c.expression -> ACFG.t -> ACFG.node GO.complete_node -> ACFG.resource ->
  bool
val is_first_reference:
  ACFG.t -> ACFG.node GO.complete_node -> ACFG.resource -> bool


val is_return_value_tested: ACFG.t -> ACFG.node GO.complete_node -> bool
