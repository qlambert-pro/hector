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

type branch_side =
    Then
  | Else
  | None

type error =
    Clear
  | Ambiguous

type value =
    NonError
  | Error of error

type assignment =
    Value of value
  | Variable of Ast_c.expression

val get_assignment_type:
  Ast_c.expression -> assignment

val function_name_of_expression:
  Ast_c.expression -> string option

val expression_equal: Ast_c.expression -> Ast_c.expression -> bool
val expression_compare: Ast_c.expression -> Ast_c.expression -> int
val statement_equal: Ast_c.statement -> Ast_c.statement -> bool

val get_arguments:
  Ast_c.expression -> (Ast_c.expression list) option

val resources_of_arguments:
  (Ast_c.expression list) option -> Ast_c.expression list

val is_string:  Ast_c.expression -> bool
val is_pointer: Ast_c.expression -> bool
val is_simple_assignment: Ast_c.assignOp -> bool
val is_testing_identifier:
  Ast_c.expression -> Ast_c.expression -> bool

(* Side Effects *)
val apply_on_assignment:
  (Ast_c.expression -> Ast_c.assignOp -> Ast_c.expression -> unit) ->
  Ast_c.expression -> unit

val apply_on_funcall_side_effect:
  (Ast_c.expression -> unit) -> Ast_c.expression -> unit

val apply_on_initialisation:
  (Ast_c.expression -> Ast_c.assignOp -> Ast_c.expression -> unit) ->
  Ast_c.onedecl -> unit

val which_is_the_error_branch:
  (Ast_c.expression -> value) ->
  (branch_side -> unit) -> Ast_c.expression -> unit
