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

val is_error : Ast_c.expression -> bool option
val expression_equal : Ast_c.expression -> Ast_c.expression -> bool

(* Side Effects *)
val apply_on_error_assignment:
  (Ast_c.expression -> Ast_c.expression option -> unit) -> Ast_c.expression ->
  unit

val apply_on_assignment:
  (Ast_c.expression -> Ast_c.expression option -> unit) -> Ast_c.expression ->
  unit

val apply_on_error_initialisation:
  (Ast_c.expression -> Ast_c.expression option -> unit) -> Ast_c.onedecl ->
  unit

val apply_on_initialisation:
  (Ast_c.expression -> Ast_c.expression option -> unit) -> Ast_c.onedecl ->
  unit

val which_is_the_error_branch:
  (branch_side -> unit) -> Ast_c.expression -> unit

val is_testing_identifier:
  Ast_c.expression -> Ast_c.expression -> bool