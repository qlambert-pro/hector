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

type oldblock = Block of int * Ast_c.expression *
                      Ast_c.statement option * Ast_c.statement list *
                      Def.block_part * int * int * Ast_c.statement list
              | BlockSimple of int * Ast_c.statement list


type block = {
  node: (Ograph_extended.nodei * Annotated_cfg.node) option;
  oldblock: oldblock;
}

val mk_block:
  (Ograph_extended.nodei * Annotated_cfg.node) ->
  int -> Ast_c.expression -> Ast_c.statement option ->
  Ast_c.statement list -> Def.block_part -> int -> int ->
  Ast_c.statement list -> block

val mk_block_simple:
  int -> Ast_c.statement list -> block

(* TODO probably not satisfying *)
val extract_statements: block -> Ast_c.statement list
val extract_branch_start: block -> int
val extract_test_case: block -> Ast_c.expression
val extract_index: block -> Ograph_extended.nodei

val compare_branch_start: block -> block -> int
val compare_branch_end_with_start: block -> block -> int

val is_error_handling_block: (int -> Ast_c.expression -> bool) -> block -> bool

val get_returned_expression: block -> Ast_c.statement option

val expression_used_as_argument: block -> Ast_c.expression -> bool
val does_block_contains_statement: block -> Ast_c.statement  -> bool
val contains_expression: block -> Ast_c.expression -> bool

val has_goto: block -> bool

(* TODO clean that *)
val get_resource_release:
  block ->
  (Resource.release * block) list ->
  (Resource.release * block) list

val find_all_resource_release_without_argument:
  block -> Ast_c.statement list

val return_st_access_resource:
  Resource.resource ->
  block ->
  bool
