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

type c_function

val mk_c_function: Ast_c.toplevel -> Ast_c.statement list -> c_function

val find_interproc_calls:
  (Ast_c.name * 'a * Ast_c.statement list) list ->
  Block.block ->
  c_function ->
  Resource.resource list ->
  Resource.resource list

(* Return a list of error handling blocks *)
val find_errorhandling:
  c_function -> Block.block list

val is_resource_allocated_properly:
  Block.block -> Block.block list -> c_function -> Resource.resource -> bool

val is_resource_released:
  Block.block -> Block.block list -> c_function -> Resource.resource -> bool

(* **
 * Return true if the allocation was the last time the released resource was
 * used.
 * *)
val resource_of_release_temp:
  Block.block -> Block.block list -> c_function -> Resource.resource -> bool

val get_resources:
  c_function -> Block.block list -> Resource.resource list
