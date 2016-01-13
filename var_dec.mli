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

val glb_exeHash: (int, Ast_c.statement list) Hashtbl.t
val ehc_Hash: (int, int) Hashtbl.t

val complex: bool ref

val all_ehc: int ref
val all_real_ehc: int ref
val all_skip_ehc: int ref
val func_rtrn_ehc: int ref
val zero_rtrn_ehc: int ref
val oth_rtrn_ehc: int ref
val no_rtrn_ehc: int ref
val num_rtrn_ehc: int ref

val func_name: string ref
val file_info: string ref


val after_mender_condition: int ref
val return_after_partition: int ref
val perfect_function: int ref
val total_perfect_function: int ref
val unorder_count: int ref
val total_unorder_count: int ref
val return_after_filter: int ref
val single_if: int ref


val hardest_simple: int ref
val hardest_harder: int ref
val hardest_hard: int ref


val simple_count: int ref
val hard_count: int ref
val harder_count: int ref
val hardest_count: int ref



val line_reduced: int ref
val line_added: int ref
val added_goto: int ref
val assignment: int ref
val no_ass: int ref
val label_added: int ref
val line_moved: int ref

val harder_function: int ref
val no_harder_hardest: int ref
val single_harder: int ref
val multi_harder: int ref
val hardest_function: int ref
val hardest_function_total: int ref
val if_branch_applied: int ref
val total_if_branch_applied: int ref

val simple_function: int ref
val hard_function: int ref
