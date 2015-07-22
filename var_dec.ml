(*
 * Copyright 2013, Inria
 * Suman Saha, Julia Lawall, Gilles Muller
 * This file is part of Hector.
 *
 * Hector is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Hector is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Hector.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Hector under other licenses.
 *)

let glb_exeHash = Hashtbl.create 123456
let ehc_Hash = Hashtbl.create 123456

let complex = ref false

let all_ehc = ref 0
let all_real_ehc = ref 0
let all_skip_ehc = ref 0
let func_rtrn_ehc = ref 0
let zero_rtrn_ehc = ref 0
let oth_rtrn_ehc = ref 0
let no_rtrn_ehc = ref 0
let num_rtrn_ehc = ref 0


let func_name = ref "0"
let file_info = ref "s"



let after_mender_condition = ref 0
let return_after_partition = ref 0
let perfect_function = ref 0
let total_perfect_function = ref 0
let unorder_count = ref 0
let total_unorder_count = ref 0
let return_after_filter = ref 0
let single_if = ref 0
let harder_function = ref 0
let no_harder_hardest = ref 0
let single_harder = ref 0
let no_harder_hardest = ref 0
let single_harder = ref 0
let multi_harder = ref 0
let hardest_function = ref 0
let hardest_function_total = ref 0
let if_branch_applied = ref 0
let total_if_branch_applied = ref 0
let simple_function = ref 0
let hard_function = ref 0


let simple_count = ref 0
let hard_count = ref 0
let harder_count = ref 0
let hardest_count = ref 0

let hardest_simple = ref 0
let hardest_harder = ref 0
let hardest_hard = ref 0



let line_reduced = ref 0
let line_added = ref 0
let added_goto = ref 0
let assignment = ref 0
let no_ass = ref 0
let label_added = ref 0
let line_moved  = ref 0
