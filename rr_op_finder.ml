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

open Common

(*why is release used as alloc ?*)
let find_missing_rr_ops_new c_function errblk rr_ops_list =
  let updated_errblk_with_goto_code =
    C_function.gather_goto_code_from_block c_function errblk in
  if updated_errblk_with_goto_code = []
  then []
  else
    let rec find_missing_rr_ops_inner = function
        [] -> []
      | (Resource.Release (args, h))::t ->
        if not (Def.stmt_exists_in_list h updated_errblk_with_goto_code)
        then
          (Resource.Resource (h, args, h))::(find_missing_rr_ops_inner t)
        else
          find_missing_rr_ops_inner  t
    in find_missing_rr_ops_inner rr_ops_list
