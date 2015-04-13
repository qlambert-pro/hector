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


let bug_info = Hashtbl.create(101)

let parse_org file =
  let i = open_in file in
  let rec loop _ =
    let l = input_line i in
    match Str.split (Str.regexp_string "BUG") l with
      ["* ";rest] ->
	(match Str.split (Str.regexp_string ":") rest with
	  _::file::_::line::_::rest ->
	    (match Str.split (Str.regexp_string "=") line with
	      [_;no] -> Hashtbl.add bug_info (file,int_of_string no) ()
	    | _ -> ())
	| _ -> ())
    | _ -> () in
  try loop () with End_of_file -> ()

let check_org file line =
  try Hashtbl.find bug_info (file,line); true
  with Not_found -> false

