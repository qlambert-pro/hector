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

module Asto = Ast_operations

type t = {
  error_constants:     Asto.StringSet.t;
  testing_functions:   Asto.StringSet.t;
  non_allocations:      Asto.StringSet.t;
  assigning_functions: (int * int) Asto.StringMap.t;
  contained_fields:    Asto.StringPairSet.t;
}

let read filepath read_line add init =

  let input_line' chan = try Some (input_line chan) with End_of_file -> None in
  let chan = open_in filepath in

  let rec lines chan acc =
    match input_line' chan with
      None      -> acc
    | Some line -> lines chan (add (read_line line) acc)
  in

  let set = lines chan init in
  close_in chan;
  set

let read_1_word_config filepath =
  read filepath (fun x -> x) Asto.StringSet.add Asto.StringSet.empty

let read_2_word_config filepath =
  read filepath
    (fun s ->
       let words = Str.split (Str.regexp " ") s in
       (List.nth words 0, List.nth words 1))
    Asto.StringPairSet.add Asto.StringPairSet.empty

let read_1_word_and_2_ints filepath =
  read filepath
    (fun s ->
       let words = Str.split (Str.regexp " ") s in
       (List.nth words 0,
        (int_of_string (List.nth words 1), int_of_string (List.nth words 2))))
    (fun (k, v) m -> Asto.StringMap.add k v m) Asto.StringMap.empty

let get directory =
  let empty_config =
    {error_constants     =     Asto.StringSet.empty;
     testing_functions   =     Asto.StringSet.empty;
     non_allocations     =     Asto.StringSet.empty;
     assigning_functions =     Asto.StringMap.empty;
     contained_fields    = Asto.StringPairSet.empty;
    }
  in

  if Sys.file_exists directory &&
     Sys.is_directory directory
  then
    let error_constants_filepath     = directory ^ "/error_constants"     in
    let testing_functions_filepath   = directory ^ "/testing_functions"   in
    let non_allocations_filepath      = directory ^ "/non_allocations"      in
    let assigning_functions_filepath = directory ^ "/assigning_functions" in
    let contained_fields_filepath    = directory ^ "/contained_fields"    in

    let config' =
      if Sys.file_exists error_constants_filepath
      then {empty_config with
            error_constants =
              read_1_word_config error_constants_filepath}
      else empty_config
    in

    let config'' =
      if Sys.file_exists testing_functions_filepath
      then {config' with
            testing_functions =
              read_1_word_config testing_functions_filepath}
      else config'
    in

    let config''' =
      if Sys.file_exists non_allocations_filepath
      then {config'' with
            non_allocations =
              read_1_word_config non_allocations_filepath}
      else config''
    in
    let config'''' =
      if Sys.file_exists assigning_functions_filepath
      then
        {config''' with
            assigning_functions =
              read_1_word_and_2_ints assigning_functions_filepath}
      else config'''
    in

    if Sys.file_exists contained_fields_filepath
    then {config'''' with
          contained_fields = read_2_word_config contained_fields_filepath}
    else config''''
  else
    begin
      Printf.eprintf "warning: Empty config. Directory \"%s\" does not exists.\n%!"
        directory;
      empty_config
    end
