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

open Common

module Asto = Ast_operations
module HC = Hector_core

let analyze_file filename =
  let (program, _) = Parse_c.parse_c_and_cpp false false filename in
  let (functions', _) = Common.unzip program in
  let functions'' = List.tl (List.rev functions') in
  let functions =
    List.map fst
      (Type_annoter_c.annotate_program !Type_annoter_c.initial_env functions'')
  in
  let releases = List.fold_left
      (fun acc f ->
         if Analyzer.is_release f
         then Asto.StringSet.add (Asto.get_name f) acc
         else acc) Asto.StringSet.empty functions
  in
  HC.set_local_releases releases;
  List.iter Analyzer.analyze_toplevel functions


let verbose = ref false
let config_dir = ref ((Sys.getenv "HOME") ^ "/.hector.d")
let macros = ref ""

let options = [
  (* if you want command line arguments, put them here:
     "option name", operation described in man Arg, "description"; *)
  "-verbose", Arg.Set verbose,
  "  verbose output";
  "--config_dir", Arg.Set_string config_dir,
  "  Specify the directory where to look for config files. The default value is ~/.hector.d";
  "--macro_file_builtins", Arg.Set_string macros,
  "  macro definition file";
]


let get_files file_argument =
  if Sys.is_directory file_argument
  then Common.cmd_to_list ("find "^ file_argument ^" -name \"*.[ch]\"")
  else [file_argument]


let _ =
  let file = ref "" in
  let anonymous str = file := str in

  Arg.parse options anonymous "";

  if !macros = "" then macros := !config_dir ^ "/standard.h";

  Parse_c.init_defs_builtins !macros;
  Common.print_to_stderr := !verbose;
  Flag_parsing_c.verbose_lexing := !verbose;
  Flag_parsing_c.verbose_parsing := !verbose;
  Flag_parsing_c.verbose_type := !verbose;

  let files = get_files !file in
  let configs = Configs.get !config_dir in

  Asto.set_error_constants     configs.Configs.error_constants;
  Asto.set_testing_functions   configs.Configs.testing_functions;
  Asto.set_assigning_functions configs.Configs.assigning_functions;
  Asto.set_contained_fields    configs.Configs.contained_fields;

  List.iter
    (function x ->
     try Common.timeout_function "60" 60
           (function () -> analyze_file x)
     with _ -> ())
    files
