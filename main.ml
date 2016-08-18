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

let test_type_c infile =
  let (program2, _stat) = Parse_c.parse_c_and_cpp false infile in
  let program2' =
    program2
    +> Common.unzip
    +> (fun (program, infos) ->
        Type_annoter_c.annotate_program !Type_annoter_c.initial_env
          program +> List.map fst,
        infos
      )
    +> Common.uncurry Common.zip
  in
  let (program, infos) = Common.unzip program2' in

  let rec loop = function
      []-> []
    | h::t->
      (Analyzer.analyze_toplevel h)::(loop t)

  in loop program

(*  List.map (Analyzer.analyze_toplevel program) program*)




let pretty_print_to_file new_code op =
  let (fl,o) = Filename.open_temp_file "error" ".c" in
  Format.set_formatter_out_channel o;
  List.iter
    (function x -> Pretty_print_c.pp_toplevel_simple x; Format.print_newline())
    new_code;
  Format.set_formatter_out_channel stdout;
  close_out o;
  Common.command2 ("indent -linux "^fl);
  op fl;
  Sys.remove fl

let compare old_file new_code cmp =
  pretty_print_to_file new_code
    (function fl ->
       let tmp_old = Filename.temp_file "old_error" "c" in
       Common.command2
         (Printf.sprintf "cp %s %s; indent -linux %s" old_file tmp_old tmp_old);
       let (run,show) = cmp tmp_old fl in
       match Common.cmd_to_list run with
         [] -> ()
       | lst -> List.iter (function x -> Printf.printf "%s\n" x) (show::lst))

let compare_with_indented old_file new_code = ()
(*  compare old_file new_code *)
(*    (fun tmp_old fl -> *)
(*      ((Printf.sprintf "diff -u -p %s %s" tmp_old fl), *)
(*       (Printf.sprintf "diff -u -p %s %s" old_file fl))) *)

let compare_with_original old_file new_code =
  compare old_file new_code
    (fun tmp_old fl ->
       ((Printf.sprintf
           "diff -q %s %s > /dev/null || diff -u -p %s %s"
           tmp_old fl old_file fl),
        (Printf.sprintf "diff -u -p %s %s" old_file fl)))

let cat new_code = ()
(*  pretty_print_to_file new_code *)
(*   (function fl -> Common.command2 (Printf.sprintf "cat %s" fl)) *)

let diff = ref false
let real_diff = ref false
let org_file = ref (None : string option)
let verbose = ref false
let macros = ref ""
let profile = ref false

let options = [
  (* if you want command line arguments, put them here:
     "option name", operation described in man Arg, "description"; *)
  "--profile", Arg.Unit (function () -> Common.profile := Common.PALL;
      profile := true),
  "   gather timing information about the main coccinelle functions";
  "-diff", Arg.Set diff,
  "  diff output"; (* diff after indent *)
  "-real_diff", Arg.Set real_diff,
  "  diff output against original"; (* diff before indent *)
  "-org", Arg.String (function s -> org_file := Some s),
  "  org file containing bug reports and false positives";
  "-verbose", Arg.Set verbose,
  "  verbose output";
  "-macro_file_builtins", Arg.Set_string macros,
  "  macro definition file";
]

let file = ref ""

let anonymous str = file := str

let _ =
  Arg.parse options anonymous "";
  if !macros = "" then macros := ("/usr/local/lib/coccinelle/standard.h");
  Parse_c.init_defs_builtins !macros;
  (match !org_file with Some x -> Org.parse_org x | _ -> ());
  Common.print_to_stderr := !verbose || !profile;
  Flag_parsing_c.verbose_lexing := !verbose;
  Flag_parsing_c.verbose_parsing := !verbose;
  Flag_parsing_c.verbose_type := !verbose;

  let files =
    if Sys.is_directory !file
    then Common.cmd_to_list ("find "^ !file ^" -name \"*.[ch]\"")
    else [!file]

  in
  Common.main_boilerplate
    (fun () -> List.iter (function x -> cat (
         try Common.timeout_function "60" 60 (function () -> test_type_c x)
         with _ -> []))
         files)
