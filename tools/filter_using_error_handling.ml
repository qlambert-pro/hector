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

type line_data =
  {name: string;
   file: string;
   f_name: string;
   line_number: string;
  }

let print_line_data d =
  Printf.printf "%s,%s,%s,%s\n%!" d.name d.file d.f_name d.line_number

type pair_data =
  {allocation: line_data;
   release:    line_data;
  }

let print d =
  print_line_data d.allocation;
  print_line_data d.release;
  Printf.printf "\n%!"

let parse =
  function
    [bl1;bl2] ->
    let wl1 = Str.split (Str.regexp ",") bl1 in
    let wl2 = Str.split (Str.regexp ",") bl2 in
    (match wl1, wl2 with
       [n1;f1;fn1;l1], [n2;f2;fn2;l2] ->
       {allocation = {
           name = n1;
           file = f1;
           f_name = fn1;
           line_number = l1;
         };
        release = {
          name = n2;
          file = f2;
          f_name = fn2;
          line_number = l2;
        };
       }
     | _ -> failwith "Misformed data lines")

  | _ -> failwith "Misformed data file"

let read_file filename =
  let input_line' chan =
    try Some (input_line chan) with End_of_file -> None in
  let chan = open_in filename in
  let rec read_until_blank_line chan acc =
    match input_line' chan with
      None      -> (true , acc)
    | Some ""   -> (false, acc)
    | Some line -> read_until_blank_line chan (line::acc)
  in
  let rec read_commits chan commit_stats =
    let (is_finished,  block) = read_until_blank_line chan [] in
    if is_finished
    then
      commit_stats
    else
      let data = parse (List.rev block) in
      read_commits chan (data::commit_stats)
  in
  let commit_stats = read_commits chan [] in
  close_in chan;
  commit_stats

let is_function_name n = function
    Ast_c.Definition def' ->
    let def = Ast_c.unwrap def' in
    (match def.Ast_c.f_name with
       Ast_c.RegularName (name, _) ->  name = n
     | _ -> false)
  | _ -> false


let statement_info_visitor f = {
  Visitor_c.default_visitor_c with
  Visitor_c.kexpr = (fun (k, visitor) (_, i)-> f i);
}


let is_error_handling cfg line_number =
  let ln = int_of_string line_number in
  let nodes =
    Graph_operations.find_all (fun (_, n) -> n.Annotated_cfg.is_error_handling)
      cfg
  in
  let is_found = ref false in
  let v = statement_info_visitor
      (fun i ->
         if i <> []
         then
           let (_, _, (l1,_), (l2,_)) = Lib_parsing_c.lin_col_by_pos i in
           is_found := !is_found || (l1 <= ln && l2 >= ln)
         else 
           ())
  in
  List.exists
    (fun (i, n) ->
       Visitor_c.vk_node v n.Annotated_cfg.parser_node; !is_found)
    nodes

let is_relevant_data path d =
  let short_filename = d.allocation.file in
  let filename =
    path ^ (String.sub short_filename 2 ((String.length short_filename)-2))
  in
  let (program, _) = Parse_c.parse_c_and_cpp false filename in
  let (functions', _) = Common.unzip program in
  let functions'' = List.tl (List.rev functions') in
  Flag_parsing_c.verbose_type := false;
  let functions =
    List.map fst
      (Type_annoter_c.annotate_program !Type_annoter_c.initial_env functions'')
  in
  let f = List.find (is_function_name d.allocation.f_name) functions in
  let cfg =
    try Some (Annotated_cfg.of_ast_c f) with
      Control_flow_c_build.Error _
    | Annotated_cfg.NoCFG -> None
  in
  match cfg with
    Some cfg ->
    let alloc = not (is_error_handling cfg d.allocation.line_number) in
    let release = is_error_handling cfg d.release.line_number in
    alloc && release
  | None -> false

(*first arg is the data presented as pairs*)
let data' = read_file Sys.argv.(1)
(*second arg is the path to the linux repo ended with /*)
let path = Sys.argv.(2)

let data = List.find_all (is_relevant_data path) data'

let _ = List.iter print (List.rev data)
