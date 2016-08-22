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

module ACFG = Annotated_cfg
module Asto = Ast_operations

let configs = Configs.get ((Sys.getenv "PWD") ^ "/configs")

let _ =
  Asto.set_error_constants   configs.Configs.error_constants;
  Asto.set_testing_functions configs.Configs.testing_functions

let cpt = ref 0

let get_cfgs filename =
  let (program, _) = Parse_c.parse_c_and_cpp false filename in
  let (functions', _) = Common.unzip program in
  let functions'' = List.tl (List.rev functions') in
  let functions =
    List.map fst
      (Type_annoter_c.annotate_program !Type_annoter_c.initial_env
         functions'');
  in
  let cfgs =
    List.map (fun f ->
        try
          let cfg = Annotated_cfg.of_ast_c f in
          Hector_core.annotate_error_handling cfg;
          Hector_core.annotate_resource_handling cfg;
          Some cfg
        with
          Control_flow_c_build.Error _
        | ACFG.NoCFG -> None)
      functions
  in
  List.fold_left
    (fun acc o -> match o with Some g -> g::acc | _ -> acc) [] cfgs

let border_color_of node =
  match (node.ACFG.is_error_handling, node.ACFG.resource_handling_type) with
    (   _, ACFG.Allocation  _) -> Some("forestgreen")
  | (   _, ACFG.Assignment  _) -> Some("orange")
  | (   _, ACFG.Release     _) -> Some("red3")
  | (   _, ACFG.Computation _) -> Some("yellow")
  | (   _, ACFG.Test        _) -> Some("purple")
  | (true,                  _) -> Some("blue")
  | _                         -> None

let color_of node =
  match (node.ACFG.is_error_handling, node.ACFG.resource_handling_type) with
    (   _, ACFG.Allocation  _) -> Some("darkolivegreen1")
  | (   _, ACFG.Assignment  _) -> Some("peachpuff")
  | (   _, ACFG.Release     _) -> Some("lightpink")
  | (   _, ACFG.Computation _) -> Some("lightyellow")
  | (   _, ACFG.Test        _) -> Some("violet")
  | (true,                  _) -> Some("lightblue")
  | _                         -> None

let print_graph filename g =
  Ograph_extended.print_ograph_mutable_generic g
    None
    (fun (key, node) ->
       (Cocci_addon.stringify_CFG_node node.ACFG.parser_node,
        border_color_of node, color_of node))
    (filename ^ "_" ^ string_of_int !cpt)
    false;
  cpt := !cpt + 1

(*first arg is the data presented as pairs*)
let filename = Sys.argv.(1)
let output   = Sys.argv.(2)

let () =
  Parse_c.init_defs_builtins "/usr/local/lib/coccinelle/standard.h";
  Flag_parsing_c.verbose_type := false;
  Flag_parsing_c.verbose_lexing := false;
  Flag_parsing_c.show_parsing_error := false;
  Flag_parsing_c.verbose_parsing := false


let cfgs = get_cfgs filename

let _ = List.iter (print_graph output) cfgs
