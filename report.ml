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

module CF = C_function

let generate_report_new func_name infos f =
  let ao = f.CF.exemplar.CF.alloc_name in
  let ro = f.CF.exemplar.CF.release_name in
  let filename = Ast_c.file_of_info infos in

  let st_lineno =
    Annotated_cfg.line_number_of_node f.CF.exemplar.CF.release
  in
  let line_no =
    Annotated_cfg.line_number_of_node f.CF.block_head
  in

  Printf.printf
    ("* TODO  [[view:%s::face=ovl-face1::linb=%d::colb=%d::cole=%d][AO:%s RO:%s *FN:%s %s::%d]]\n")
    filename line_no 0 0 ao ro func_name filename line_no;

  Printf.printf
    ("** [[view:%s::face=ovl-face2::linb=%d::colb=%d::cole=%d][AO:%s RO:%s *FN:%s %s::%d]]\n")
    filename st_lineno 0 0 ao ro func_name filename st_lineno
