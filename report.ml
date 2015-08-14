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




(******************************  Begin of testing Error-Handling Blocks *************************************)

let rec testing_ehc func_name = function
    []-> ()
  |   (brnch_strtlineno, test_case, goto, st_normal, typ, blk_strtlineno, blk_endlineno, stmtlist)::t->
    print_string(func_name);
    print_string(" ");
    print_string(string_of_int(blk_strtlineno));
    print_string("\n");
    testing_ehc func_name t


(****************************** End of testing Error-HandlingBlocks *************************************)



let rec generate_report_new func_name block iifunc1 = function
    []-> false
  |  (alloc,args,h)::t-> match Ast_c.unwrap h with
    |   Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName (id, [ii2]))), typ1), ii1), es)), typ), ii)) ->

      let ao = match Ast_c.unwrap alloc with
          Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName (id10, [ii12]))), typ11), ii11), es10)), typ10), ii10)) -> id10
        | _-> " " in
      let st_lineno = Def.find_startline_no [h] in
      let filename = Ast_c.file_of_info iifunc1 in
      let line_no = Block.extract_branch_start block in

      let ro = id in
      print_string("* TODO ");
      print_string(" [[view:");
      print_string(filename);
      print_string("::face=ovl-face1::linb=");
      print_string(string_of_int(line_no));
      print_string("::colb=");
      print_string(string_of_int(0));
      print_string("::cole=");
      print_string(string_of_int(0));
      print_string("][");
      print_string("AO:");
      print_string(ao);
      print_string("  ");
      print_string("RO:");
      print_string(ro);
      print_string(" ");
      print_string("*FN:");
      print_string(func_name);
      print_string(" ");
      print_string(filename);
      print_string("::");
      print_string(string_of_int(line_no));
      print_string("]]");
      print_string("\n");

      print_string("** [[view:");
      print_string(filename);
      print_string("::face=ovl-face2::linb=");
      print_string(string_of_int(st_lineno));
      print_string("::colb=");
      print_string(string_of_int(0));
      print_string("::cole=");
      print_string(string_of_int(0));
      print_string("][");
      print_string("AO:");
      print_string(ao);
      print_string("  ");

      print_string("RO:");
      print_string(ro);
      print_string(" ");
      print_string("*FN:");
      print_string(func_name);
      print_string(" ");

      print_string(filename);
      print_string("::");
      print_string(string_of_int(st_lineno));
      print_string("]]");
      print_string("\n");
      generate_report_new func_name block iifunc1 t
    |  _-> generate_report_new func_name block iifunc1 t
