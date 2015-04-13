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


let rec insert_exp_into_ref_list e  re e2 new_ref_list = function
     []-> (new_ref_list@[(e2,[e])]@[(e2,[re])])
  |  (a,b)::t -> if(Def.compare_exps a e2) then  (new_ref_list@[(a,(b@[e]))]@[(e2,[re])]@t)
                 else insert_exp_into_ref_list e re e2 (new_ref_list@[(a,b)]) t



let rec insert_exp_into_ref_list_loop new_ref_list  e1 ref_list = function
   []->new_ref_list
  | h::t-> 
           let new_ref_list = insert_exp_into_ref_list e1 e1 h [] ref_list in
           insert_exp_into_ref_list_loop new_ref_list  e1 new_ref_list t


let rec gather_all_ref_var ref_list = function
   []-> ref_list
  | h::t-> match Ast_c.unwrap h with
           | Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.RecordAccess (e, name)), typ1), ii1), op,
                                                                (((Ast_c.Ident (ident)), typ2), ii2) )), typ), ii)) ->

                        let new_ref_list = insert_exp_into_ref_list e (((Ast_c.RecordAccess (e, name)), typ1), ii1)
                                          (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in
                        gather_all_ref_var new_ref_list t

           | Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.Ident (ident)), typ2), ii2) , op,
                                                                (((Ast_c.RecordAccess (e, name)), typ1), ii1))), typ), ii)) ->
                        let new_ref_list = insert_exp_into_ref_list e (((Ast_c.RecordAccess (e, name)), typ1), ii1)
                                          (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in
                        gather_all_ref_var new_ref_list t

           | Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.RecordPtAccess (e, name)), typ1), ii1), op,
                                                                (((Ast_c.Ident (ident)), typ2), ii2) )), typ), ii)) ->
                        let new_ref_list = insert_exp_into_ref_list e  (((Ast_c.RecordPtAccess (e, name)), typ1), ii1)
                                          (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in

                        gather_all_ref_var new_ref_list t
           | Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ1), ii1))), typ), ii)) ->
	                let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
			let new_ref_list = insert_exp_into_ref_list_loop [] e1 ref_list args_list in
	       gather_all_ref_var new_ref_list t
           | Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.Ident (ident)), typ2), ii2), op,
                                                                (((Ast_c.RecordPtAccess (e, name)), typ1), ii1))), typ), ii) ) ->
			let new_ref_list = insert_exp_into_ref_list e  (((Ast_c.RecordPtAccess (e, name)), typ1), ii1)
                                          (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in
                        gather_all_ref_var new_ref_list t
           | _-> gather_all_ref_var ref_list t


