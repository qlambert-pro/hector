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

(* Find resource releasing operations from error handling block *)


let rec exp_exists_in_stmtlist exp = function
  []-> false
  | h::t-> match Ast_c.unwrap h with
            Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
	      if(Def.exp_exists_in_stmt exp h) then true
              else exp_exists_in_stmtlist exp t
            
            |_-> exp_exists_in_stmtlist exp t 

let rec no_exp_exists_in_stmtlist list  = function
  []-> true
  | h::t-> if (exp_exists_in_stmtlist (Some h) list) then false
           else no_exp_exists_in_stmtlist list t


let rec exp_exists_in_stmtlist_new exp = function
  []-> false
  |  h::t-> match Ast_c.unwrap h with
               Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
		 let args_list = (Def.remove_optionlist (Def.create_argslist [] es)) in
		 if(Def.exp_exists_in_list exp args_list) then true
		 else exp_exists_in_stmtlist_new exp t
              |_-> exp_exists_in_stmtlist_new exp t 

let rec all_exp_exists_in_list list = function
  []-> true
  |  h::t-> if (exp_exists_in_stmtlist_new h list) then
               all_exp_exists_in_list list t
            else false       

let rec unused_ptr_args list = function
   []-> []
  | h::t-> if (exp_exists_in_stmtlist_new h list) then
               unused_ptr_args list t
           else h::(unused_ptr_args list t)


let rec find_ptr_args_list = function
  []->[]
  | h::t->match  h with
           (((Ast_c.Cast    (cast, e)), typ), ii)-> 

                                                 (match (Def.is_pointer e) with
						   Def.IsPtr->
						     e::find_ptr_args_list t
						 | Def.UnknownType->
						     e::find_ptr_args_list t
						 | _-> find_ptr_args_list t
                                                 )
         |_-> (match (Def.is_pointer h) with
                 Def.IsPtr->h::find_ptr_args_list t
               | Def.UnknownType->h::find_ptr_args_list t
               | _-> find_ptr_args_list t
	     )




let rec stmt_exists_in_list st = function
    []-> false
  | (args,h)::t -> if(Def.compare_stmts st  h) then true
                   else stmt_exists_in_list st t


let rec create_argslist args_list = function
     []-> args_list
  |   h::t-> (match Ast_c.unwrap h with
             Common.Left es -> (match  es with
                                 |  ((exp, typ), ii) ->create_argslist (args_list@[Some ((exp, typ), ii)]) t
                                 |  _ -> create_argslist args_list t )
                                 |  _ -> create_argslist args_list t
            )


let args_contains_array = function
  (((Ast_c.ArrayAccess    (e1, e2)), typ), ii) -> true
  | _-> false

let rec remove_int = function
  []-> []
  | h::t-> 
          match (Def.is_pointer h) with
            Def.IsntPtr-> (remove_int t)
          | _-> h::(remove_int t)


let first_arg_is_array args_list = (* added by julia *)
  match remove_int args_list with
    [] -> false
  | h::t -> args_contains_array h

let rec stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list top stmtlist = function
  []->rr_ops_list
  | h::t->
      match Ast_c.unwrap h with
        Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName (fn_n, [ii3]))), typ1), ii1), es)), typ), ii)) ->
	  let args_list = (Def.remove_optionlist (create_argslist [] es)) in
	  if (List.length args_list) = 0 then
	    if (not(stmt_exists_in_list h rr_ops_list)) then
	      stack_rr_op_new func_name iifunc1 lbl_list (([],h)::rr_ops_list) top stmtlist t
	    else stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list (h::top) stmtlist t
	  else if first_arg_is_array args_list then 
	    stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list (h::top) stmtlist t
	  else if (Def.string_exists_in_explist args_list) then
	    stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list (h::top) stmtlist t  
          else if (stmt_exists_in_list h rr_ops_list) then
	    stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list (h::top) stmtlist t
	  else (
	    let ptr_args_list = find_ptr_args_list args_list in
            if (List.length ptr_args_list)=0 then (
	      stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list (h::top) stmtlist t)
	    else if (all_exp_exists_in_list t ptr_args_list)  then
	      stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list (h::top) stmtlist t
            else  ( let unused_args = unused_ptr_args t ptr_args_list in

            if(no_exp_exists_in_stmtlist t unused_args) (*&& (not(Def.stmt_exists_in_list h top))*) then(
	      stack_rr_op_new func_name iifunc1 lbl_list ((unused_args,h)::rr_ops_list) top stmtlist t)
            else (stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list (h::top) stmtlist t)
		)
		)
      |_-> stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list (h::top) stmtlist t




(* Find Missing resource releasing operations from error handling block *)



let find_missing_rr_ops_new lbl_list errblk rr_ops_list =
        let updated_errblk_with_goto_code = Rm_true_positives.gather_goto_code lbl_list [] errblk  in
           if(List.length updated_errblk_with_goto_code) =0 then []
           else let rec find_missing_rr_ops_inner = function
                []->[]
             |  (args,h)::t->
		 if (not(Def.stmt_exists_in_list h updated_errblk_with_goto_code)) then
                                   (h,args,h)::(find_missing_rr_ops_inner t)
                 else find_missing_rr_ops_inner  t
                in find_missing_rr_ops_inner rr_ops_list


