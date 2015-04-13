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

let s = ref 0
let count_return_apply = ref 0 
let only_return = ref 0
let only_goto = ref 0
let goto_return = ref 0
let return_after_filter = ref 0
let return_after_partition = ref 0

let mark = ref 0
let return_help_count = ref 0

let simple_count = ref 0
let hard_count = ref 0 
let harder_count = ref 0
let hardest_count = ref 0

let line_reduced = ref 0
let line_added = ref 0
let added_goto = ref 0
let if_branch_applied = ref 0
let total_if_branch_applied = ref 0
let assignment = ref 0
let label_added = ref 0
let line_moved  = ref 0

let reg_dir = Str.regexp "linux-[0-9\\.]+/[a-z]+"
let reg_ver = Str.regexp "linux-[a-zA-Z0-9\\.]+"

let unorder_count = ref 0
let total_unorder_count = ref 0


let hardest_simple = ref 0
let hardest_harder = ref 0
let hardest_hard = ref 0

let single_if = ref 0

let partition_type = ref 0
let ver_name = ref "s"
let dir_name = ref "s"

let perfect_function = ref 0 
let total_perfect_function = ref 0

let path_list = ref []
let harder_list = ref []

let harder_simple = ref 0
let harder_hard = ref 0


let simple_function = ref 0
let hard_function = ref 0
let harder_function = ref 0
let hardest_function = ref 0
let no_harder_hardest = ref 0
let single_harder  = ref 0
let multi_harder = ref 0
let hardest_function_total = ref 0


let bug_count = ref 0
let unordr_rels_mark = ref 0
let x = ref 0
let if_branch_count = ref 0
let ehc_count = ref 0
let fixing = ref 0
let blk_no = ref 0
let goto_mark = ref 0 
let example_mark1 = ref 0
let example_mark2 = ref 0
let example_mark3 = ref 0
let example_mark4 = ref 0
let example_mark5 = ref 0
let all_def = ref []
let nolink  = ref 0
let nolinklist = ref []
let link    = ref 0
let linklist = ref []

let noerr  = ref 0
let noerrlist = ref []
let err    = ref 0
let errlist = ref []
let ret    = ref 0
let retlist = ref []
let noret  = ref 0
let noretlist = ref []

let nointerproc = ref 0
let nointerproclist = ref []
let interproc = ref 0
let interproclist = ref []
let same_arg_diff_name_list = ref []
let count_sadn1  = ref 0 
let count_sadn2  = ref 0
let count_sadn3  = ref 0
let count_sadn4  = ref 0
let count_sadn5  = ref 0
let count_sadn6  = ref 0
let count_sadn7  = ref 0
let count_sadn8  = ref 0
let count_sadn9  = ref 0
let count_sadn10  = ref 0
let count_sadn11  = ref 0

let test_count1 = ref 0
let test_count2 = ref 0
let test_count3 = ref 0
let test_count4 = ref 0
let test_count5 = ref 0
let test_count6 = ref 0
let test_count7 = ref 0
let test_count8 = ref 0
let test_count9 = ref 0
let test_count10 = ref 0
let test_count11 = ref 0
let test_count12 = ref 0
let test_count13 = ref 0
let test_count14 = ref 0
let test_count15 = ref 0
let test_count16 = ref 0
let test_count17 = ref 0
let test_count18 = ref 0
let test_count19 = ref 0
let test_count20 = ref 0
let test_count21 = ref 0
let test_count22 = ref 0
let test_count23 = ref 0
let test_count24 = ref 0
let test_count25 = ref 0
let test_count26 = ref 0
let test_count27 = ref 0
let test_count28 = ref 0
let test_count29 = ref 0

type result =
     Nothing 
  |   Simple
  |   Hard
  |   Harder
  |   Hardest

type status =
     Complete
  |  Incomplete

type stmtype =
     Pre_single
   | Pre_multiple
   | Post_single
   | Post_multiple

type scan_style =
     Forward
  |  Backward

(* Remove  Ast_c.StmtElem from statement list *)


let single_stmtEle_remove l=
          match l with
              Ast_c.StmtElem st -> st
         



let rec  stmtEle_remove l=
          match l with
           [] -> []
          |  h::t -> match h with
                    Ast_c.StmtElem st -> st::(stmtEle_remove t)
            |  _ -> stmtEle_remove t


(* Add  Ast_c.StmtElem into statement list *)

let stmtEle_add_list  l=
    List.map (function st -> Ast_c.StmtElem st ) l


(* Clean one statement  *)

let clean_statement st =  Lib_parsing_c.real_al_statement st


(* Clean all statements in the list *) 

let  rec clean_statement_list  = function
  []-> []
  |  h::t-> (clean_statement h)::(clean_statement_list t)




(* Clean expression *)

let clean_exp  exp =  Lib_parsing_c.al_expr exp 


let rec clean_exp_list = function
    []-> []
  |   h::t-> (clean_exp h)::(clean_exp_list t)

(* Clean arguments *)

let clean_arg  arg =  Lib_parsing_c.al_arguments arg



(* Clean all arguments in the list *)

let  rec clean_args  = function
  []-> []
  |   h::t-> (clean_arg h)::(clean_args t)



(* Compare two names *)

let compare_name name1 name2 =
   if (Lib_parsing_c.al_name name1) =  (Lib_parsing_c.al_name name2) then true
   else false  


(* Compare two statements *)

let compare_stmts st1 st2 =
   if (clean_statement st1) = (clean_statement st2) then true 
   else false


(* Compare two statement lists *)

let compare_stmts_list st1 st2 =
   if (clean_statement_list st1) = (clean_statement_list st2) then true
   else false 

(* Compare two expression *)

let compare_exp exp1 exp2 =
  if (clean_exp exp1) = (clean_exp exp2) then true
  else false

(* Create Identifier *)

let create_id_name name = Ast_c.RegularName(name,[])



(* Create Assignment statement *)

let create_assignment_st typ name exp =
  (Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.Ident (name)), typ), []), 
  Ast_c.SimpleAssign, 
  ((exp, typ), []))), typ), [])),[])


(* Create Return statement *)

let create_return_st typ name =
(Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (create_id_name name)), typ), [])),[])


(* Create Label statement *)

let create_label_st name st =
(Ast_c.Labeled (Ast_c.Label ((create_id_name name), st)),[])


(* Create Label statements *)

let create_label_sts name st_list =
(create_label_st name (List.hd st_list))::(List.tl st_list)


(* Create If statement *)

let create_if_st e  st2 lbl if_code  = 
 match if_code with
    [] ->  (Ast_c.Selection  (Ast_c.If (e, ((Ast_c.Compound (stmtEle_add_list (if_code))),[]), st2)),[])
  | h::t -> if (List.length (h::t)) =  1 then 
                    (Ast_c.Selection  (Ast_c.If (e, (List.hd if_code), st2)),[])
            else
                    (Ast_c.Selection  (Ast_c.If (e, ((Ast_c.Compound (stmtEle_add_list (if_code))),[]), st2)),[])


(* Create Compound statement *)

let create_compound_st st =
  match Ast_c.unwrap st with
      Ast_c.Compound  st -> (stmtEle_remove st)
  |  _ -> [st]


(* find maximum number with out (label). if there is no out or out with number then it will return 0 *)

let find_lbl_out_no prog = 
let n = ref 0 in
  let rec find_lbl_no_loop = function
  [] -> !n
    |  h::t->(match Ast_c.unwrap h with 
             Ast_c.Labeled (Ast_c.Label (Ast_c.RegularName(s,ii), st)) ->  
	                     (match  Str.split (Str.regexp "out") s with
	                        []->find_lbl_no_loop  t
	                      | [x] -> try  
				         if int_of_string (x)> !n then 
					      (n:=int_of_string (x); find_lbl_no_loop  t)
                                         else find_lbl_no_loop  t   
		                       with _ -> find_lbl_no_loop  t
			      | _ -> find_lbl_no_loop  t
	       |  _ -> find_lbl_no_loop  t)  
      |	   _ -> find_lbl_no_loop  t
      )
  in find_lbl_no_loop  prog



(* find starting line no of any statement *)

let find_startline_no st =
    match st with
      []-> 0
    | h::t->  let info = Lib_parsing_c.ii_of_stmt h in
                let (file,current_element,(start_line,_),(end_line,_))= Lib_parsing_c.lin_col_by_pos info in
                start_line


let find_endline_no st =
    match List.rev st with
      []-> 0
    | h::t->  let info = Lib_parsing_c.ii_of_stmt h in
              let (file,current_element,(start_line,_),(end_line,_))= Lib_parsing_c.lin_col_by_pos info in
              end_line


let find_lcol st =
    match st with
      []-> 0
    |  h::t->  let info = Lib_parsing_c.ii_of_stmt h in
                let (file,current_element,(start_line,lcol),(end_line,rcol))= Lib_parsing_c.lin_col_by_pos info in
                lcol


let find_rcol st =
    match List.rev st with
      []-> 0
    |   h::t->  let info = Lib_parsing_c.ii_of_stmt h in
                let (file,current_element,(start_line,lcol),(end_line,rcol))= Lib_parsing_c.lin_col_by_pos info in
                rcol


(* Check whether the statement has inner block *)

let has_inner_block st = 
  let l = (create_compound_st st) in
  let rec has_inner_block_loop = function
          []->  false
    |  h::t -> (match Ast_c.unwrap h with
      |	 Ast_c.Labeled    (Ast_c.Case  (e, st)) -> true
      |	 Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> true 
      |	 Ast_c.Selection  (Ast_c.Switch (e, st )) ->true
      |	 Ast_c.Iteration  (Ast_c.While (e, st)) -> true
      |	 Ast_c.Iteration  (Ast_c.DoWhile (st, e)) -> true
      |	 Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) -> true
      |	 Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> true
      |	 _ -> has_inner_block_loop t)
  in has_inner_block_loop l


let rec has_inner_block_new  = function
    []->  false
    |   h::t -> (match Ast_c.unwrap h with
       |	  Ast_c.Labeled    (Ast_c.Case  (e, st)) -> true
       |	  Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> true
       |	  Ast_c.Selection  (Ast_c.Switch (e, st )) ->true
       |	  Ast_c.Iteration  (Ast_c.While (e, st)) -> true
       |	  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) -> true
       |	  Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) -> true
       |	  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> true
       |	  _ -> has_inner_block_new t)




let is_return_id_in_expr e e1 =
 let flag = ref false in
 (Visitor_c.vk_expr
   { Visitor_c.default_visitor_c with
     Visitor_c.kexpr =
     (function (k,bigf) -> function exp -> k exp;
        if (compare_exp exp e1) then ((*Printf.printf "\n\n%s\n\n" (Dumper.dump exp);*)flag:= true) 
       else ((*Printf.printf "\n\n%s\n\n" (Dumper.dump exp);*) ())
       ) }
   e);
 !flag



(* Check whether the if branch has appropriate return , say return -ENOMEM or return ret(any variable) or return ERR_PTR(...)*)

let rec has_return = function
     [] -> false
  |  h::t ->( match Ast_c.unwrap h with
                Ast_c.Jump (Ast_c.ReturnExpr((((Ast_c.Unary((((Ast_c.Ident(Ast_c.RegularName (id, [ii1]))), typ1), ii2),
                      Ast_c.UnMinus)), typ2),ii3) )) ->  if id =~ "^[A-Z_][A-Z_0-9]*$" then true 
			                                 else false
              | Ast_c.Jump (Ast_c.ReturnExpr((((Ast_c.Unary((((Ast_c.Constant (c)), typ1), ii2),Ast_c.UnMinus)), typ2),ii3) )) -> true 
              | Ast_c. Jump (Ast_c.ReturnExpr (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(s,ii))), typ), ii1), es)), typ1), ii2)) -> 
		                                         if s =~ "ERR_PTR" || s =~ "PTR_ERR" then true else false
                                                            
              | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) ->
                                                if id =~ "^[a-z_][a-z_0-9]*$" then true
		                                else  if id =~ "NULL" then true
                                                else false                                                                                                                                      | _ -> has_return t
            )


let rec has_return_all  = function
     [] -> false
   | h::t ->( match Ast_c.unwrap h with
                Ast_c.Jump (Ast_c.ReturnExpr e)-> true
              | Ast_c.Jump (Ast_c.Return ) -> true 
              |  _ -> has_return_all t
            )


type ptr = IsPtr | IsntPtr | UnknownType

let is_pointer ((exp, info), ii) =
 match !info with
   (Some((_,ty),_), _) ->
     (match Ast_c.unwrap ty with
       Ast_c.Pointer _ -> IsPtr
     |   _ -> IsntPtr)
 |  (None,_)  -> UnknownType


let is_pointer_main ((exp, typ11), ii11) = 
   match (is_pointer ((exp, typ11), ii11)) with
     IsPtr -> true
   | _ -> false



let rec remove_integer = function
    []-> []
  | h::t-> (match is_pointer h with
               IsPtr-> h::(remove_integer t)
              | _ -> remove_integer t 
           )
let rec has_return_bug_new e = function
     [] -> false
  |  h::t ->( match Ast_c.unwrap h with
                Ast_c.Jump (Ast_c.ReturnExpr((((Ast_c.Unary((((Ast_c.Ident(Ast_c.RegularName (id, [ii1]))), typ1), ii2),
                      Ast_c.UnMinus)), typ2),ii3) )) ->  if id =~ "^[A-Z_][A-Z_0-9]*$" then true
                                                         else false
              | Ast_c.Jump (Ast_c.ReturnExpr((((Ast_c.Unary((((Ast_c.Constant (c)), typ1), ii2),Ast_c.UnMinus)), typ2),ii3) )) -> true
              | Ast_c. Jump (Ast_c.ReturnExpr (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(s,ii))), typ), ii1), es)), typ1), ii2)) ->
                                                         if s =~ "ERR_PTR" || s =~ "PTR_ERR" then true else false
	      |	Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) -> true

              |	_-> has_return_bug_new e t
            )


let rec make_argslist args_list = function
     []-> args_list
  |  h::t-> (match Ast_c.unwrap h with
               Common.Left es -> (match  es with
                                   ((exp, typ), ii) ->
                                           (make_argslist (args_list@[Some ((exp, typ), ii)]) t)
                                  | _ -> make_argslist args_list t )
              | _ -> make_argslist args_list t
            )


let rec remove_option = function
  []-> []
  |  (Some h)::t -> h::(remove_option t)


let rec st_exists_in_list func_name = function
   []-> false
  |   h::t -> if(compare_stmts func_name h ) then true else st_exists_in_list func_name t


let rec refine_code = function
  []-> []
  |  h::t-> (match Ast_c.unwrap h with
    |  Ast_c.ExprStatement (None) -> h::(refine_code t)
    |  Ast_c.ExprStatement (Some e) -> h::(refine_code t)
    |  Ast_c.Jump (Ast_c.Goto name) ->h::(refine_code t)
    |  Ast_c.Jump (Ast_c.Return)  -> h::(refine_code t)
    |  Ast_c.Jump (Ast_c.ReturnExpr e)  -> h::(refine_code t)
    |  _ -> refine_code t

    )

let rec code_for_goto name = function
    []->[]
  | (label,code)::t -> if (compare_name label name)
                          then  (if(has_inner_block_new code) then [] else code )
	               else code_for_goto name t



let rec collect_goto_code lbl_list stmt_list goto_list = function
   []-> None
  | h::t-> match Ast_c.unwrap h with
            Ast_c.Jump (Ast_c.Return) -> Some (stmt_list@[h])
          | Ast_c.Jump (Ast_c.ReturnExpr e) -> Some (stmt_list@[h])
          | Ast_c.Jump (Ast_c.Goto name) ->
             if (st_exists_in_list h goto_list) then None
             else
                collect_goto_code lbl_list stmt_list (goto_list@[h]) (refine_code (code_for_goto name lbl_list))
          | _ -> collect_goto_code lbl_list (stmt_list@[h]) goto_list t


let rec what_is_in_return1 if_exp lbl_list = function
    []-> false
 |  h::t-> (match Ast_c.unwrap h with
               Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Unary (e, Ast_c.UnMinus)), typ), ii)) ->
                     (match e with
                       (((Ast_c.Ident (Ast_c.RegularName(s,ii2))), typ1), ii1)->
                         if s =~ "^[A-Z_][A-Z_0-9]*$" then true
                         else false
                     |  (((Ast_c.Constant (c)), typ1), ii1)-> true
                     )
            |  Ast_c. Jump (Ast_c.ReturnExpr (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(s,ii2))), typ1), ii1), es)), typ), ii)) ->
                     if s =~ "ERR_PTR" || s =~ "PTR_ERR" then true
                     else false
            |  Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (Ast_c.RegularName(s,ii1))), typ), ii)) ->
                     if s =~ "NULL" then true
                     else if s =~ "^[A-Z_][A-Z_0-9]*$" then false
                     else true
            |   Ast_c.Jump (Ast_c.Goto name) -> true
            | _ ->what_is_in_return1 if_exp lbl_list t
            ) 


let rec what_is_in_return if_exp lbl_list = function
    []-> false
  | h::t-> (match Ast_c.unwrap h with
               Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Unary (e, Ast_c.UnMinus)), typ), ii)) ->
                     (match e with
		       (((Ast_c.Ident (Ast_c.RegularName(s,ii2))), typ1), ii1)->
			 if s =~ "^[A-Z_][A-Z_0-9]*$" then true
			 else false
                      | (((Ast_c.Constant (c)), typ1), ii1)-> true
                     )
             | Ast_c. Jump (Ast_c.ReturnExpr (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(s,ii2))), typ1), ii1), es)), typ), ii)) ->
                     if s =~ "ERR_PTR" || s =~ "PTR_ERR" then true 
                     else false
	     | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (Ast_c.RegularName(s,ii1))), typ), ii)) -> 
		     if s =~ "NULL" then true
		     else if s =~ "^[A-Z_][A-Z_0-9]*$" then false
                     else if (is_return_id_in_expr if_exp (((Ast_c.Ident (Ast_c.RegularName(s,ii1))), typ), ii)) then
		       match if_exp with 
			 (((Ast_c.Unary (((exp1, typ2), ii3), Ast_c.Not)), typ1), ii2)->
                             (match is_pointer ((exp1, typ2), ii3) with
                                IsPtr-> true
			      | _-> false
                             )
		       | (((Ast_c.Binary   (e1, Ast_c.Logical Ast_c.Eq, e2)), typ1), ii2)->
                             (match is_pointer e1  with
                                IsPtr-> true
                              | _-> false
                             )
                       |  (((Ast_c.Ident (ident)), typ1), ii2)->
                             (match is_pointer (((Ast_c.Ident (ident)), typ1), ii2)  with
                                IsntPtr-> true
                             |  _-> false
                             )
                       |   (((Ast_c.Binary   (e1, Ast_c.Logical Ast_c.NotEq, e2)), typ1), ii2)->
                             (match is_pointer e1  with
                                IsntPtr-> true
                             |  _-> false
                             )
                       | ((( Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(s,ii4))), typ2), ii3), es)), typ1), ii2)->
			     if s =~ "IS_ERR" && (List.length (make_argslist [] es) > 0) then 
			       if (is_return_id_in_expr (List.hd (remove_option (make_argslist [] es))) (((Ast_c.Ident (Ast_c.RegularName(s,ii1))), typ), ii)) then
			             (match is_pointer (List.hd (remove_option (make_argslist [] es)))  with
                                        IsPtr-> true
                                      | _-> false
                                     )
			       else true
                             else true
                     else false

	     |  Ast_c.Jump (Ast_c.Goto name) ->  
		 match (collect_goto_code lbl_list [] [] [h]) with
		   None-> false
                 | Some a -> what_is_in_return if_exp lbl_list a

             | _-> what_is_in_return if_exp lbl_list t
	     )


                     
		  
 
		

let rec has_return_bug e = function
     [] -> false
  |   h::t ->( match Ast_c.unwrap h with
                Ast_c.Jump (Ast_c.ReturnExpr((((Ast_c.Unary((((Ast_c.Ident(Ast_c.RegularName (id, [ii1]))), typ1), ii2),
                      Ast_c.UnMinus)), typ2),ii3) )) ->  if id =~ "^[A-Z_][A-Z_0-9]*$" then true
                                                         else false
             |  Ast_c.Jump (Ast_c.ReturnExpr((((Ast_c.Unary((((Ast_c.Constant (c)), typ1), ii2),Ast_c.UnMinus)), typ2),ii3) )) -> true
             |  Ast_c. Jump (Ast_c.ReturnExpr (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(s,ii))), typ), ii1), es)), typ1), ii2)) ->
                                                         if s =~ "ERR_PTR" || s =~ "PTR_ERR" then true else false

             
             |  Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) -> 
                  (match e with 
                    (((Ast_c.Unary    (((exp, typ11), ii11), Ast_c.Not)), typ10), ii10)->
		      if(compare_exp ((exp, typ11), ii11) (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) then 
                           (match is_pointer ((exp, typ11), ii11) with
			      IsPtr-> true
                            | _-> false
                           )
		      else false
                  | (((Ast_c.Binary   (e10, Ast_c.Logical Ast_c.Eq, e2)), typ10), ii10)-> 

		      if(compare_exp e10 (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) then 
			   (match is_pointer e10 with
                              IsPtr-> true
			   |  _-> false
                           )

                      else false
                  | (((Ast_c.Ident (ident)), typ11), ii11)-> 
		      if(compare_exp (((Ast_c.Ident (ident)), typ11), ii11) (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) then 
			   (match is_pointer ((exp, typ11), ii11) with
                              IsntPtr-> true
			   |  _-> false
                           )
                      else false
                  |  (((Ast_c.Binary   (e10, Ast_c.Logical Ast_c.NotEq, e11)), typ10), ii10)-> 
		      
                      if(compare_exp e10 (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) then
			   (match is_pointer e10  with
                              IsntPtr-> true
                           |   _-> false
                           )
                      else false



                  | _ -> 
                                                if id =~ "^[a-z_][a-z_0-9]*$"  then (
                                                   (
						    if (is_return_id_in_expr e (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) then 
						     (true)
						    else (false) ))
                                                else  if id =~ "NULL" then true
                                                else false   
                  )
             | Ast_c.Jump (Ast_c.Goto name) ->  true
                  


             |  _ -> has_return_bug e t
            )


(* Check whether the if branch has goto statement *)

let rec has_goto = function
  []-> false
  | h::t -> (match Ast_c.unwrap h with
                 Ast_c.Jump (Ast_c.Goto name) -> true
              | _ -> has_goto t
            )



(* Check whether return_id is used in the if branch  *)

let is_return_id_used_inner st return_id =
 let flag = ref false in
 (Visitor_c.vk_statement
   { Visitor_c.default_visitor_c with
     Visitor_c.kexpr =
     (function (k,bigf) -> function exp -> k exp;
        match return_id with
           None -> ()
        |  Some a -> if (compare_exp exp a) then flag:= true else ()
       ) }
   st);
 !flag

 let check_string_exp exp =
   match exp with  
   |  (((Ast_c.Constant (Ast_c.MultiString _)), typ), ii)->true   
   |   (((Ast_c.Constant (Ast_c.String _)), typ), ii)->true
   |  _ -> false

let has_string st  =
 let flag = ref false in
 (Visitor_c.vk_statement
   { Visitor_c.default_visitor_c with
     Visitor_c.kexpr =
     (function (k,bigf) -> function exp -> k exp;
         if (check_string_exp  exp) then
           flag:=true
         else ()
       ) }
   st);
 !flag



let rec is_return_id_used return_id = function
     [] ->  false
  |  h::t -> if(is_return_id_used_inner h return_id) then true  else is_return_id_used return_id t



let rec is_return_id_used_bug st  = function
     [] ->  true
  |   h::t -> if(is_return_id_used_inner st h) then  is_return_id_used_bug st t
              else false

let rec exp_conv_to_st = function
  []->[]
  | h::t->(Ast_c.ExprStatement (Some h), [])::(exp_conv_to_st t)



(* Check whether st1 is suffix of st2 *)

let is_suffix st1 st2 =
if (List.length st1)= 0 then true
else
(
  let rec is_suffix_loop = function
      [] -> false
    | h::t ->  if (clean_statement_list st1) = (clean_statement_list (h::t)) then (true) else is_suffix_loop t
in is_suffix_loop st2
)


(* skip return from statement list *)

let rec skip_return = function
   []-> []
  | h::t-> (match Ast_c.unwrap h with 
              Ast_c.Jump (Ast_c.ReturnExpr e)-> skip_return t
            | _ -> h::(skip_return t)
           )


(* Check whether a suffix of st1  match with suffix of st2, means intersect *)

let is_intersect st1 st2 =
   let rec is_inters_loop = function
       [] -> false
     | h::t -> is_suffix (h::t) st2 || is_inters_loop t
in is_inters_loop st1



(* Check whether a suffix of st1  match with suffix of any element of st2, means intersect *)

let rec is_intersect_with_list st1 = function
    []-> false
  | h::t -> if (is_intersect st1 h) then true else is_intersect_with_list st1 t


(*Exapnad goto in statement list. if any statement list has any goto x statement then it is appended  with all statments of label x and it   recurssively *)







let rec expand_goto lbl_list = function
   []-> []
  |  h::t -> (match h with
               (Ast_c.Jump (Ast_c.Goto name),ii) -> expand_goto lbl_list (code_for_goto name lbl_list)
             | (Ast_c.Labeled (Ast_c.Label (name, st)),ii)->(match st with
                                                    (Ast_c.Jump (Ast_c.Goto name1),ii) -> expand_goto lbl_list (code_for_goto name1 lbl_list)
	                                               |  _ -> h::(expand_goto lbl_list t)
                                                       )
             | (Ast_c.Iteration  (Ast_c.While (e, st)),ii)-> if !mark = 0 then  
(Ast_c.Iteration  (Ast_c.While (e, ((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st)))),[]))),ii)::(expand_goto lbl_list t)
else(
mark:=0;
(Ast_c.Iteration  (Ast_c.While (e, ((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st)))),[]))),ii))::(expand_goto lbl_list [])
  
	     |  (Ast_c.Selection  (Ast_c.If (e, st1, st2)),ii) -> if!mark =0 then
(Ast_c.Selection  (Ast_c.If (e, ((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st1)))),[]), st2)),ii)::(expand_goto lbl_list t)
else(
mark:=0;
(Ast_c.Selection  (Ast_c.If (e, ((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st1)))),[]), st2)),ii)::(expand_goto lbl_list []))
	     | (Ast_c.Selection  (Ast_c.Switch (e, st)),ii) -> if!mark =0 then
(Ast_c.Selection  (Ast_c.Switch (e, ((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st)))),[]))),ii)::(expand_goto lbl_list t)
else(
mark:=0;
(Ast_c.Selection  (Ast_c.Switch (e, ((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st)))),[]))),ii)::(expand_goto lbl_list []))
	     |  (Ast_c.Iteration  (Ast_c.DoWhile (st, e)),ii) ->  if!mark =0 then
(Ast_c.Iteration  (Ast_c.DoWhile (((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st)))),[]), e)),ii)::(expand_goto lbl_list t)
else(
mark:=0;
(Ast_c.Iteration  (Ast_c.DoWhile (((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st)))),[]), e)),ii)::(expand_goto lbl_list []))

	     |  (Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)),ii) ->if!mark =0 then
(Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st)))),[]))),ii)::(expand_goto lbl_list t)
else(
mark:=0;
(Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),((Ast_c.Compound (stmtEle_add_list (expand_goto lbl_list (create_compound_st st)))),[]))),ii)::(expand_goto lbl_list []))

	     |  (Ast_c.Jump (Ast_c.Return),ii) ->  mark:= 1; h::(expand_goto lbl_list [])
	     |  (Ast_c.Jump (Ast_c.ReturnExpr e),ii) ->mark:= 1 ; h::(expand_goto lbl_list [])

             |  _ -> h::(expand_goto lbl_list t)
            )


(* Check whether any if branch share with existing lbl_list*)

let rec is_shared_lbl_list lbl_code lbl_list = function
  []-> false
  |(name,code)::t -> if (is_intersect  lbl_code   code) then true 
                     else is_shared_lbl_list lbl_code lbl_list t

(* Check whether any if branch share with other if branches*)

let rec is_shared_if_list return_id lbl_code1 line_no1 lbl_list if_list = function
  []-> false
  | (line_no2,if_code,lbl_code,status)::t-> if (is_intersect  (skip_return lbl_code1) (skip_return lbl_code)) 
                                                 && line_no1!=line_no2 then true
                                            else is_shared_if_list return_id lbl_code1 line_no1 lbl_list if_list t 


(* Check whether two branches are not in same order or not *)


let rec match_list_inner h = function
  []-> false
  | h1::t1-> if (compare_stmts h h1) then true else match_list_inner h t1


let rec match_list lbl_code1 = function
  []-> false
  | h::t -> if (match_list_inner h lbl_code1) then true else match_list lbl_code1 t


let rec chk_unorder lbl_code2 line_no= function
     []->[]
  |  (line_no1,if_code,lbl_code1,status)::t -> if (match_list (skip_return lbl_code1)  (skip_return lbl_code2)) && line_no != line_no1 then 
                                                  (unorder_count:=1; chk_unorder lbl_code2 line_no [])
                                               else chk_unorder lbl_code2 line_no t


(* Filtering if branches list *)

let  filter_shared  return_id lbl_list init_lbl_list if_list =
   let rec filter_shared_loop = function 
    []->[]
  | (line_no,if_code,lbl_code,status)::t -> if (is_suffix  lbl_code    init_lbl_list ) then
                                               (line_no,if_code,lbl_code,status)::(filter_shared_loop t)
                                            else if (is_shared_lbl_list lbl_code lbl_list lbl_list) then 
					       (line_no,if_code,lbl_code,status)::(filter_shared_loop t)
                                            else if (is_shared_if_list return_id lbl_code line_no lbl_list if_list if_list) then 
					       (line_no,if_code,lbl_code,status)::(filter_shared_loop t)
                                            else ( chk_unorder lbl_code line_no if_list; perfect_function:= 1; filter_shared_loop t)
   in filter_shared_loop if_list



(* Partition if branch into if_code (which need to be inside if branch) and lbl_code(which need to be transfer to label or can be shared)  *)


let rec partition_b  if_code lbl_code  = function
   []->  (*print_string(string_of_int(List.length if_code));print_string(string_of_int(List.length lbl_code));*)(if_code, lbl_code)
  |  h::t -> (match Ast_c.unwrap h  with
    |  Ast_c.ExprStatement (Some ((Ast_c.FunCall(e,args), typ),ii))->   
                 if (List.exists (function s->
                         match Ast_c.unwrap s with
                           Common.Left es ->
                             (match Ast_c.unwrap es with
                                (Ast_c.Constant (Ast_c.String _),_) -> true
                             |  (Ast_c.Constant (Ast_c.MultiString _),_) -> true
                             |  _ -> false
                             )
                         |   _ -> false) args) || (has_string h)
                               then (partition_b (if_code@[h]) lbl_code t)
                 else  partition_b if_code (lbl_code@[h]) t

    |  Ast_c.Jump (Ast_c.ReturnExpr e)-> (*print_string("Return");*)
                                                 partition_b if_code (lbl_code@[h]) t
    | Ast_c.Jump (Ast_c.Goto name) -> (*print_string("Return");*)
                                                 partition_b if_code (lbl_code@[h]) t

    |  _ -> (*print_string("Other");*) partition_b (if_code@[h]) lbl_code  t)




let rec partition  if_code lbl_code  = function
   []->        (if_code, lbl_code)
 | h::t -> (match Ast_c.unwrap h  with
             | Ast_c.ExprStatement (Some ((Ast_c.FunCall(e,args), typ),ii))->
		 if (List.exists (function s->
                         match Ast_c.unwrap s with
                           Common.Left es ->
                             (match Ast_c.unwrap es with
                                (Ast_c.Constant (Ast_c.String _),_) -> true
                              | (Ast_c.Constant (Ast_c.MultiString _),_) -> true
                              | _ -> false
                             )
                         |  _ -> false) args) || (has_string h)then partition (if_code@lbl_code@[h]) [] t
                 else partition if_code (lbl_code@[h]) t
  
             | Ast_c.Jump (Ast_c.ReturnExpr e)-> 
                                                 partition if_code (lbl_code@[h]) t
             | _ ->partition (if_code@lbl_code@[h]) []  t)



(* Partition every element of If_list into If_code and lbl_code *)


let rec remove_cast = function
  []-> []
  |  h::t-> match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some (((Ast_c.Cast    (c, (((Ast_c.FunCall  (e, es)), typ1), ii1))), typ), ii))->
	              (Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ1), ii1)),[])::(remove_cast t)
    |  _-> h::(remove_cast t)



let rec do_partition_if_list = function
    []-> []
  |  (a,b)::t-> (*Printf.printf "%s" (Dumper.dump b);*)
              let (if_code,lbl_code) = (*partition_b [] [] (remove_cast b) in*) partition [] [] b in 
                   if(List.length lbl_code)> 1 then 
                        
                        (a,if_code,lbl_code,Incomplete)::(do_partition_if_list t)
                   else (perfect_function:=1; do_partition_if_list t) 
				    
                 
(* Create lbl_list from existing label and out0 label *)


(* let rec create_lbl_list_loop current finished  st =
  match Ast_c.unwrap st  with
    |  Ast_c.Labeled (Ast_c.Label (name, st)) ->
	     (((name,  [st] )::((List.map (function (label,code ) -> (label, code@[st])) current))), finished)
    |  Ast_c.Jump (Ast_c.Goto name) -> ([], (List.map (function (label,code)-> (label, code@[st])) current )@finished)
    |  Ast_c.Jump (Ast_c.ReturnExpr e) -> ([], (List.map (function (label,code)-> (label, code@[st])) current )@finished)
    |  _ -> ((List.map (function (label,code ) -> (label, code@[st])) current), finished)

*)


let rec remove_stmtElelist l=
          match l with
            [] -> []
          |  h::t -> match h with
                      Ast_c.StmtElem st -> st::(remove_stmtElelist t)
            |  _-> remove_stmtElelist t



let create_stmtlist st =
  match Ast_c.unwrap st with
      Ast_c.Compound  st -> (remove_stmtElelist st)
  |     _ -> [st]


let rec return_exists_in_list_bool = function
    []-> false
  |  h::t ->  match Ast_c.unwrap h with
               Ast_c.Jump (Ast_c.ReturnExpr e) -> true
    |    Ast_c.Jump (Ast_c.Return) -> true
    |    _ -> return_exists_in_list_bool t



let rec create_lbl_list_loop current finished  st =
  match Ast_c.unwrap st  with
  |    Ast_c.Labeled (Ast_c.Label (name, st1)) -> (match Ast_c.unwrap st1 with
    |  Ast_c.Labeled (Ast_c.Label (name, st2)) ->
                                                       (((name,  [st2] )::(name,  [st2] )::((List.map (function (label,code ) -> (label, code@[st2])) current))), finished)
    |  Ast_c.Selection  (Ast_c.If (e, st11, st22)) ->
                                                        if(return_exists_in_list_bool (create_stmtlist st11)) then(
                                                          (((name,  [st1] )::((List.map (function (label,code ) -> (label, code@[st1])) current))), finished))
                                                        else (
                                                          (((name,  (create_stmtlist st11) )::((List.map (function (label,code ) ->
                                                            (label, code@(create_stmtlist st11))) current))), finished))
    |  Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st11)) ->
                                                       if(return_exists_in_list_bool (create_stmtlist st11)) then
                                                         (((name,  [st1] )::((List.map (function (label,code ) -> (label, code@[st1])) current))), finished)
                                                       else
                                                         (((name,  (create_stmtlist st11) )::((List.map (function (label,code ) ->
                                                           (label, code@(create_stmtlist st11))) current))), finished)
    |  _-> (((name,  [st1] )::((List.map (function (label,code ) -> (label, code@[st1])) current))), finished)
                                                 )
  |    Ast_c.Jump (Ast_c.Goto name) -> ([], (List.map (function (label,code)-> (label, code@[st])) current )@finished)
  |    Ast_c.Jump (Ast_c.ReturnExpr e) -> ([], (List.map (function (label,code)-> (label, code@[st])) current )@finished)
  |    Ast_c.Selection  (Ast_c.If (e, st11, st22)) -> if(return_exists_in_list_bool (create_stmtlist st11)) then(
                                                     ((List.map (function (label,code ) -> (label, code@[st])) current), finished))
                                                   else(
                                                     ((List.map (function (label,code ) -> (label, code@(create_stmtlist st11))) current), finished))
  |    Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st1)) ->
                                                   if(return_exists_in_list_bool (create_stmtlist st1)) then
                                                     ((List.map (function (label,code ) -> (label, code@[st])) current), finished)
                                                   else
                                                     ((List.map (function (label,code ) -> (label, code@(create_stmtlist st1))) current), finished)
  |    _ -> ((List.map (function (label,code ) -> (label, code@[st])) current), finished)


let rec create_lbl_list current finished = function
     [] ->  current@finished
  |  e::es ->
       let (current, finished) = create_lbl_list_loop current finished e
in create_lbl_list current finished es


(*Create if branches list for count only goto or goto and return both *) 

let rec is_goto_forward goto_st name1 = function
   []-> false
  | h::t-> match Ast_c.unwrap h with
             Ast_c.Labeled (Ast_c.Label (name2, st))-> if (compare_name name1 name2) then
		                                           if ((find_startline_no [h])> (find_startline_no [goto_st])) then true
							   else false
		                                       else
	                                                   is_goto_forward goto_st name1 t
           |_ -> is_goto_forward goto_st name1 t

 
let rec has_goto_new prog = function
  []-> false
  | h::t -> (match Ast_c.unwrap h with
                 Ast_c.Jump (Ast_c.Goto name) -> if is_goto_forward h name prog then true
		                                 else false
              |  _ -> has_goto_new prog t
             )

let rec create_if_list_goto prog = function
   [] -> []
  |  h::t -> (match Ast_c.unwrap h with
              Ast_c.Labeled (Ast_c.Label (name, st))->let old_list = create_if_list_goto prog [] in old_list
    |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
                                                        if(has_inner_block st1) && (has_inner_block st2)then
                                                              let new_list = (create_if_list_goto prog (create_compound_st st1)) in
                                                              let new_list1 = (create_if_list_goto prog (create_compound_st st2)) in
                                                              let old_list =  create_if_list_goto prog t in
                                                                  new_list@new_list1@old_list
                                                        else if(has_inner_block st1) && (not(has_inner_block st2)) then
                                                              let new_list = (create_if_list_goto prog (create_compound_st st1)) in
                                                              let old_list =  create_if_list_goto prog t in
                                                                  new_list@old_list
                                                        else if(not(has_inner_block st1)) && (has_inner_block st2)then

                                                              let new_list1 = (create_if_list_goto prog (create_compound_st st2)) in
                                                              let old_list =  create_if_list_goto prog t in
                                                              if(has_goto(create_compound_st st1)) then
                                                                 let old_list = create_if_list_goto prog t in
                                                                 ((find_startline_no (create_compound_st st1)),(create_compound_st st1))::old_list@new_list1
                                                              else let old_list = create_if_list_goto prog t in new_list1@old_list
                                                        else if(has_goto (create_compound_st st1))then
                                                                 let old_list = create_if_list_goto prog t in
                                                                 ((find_startline_no (create_compound_st st1)),(create_compound_st st1))::old_list
                                                        else let old_list = create_if_list_goto prog t in old_list
    |  Ast_c.Selection  (Ast_c.Switch (e, st)) ->if(has_inner_block st) then
                                                              let new_list = (create_if_list_goto prog (create_compound_st st)) in
                                                              let old_list =  create_if_list_goto prog t in
                                                                  new_list@old_list
                                                        else let old_list = create_if_list_goto prog t in old_list
    |  Ast_c.Iteration  (Ast_c.While (e, st))->if(has_inner_block st) then
                                                              let new_list = (create_if_list_goto prog (create_compound_st st)) in
                                                              let old_list =  create_if_list_goto prog t in
                                                                  new_list@old_list
                                                      else let old_list = create_if_list_goto prog t in old_list
    |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->if(has_inner_block st) then
                                                              let new_list = (create_if_list_goto prog (create_compound_st st)) in
                                                              let old_list =  create_if_list_goto prog t in
                                                                  new_list@old_list
                                                         else let old_list = create_if_list_goto prog t in old_list
    |  Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->if(has_inner_block st) then
                                                                                          let new_list = (create_if_list_goto prog (create_compound_st st)) in
                                                                                          let old_list =  create_if_list_goto prog t in
                                                                                              new_list@old_list
                                                                                       else let old_list = create_if_list_goto prog t in old_list
    |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> if(has_inner_block st) then
                                                                     let new_list = (create_if_list_goto prog (create_compound_st st)) in
                                                                     let old_list =  create_if_list_goto prog t in
                                                                                              new_list@old_list
                                                                                       else let old_list = create_if_list_goto prog t in old_list
    |  _ ->let old_list = create_if_list_goto prog t in old_list
          )

(* Check whether if branch has atleast one f(x) where x does not contain any string *)

let rec has_function = function
  []-> false
  | h::t->  (match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some ((Ast_c.FunCall(e,args), typ),ii))->
                 if (List.exists (function s->
                         match Ast_c.unwrap s with
                           Common.Left es ->
                             (match Ast_c.unwrap es with
                                (Ast_c.Constant (Ast_c.String _),_) -> true
                             |  (Ast_c.Constant (Ast_c.MultiString _),_) -> true
                             |  _ -> false
                             )
                         |   _ -> false) args) then has_function t
                 else true
             | _ -> has_function t)



let rec has_switch = function
    []-> false
  | h::t-> (match Ast_c.unwrap h with
                Ast_c.Labeled (Ast_c.Label (name, st))-> if(has_inner_block st) then
		                                       if (has_switch (create_compound_st st)) then true else (has_switch t)
  		                                         else (has_switch t)
             |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> if(has_inner_block st1) then
                                                           if (has_switch (create_compound_st st1)) then true else (has_switch t)
                                                         else (has_switch t)
	     |  Ast_c.Selection  (Ast_c.Switch (e, st)) -> true
             |  Ast_c.Iteration  (Ast_c.While (e, st))->if(has_inner_block st) then
                                                           if (has_switch (create_compound_st st)) then true else (has_switch t)
                                                         else (has_switch t)
                                                          
             |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->if(has_inner_block st) then
                                                           if (has_switch (create_compound_st st)) then true else (has_switch t)
                                                         else (has_switch t)
                                                          
             |  Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->if(has_inner_block st) then
                                                           if (has_switch (create_compound_st st)) then true else (has_switch t)
                                                         else (has_switch t)
                                                          
             |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> if(has_inner_block st) then
                                                           if (has_switch (create_compound_st st)) then true else (has_switch t)
                                                         else (has_switch t)
                                                          
             | _ -> (has_switch t)
           ) 

(* Create if branches list *)

let rec has_multiple_st = function
  []-> false
  |   h::t -> (match Ast_c.unwrap h with
    |    Ast_c.ExprStatement (None) -> false
    |   _ -> true
)


let rec create_if_list_in_exe_path lbl_list tmp_base_if_list base_if_list if_list = function
    []-> base_if_list::if_list
  | h::t-> match Ast_c.unwrap h with
            |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
                 if (has_multiple_st (create_compound_st st2)) then
                     if(has_inner_block st1) && (has_inner_block st2) then
                          let tmp_base_if_list = base_if_list in 
                          let new_if_list1 = create_if_list_in_exe_path lbl_list tmp_base_if_list base_if_list if_list (create_compound_st st1)  in
			  let new_if_list2 = create_if_list_in_exe_path lbl_list tmp_base_if_list base_if_list if_list (create_compound_st st2)  in
			           create_if_list_in_exe_path lbl_list tmp_base_if_list tmp_base_if_list (new_if_list1@new_if_list2) t
                     else if (has_inner_block st1) && (not(has_inner_block st2)) then
                          let tmp_base_if_list = base_if_list in
                          let new_if_list = create_if_list_in_exe_path lbl_list tmp_base_if_list base_if_list if_list (create_compound_st st1)  in
                                   create_if_list_in_exe_path lbl_list tmp_base_if_list tmp_base_if_list 
			                ((tmp_base_if_list@[(find_startline_no (create_compound_st st2))])::new_if_list) t
                     else if (not(has_inner_block st1)) && (has_inner_block st2) then
                          let tmp_base_if_list = base_if_list in
                          let new_if_list = create_if_list_in_exe_path lbl_list tmp_base_if_list base_if_list if_list (create_compound_st st2)  in
                                   create_if_list_in_exe_path lbl_list tmp_base_if_list tmp_base_if_list 
			                ((tmp_base_if_list@[(find_startline_no (create_compound_st st1))])::new_if_list) t
                     else
                          create_if_list_in_exe_path lbl_list tmp_base_if_list 
			       (base_if_list@[(find_startline_no (create_compound_st st1))]@[(find_startline_no (create_compound_st st2))]) if_list t
                 
                 else
                     if(has_inner_block st1) then
                          let tmp_base_if_list = base_if_list in
                          let new_if_list = create_if_list_in_exe_path lbl_list tmp_base_if_list base_if_list if_list (create_compound_st st1)  in
                                   create_if_list_in_exe_path lbl_list tmp_base_if_list tmp_base_if_list new_if_list t
                     else
                          create_if_list_in_exe_path lbl_list tmp_base_if_list (base_if_list@[(find_startline_no (create_compound_st st1))]) if_list t
                   
            | _ -> create_if_list_in_exe_path lbl_list tmp_base_if_list base_if_list if_list t
		
let opp_exp_st st =
   match Ast_c.unwrap st with

   | Ast_c.ExprStatement (Some (((Ast_c.Binary   ((((Ast_c.ParenExpr (((Ast_c.Assignment (e1, op1, e2), typ5), ii5))), typ4), ii4),
                            (Ast_c.Logical Ast_c.Eq) , ((Ast_c.Ident (Ast_c.RegularName("NULL",ii3)), typ1), ii1))), typ), ii)) ->
       ((Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op1, (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]),
       (Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op1, e2), typ), [])),[]))

   |  Ast_c.ExprStatement (Some (((Ast_c.Binary   ((((Ast_c.ParenExpr (((Ast_c.Assignment (e1, op1, e2), typ5), ii5))), typ4), ii4),
                            (Ast_c.Logical Ast_c.NotEq) , ((Ast_c.Ident (Ast_c.RegularName("NULL",ii3)), typ1), ii1))), typ), ii)) ->

        ((Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op1, e2), typ), [])),[]),
       (Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op1, (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]))

   |  Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
        ((Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op, e2), typ), [])),[]),
        (Ast_c.ExprStatement (None),[]))
   |  Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR",ii3))), typ1), ii1), es) ), typ), ii)) ->
        if (List.length es>= 0) then
           (* if (exp_chk (List.hd (remove_option (make_argslist [] es)))) then *)
                 ((Ast_c.ExprStatement (Some ((Ast_c.Assignment ((List.hd (remove_option (make_argslist [] es))), (Ast_c.SimpleAssign),
                                                         (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]),
                  (Ast_c.ExprStatement (Some (List.hd (remove_option (make_argslist [] es)))),[]))
(*          else (st,st)*)
        else (st,st)
   |  Ast_c.ExprStatement (Some (((Ast_c.Binary   ((((Ast_c.FunCall
((((Ast_c.Ident (Ast_c.RegularName("PTR_ERR",ii3))), typ2), ii2), es)), typ1), ii1), (Ast_c.Logical Ast_c.Eq),
(((Ast_c.Unary    (e, Ast_c.UnMinus)), typ4), ii4) )), typ), ii))->

  if (List.length es>= 0) then
           (* if (exp_chk (List.hd (remove_option (make_argslist [] es)))) then *)
                 ((Ast_c.ExprStatement (Some ((Ast_c.Assignment ((List.hd (remove_option (make_argslist [] es))), (Ast_c.SimpleAssign),
                                                         (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]),
                  (Ast_c.ExprStatement (Some (List.hd (remove_option (make_argslist [] es)))),[]))
(*          else (st,st)*)
   else (st,st)

   |  Ast_c.ExprStatement (Some (((Ast_c.Binary   ((((Ast_c.FunCall
((((Ast_c.Ident (Ast_c.RegularName("PTR_ERR",ii3))), typ2), ii2), es)), typ1), ii1), (Ast_c.Logical Ast_c.NotEq),
(((Ast_c.Unary    (e, Ast_c.UnMinus)), typ4), ii4) )), typ), ii))->


  if (List.length es>= 0) then
           (* if (exp_chk (List.hd (remove_option (make_argslist [] es)))) then *)
                 (Ast_c.ExprStatement (Some (List.hd (remove_option (make_argslist [] es)))),[]),
                 ((Ast_c.ExprStatement (Some ((Ast_c.Assignment ((List.hd (remove_option (make_argslist [] es))), (Ast_c.SimpleAssign),
                                                         (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]))
(*          else (st,st)*)
   else (st,st)
  | Ast_c.ExprStatement (Some((Ast_c.FunCall
 (((Ast_c.Ident (Ast_c.RegularName("unlikely",ii3)), typ1), ii1), es), typ), ii)  ) ->
        (
           match (remove_option (make_argslist [] es))  with
              []-> (st,st)
            | h1::t1-> ( match h1 with
                           ((Ast_c.Unary (((Ast_c.Ident (ident), typ6), ii6), Ast_c.Not), typ5), ii5)->
                 ((Ast_c.ExprStatement (Some ((Ast_c.Assignment ( ((Ast_c.Ident (ident), typ6), []) , (Ast_c.SimpleAssign),
                                                         (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]),
                  (Ast_c.ExprStatement (Some ((Ast_c.Ident (ident), typ6), [])),[]))

                          | ((Ast_c.Ident (ident), typ6), ii6) ->
                 (Ast_c.ExprStatement (Some ((Ast_c.Ident (ident), typ6), [])),[]),
                 ((Ast_c.ExprStatement (Some ((Ast_c.Assignment (((Ast_c.Ident (ident), typ6), []), (Ast_c.SimpleAssign),
                                                         (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]))


                          | _-> (st,st)
                       )

   | _ -> (st,st)
         )

  | Ast_c.ExprStatement (Some (((Ast_c.Unary    ((((Ast_c.ParenExpr ((((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR",ii5))),
typ4), ii4), es)), typ3), ii3))), typ2), ii2), Ast_c.Not)), typ), ii)) ->

        if (List.length es>= 0) (*&& (exp_chk (List.hd (remove_option (make_argslist [] es)))) *) then
          ((Ast_c.ExprStatement (Some (List.hd (remove_option (make_argslist [] es)))),[]),
          (Ast_c.ExprStatement (Some ((Ast_c.Assignment ((List.hd (remove_option (make_argslist [] es))), (Ast_c.SimpleAssign),
                                                         (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]))
        else (st,st)
   |  Ast_c.ExprStatement (Some (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.Eq),
                   (((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii)) ->
        ((Ast_c.ExprStatement (Some ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]),
        (Ast_c.ExprStatement (Some e11),[]))
   |  Ast_c.ExprStatement (Some (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.NotEq),
                   (((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii)) ->
        ((Ast_c.ExprStatement (Some e11),[]),
        (Ast_c.ExprStatement (Some ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]))
   |  Ast_c.ExprStatement (Some (((Ast_c.Unary (e11, Ast_c.Not)), typ), ii))  ->
        ((Ast_c.ExprStatement (Some ((Ast_c.Assignment (e11, (Ast_c.SimpleAssign),
                                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]),
        (Ast_c.ExprStatement (Some e11),[]))
   |  Ast_c.ExprStatement (Some ((exp, typ), ii)) (*when exp_chk ((exp, typ), ii) *)->
        ((Ast_c.ExprStatement (Some ((exp, typ), ii)),[]),
        (Ast_c.ExprStatement (Some ((Ast_c.Assignment (((exp, typ), ii), (Ast_c.SimpleAssign),
                                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",[])), typ), [])), typ), [])),[]))




   
let rec exp_exists_in_list exp = function
     []-> false
  |   h::t-> if(compare_exp exp h) then true else exp_exists_in_list exp t


let rec find_reaching_definition var def alloc line_no = function
   []-> (Some def, Some alloc)
  | h::t->
            let start_lineno = find_startline_no (create_compound_st h) in
            let end_lineno = find_endline_no (create_compound_st h) in
            if (line_no >= start_lineno && line_no <= end_lineno) || line_no >= end_lineno then
              (
                   match Ast_c.unwrap h with
		   |  Ast_c.Labeled (Ast_c.Label (name, st)) -> 
		              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
				find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t

                   |   Ast_c.Labeled (Ast_c.Case  (e, st)) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t

                   |   Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t

                   |   Ast_c.Labeled (Ast_c.Default st) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t
                
		   |   Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) -> 
		             if (exp_exists_in_list var (remove_option (make_argslist [] es)))  then
			       if(List.length (make_argslist [] es) = 1) then
                                    find_reaching_definition var def  (h, start_lineno) line_no t
			       else find_reaching_definition var def alloc line_no t
                             else find_reaching_definition var def alloc line_no t


                   |   Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
                                 if (compare_exp e1 var) then
                                    find_reaching_definition var (e2,start_lineno) alloc line_no t
                                 else find_reaching_definition var def alloc line_no t


                   |   Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
                              let start_line1 = find_startline_no (create_compound_st st1) in
                              let end_line1 = find_endline_no (create_compound_st st1) in
                              let start_line2 = find_startline_no (create_compound_st st2) in
                              let end_line2 = find_endline_no (create_compound_st st2) in

                              if line_no > start_line1 && line_no <= end_line1 then
                                let def=    
                                  (match (opp_exp_st ((Ast_c.ExprStatement (Some e)),[])) with   
                                    |   (((Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii1))),ii), b)->   
                                               if (compare_exp e1 var) then ((((Ast_c.Assignment (e1, op, e2)), typ), ii1),start_lineno) else def   
                                    |   _->def) in     

                                find_reaching_definition var def alloc line_no (create_compound_st st1)
                              else if line_no > start_line2 && line_no <= end_line2 then
                                  let def=   
                                    (match (opp_exp_st ((Ast_c.ExprStatement (Some e)),[])) with    
                                    |    (a,((Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii1))),ii))->   
                                             if (compare_exp e1 var) then (e2,start_lineno) else def    
                                    |    _->def) in    
                                find_reaching_definition var def alloc line_no (create_compound_st st2)

                              else find_reaching_definition var def alloc line_no t

                   |   Ast_c.Selection  (Ast_c.Switch (e, st)) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t

                   |   Ast_c.Iteration  (Ast_c.While (e, st)) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t

                   |   Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t

                   |   Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t

                   |   Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                find_reaching_definition var def alloc line_no (create_compound_st st)
                              else find_reaching_definition var def alloc line_no t
                   | _ -> find_reaching_definition var def alloc line_no t


                   |_ -> find_reaching_definition var def alloc line_no t


              )
              else find_reaching_definition var def alloc line_no t
                

let rec create_if_list lbl_list = function
   [] -> []
 | h::t -> (match Ast_c.unwrap h with
              Ast_c.Labeled (Ast_c.Label (name, st))->
		                                        if(has_inner_block st) then
                                                              let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                              let old_list =  create_if_list  lbl_list t in
                                                                  new_list@old_list
                                                        else let old_list = create_if_list  lbl_list t in old_list

            |  Ast_c.Labeled (Ast_c.Case  (e, st)) -> 
                                                        if(has_inner_block st) then
                                                              let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                              let old_list =  create_if_list  lbl_list t in
                                                                  new_list@old_list
                                                        else let old_list = create_if_list  lbl_list t in old_list
            |  Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) -> 
                                                        if(has_inner_block st) then
                                                              let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                              let old_list =  create_if_list  lbl_list t in
                                                                  new_list@old_list
                                                        else let old_list = create_if_list  lbl_list t in old_list
            |  Ast_c.Labeled (Ast_c.Default st) -> 
                                                        if(has_inner_block st) then
                                                              let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                              let old_list =  create_if_list  lbl_list t in
                                                                  new_list@old_list
                                                        else let old_list = create_if_list  lbl_list t in old_list

            | Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->

             let e1 = 
             (match (opp_exp_st ((Ast_c.ExprStatement (Some e)),[])) with
                | (a,((Ast_c.ExprStatement (Some e1)),ii))-> e1
                | _->e) in

                                                        if(has_inner_block st1) && (has_inner_block st2)then
                                                              let new_list = (create_if_list  lbl_list (create_compound_st st1)) in
							      let new_list1 = (create_if_list  lbl_list (create_compound_st st2)) in
                                                              let old_list =  create_if_list  lbl_list t in
                                                                  new_list@new_list1@old_list
                                                        else if(has_inner_block st1) && (not(has_inner_block st2)) then

                                                              let new_list1 = (create_if_list  lbl_list (create_compound_st st1)) in
                                                              let old_list =  create_if_list  lbl_list t in
                                                             (* if(has_return_bug e1 (create_compound_st st2)) then*)
							      if(has_return  (create_compound_st st2))
                                                                   && (has_function (create_compound_st st2)) then
                                                                 let old_list = create_if_list  lbl_list  t in
                                                                     ((find_startline_no (create_compound_st st2)),
                                                                      (create_compound_st st2))::old_list@new_list1
                                                              else let old_list = create_if_list  lbl_list t in new_list1@old_list

                                                        else if(not(has_inner_block st1)) && (has_inner_block st2)then
                                                              
                                                              let new_list1 = (create_if_list  lbl_list (create_compound_st st2)) in
                                                              let old_list =  create_if_list  lbl_list t in
(*                         				      if(what_is_in_return1 e lbl_list (create_compound_st st1))  then *)
							       if(has_return  (create_compound_st st1))
							           && (has_function (create_compound_st st1)) then
							       (*if(has_return_bug e (create_compound_st st1)) then*)
                                                                 let old_list = create_if_list  lbl_list  t in
					                             ((find_startline_no (create_compound_st st1)),
								      (create_compound_st st1))::old_list@new_list1             
                                                              else let old_list = create_if_list  lbl_list t in new_list1@old_list
                                                        else (
                                                             (* if(what_is_in_return1 e lbl_list (create_compound_st st1))  then *)

                                                               if(has_return  (create_compound_st st1)) && (has_return  (create_compound_st st2))
                                                                   && (has_function (create_compound_st st1)) && (has_function (create_compound_st st2)) then
							      (* if(has_return_bug e (create_compound_st st1)) && (has_return_bug e1 (create_compound_st st2)) then*)
							         let old_list = create_if_list  lbl_list t in
             							 ((find_startline_no (create_compound_st st1)),
								  (create_compound_st st1))::((find_startline_no (create_compound_st st2)),
                                                                  (create_compound_st st2))::old_list
							    (* else if (not(has_return_bug e (create_compound_st st1))) && (has_return_bug e1 (create_compound_st st2)) then*)
                                                              else if (not (has_return (create_compound_st st1))) && ((has_return  (create_compound_st st2)) 
														       && (has_function (create_compound_st st2))) then
                                                                 let old_list = create_if_list  lbl_list t in
                                                                 ((find_startline_no (create_compound_st st2)),
                                                                  (create_compound_st st2))::old_list
                                                            (*  else if (has_return_bug e (create_compound_st st1)) && (not(has_return_bug e1 (create_compound_st st2))) then*)
							       else if (not (has_return (create_compound_st st2))) && ((has_return  (create_compound_st st1))
                                                                                                          && (has_function (create_compound_st st1))) then

                                                                 let old_list = create_if_list  lbl_list t in
                                                                 ((find_startline_no (create_compound_st st1)),
                                                                  (create_compound_st st1))::old_list


							      else let old_list = create_if_list  lbl_list t in old_list
								)
                              
            | Ast_c.Selection  (Ast_c.Switch (e, st)) ->if(has_inner_block st) then 
                                                              let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                              let old_list =  create_if_list  lbl_list t in
							          new_list@old_list
                                                        else let old_list = create_if_list  lbl_list t in old_list
            | Ast_c.Iteration  (Ast_c.While (e, st))->if(has_inner_block st) then
                                                              let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                              let old_list =  create_if_list  lbl_list t in
                                                                  new_list@old_list
	                                              else let old_list = create_if_list  lbl_list t in old_list
            | Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->if(has_inner_block st) then
                                                              let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                              let old_list =  create_if_list  lbl_list t in
                                                                  new_list@old_list
	                                                 else let old_list = create_if_list  lbl_list t in old_list
            | Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->if(has_inner_block st) then
                                                                                          let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                                                          let old_list =  create_if_list  lbl_list t in
                                                                                              new_list@old_list
	                                                                               else let old_list = create_if_list  lbl_list t in old_list
            | Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> if(has_inner_block st) then
                                                                      let new_list = (create_if_list  lbl_list (create_compound_st st)) in
                                                                      let old_list =  create_if_list  lbl_list t in
                                                                                              new_list@old_list
                                                                   else let old_list = create_if_list  lbl_list t in old_list
            | _ ->let old_list = create_if_list  lbl_list t in old_list
          )



(* Divide whole function body one part contain XXXX other part contain all top level code at end of the function  *)

					       
let rec create_init_lbl_prog_list prog_list init_lbl_list  = function
    []-> (prog_list,init_lbl_list)
  | h::t-> (match Ast_c.unwrap h with
              Ast_c.Labeled (Ast_c.Label (name, st)) ->if (List.length init_lbl_list)>0 then
		                                          (match Ast_c.unwrap st with
                                                           |     Ast_c.ExprStatement (Some e) -> 
							         create_init_lbl_prog_list prog_list (init_lbl_list@[h]) t
							   |	 Ast_c.Jump (Ast_c.Goto name) -> (prog_list, (init_lbl_list@[h]))
							   |	 Ast_c.Jump (Ast_c.Return) -> (prog_list, (init_lbl_list@[h]))
							   |	 Ast_c.Jump (Ast_c.ReturnExpr e) -> (prog_list, (init_lbl_list@[h]))
							   |	 Ast_c.Decl decl -> create_init_lbl_prog_list prog_list (init_lbl_list@[h]) t
							   |	 _ -> (prog_list,[])
							  )
		                                       else (prog_list,init_lbl_list)
                                                        
            | Ast_c.ExprStatement (Some e) -> create_init_lbl_prog_list prog_list (init_lbl_list@[h]) t
	    | Ast_c.Jump (Ast_c.Goto name) -> (prog_list, (init_lbl_list@[h]))
            | Ast_c.Jump (Ast_c.Return) -> (prog_list, (init_lbl_list@[h]))
	    | Ast_c.Jump (Ast_c.ReturnExpr e) -> (prog_list, (init_lbl_list@[h]))
	    | Ast_c.Decl decl -> create_init_lbl_prog_list prog_list (init_lbl_list@[h]) t
            | _ -> create_init_lbl_prog_list (prog_list@init_lbl_list@[h]) [] t
           ) 


(* Check whether function call uses any ID *)

let rec check_arguments = function
  []-> None
  | h::t-> (match Ast_c.unwrap h with
               Common.Left es -> (match  es with
                                   (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii) ->
				                 if id =~ "^[a-z_][a-z_0-9]*$" then 
				                          Some (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)
                                                 else check_arguments t
                                  | _ -> check_arguments t )
             | _ -> check_arguments t 
           )


let rec check_arguments_bug args_list = function
     []-> args_list
  |  h::t-> (match Ast_c.unwrap h with
               Common.Left es -> (match  es with
                                   (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii) ->
                                                 if id =~ "^[a-z_][a-z_0-9]*$" then
                                             (check_arguments_bug (args_list@[Some (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)])t)
                                                 else check_arguments_bug args_list t

                                  |  _ -> check_arguments_bug args_list t )
             |  _ -> check_arguments_bug args_list t
           )



(* find return Id and return type from function, Input is (prog@if_list) *)

let rec find_return_id_inner  = function
   [] -> None
 | h::t -> ( match Ast_c.unwrap h with
               Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)) ->
                                                 if id =~ "^[a-z_][a-z_0-9]*$" then
                                                          Some (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)
						 else find_return_id_inner t

              | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.FunCall  (e, args)), typ), ii)) ->  (match (check_arguments args) with None-> find_return_id_inner t
                                                                                                                          |  Some a-> Some a)
		                                                                            		                                                                         
              | Ast_c.Labeled (Ast_c.Label (name, st)) -> (match Ast_c.unwrap st with 
		                                              Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (ident)), typ), ii)) -> Some (((Ast_c.Ident (ident)), typ), ii)
                                                 	    | Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.FunCall  (e, args)), typ), ii)) -> 
								          (match (check_arguments args) with None-> find_return_id_inner t
												                                 | Some a-> Some a)								                                     												  
		                                            | _ -> find_return_id_inner t 
		                                           )
              | _ -> find_return_id_inner t  
           )

let rec find_return_id = function
   []-> None
  | h::t-> (match (find_return_id_inner h) with None -> find_return_id t | Some a -> Some a)



(* Marge If list *)

let rec marge_if_list = function
  []-> []
  |(a,b)::t-> b@(marge_if_list t)




(* Update if_list for changing return -ENOMEM to ret = -ENOMEM; return ret and return ERR_PTR(-ENOMEM) to ret = ERR_PTR(-ENOMEM) ; return ret *) 

let rec update_if_list_cng_return_inner return_id_str newlist = function
   [] -> newlist
 | h::t -> (match  h with
               (Ast_c.Jump (Ast_c.ReturnExpr((((Ast_c.Unary(e,Ast_c.UnMinus)), typ), ii) )),i)  -> 
                            let exp = (Ast_c.Unary(e, Ast_c.UnMinus)) in              
			    assignment:=!assignment+1;
			    update_if_list_cng_return_inner return_id_str ((create_assignment_st typ 
									      return_id_str exp)::newlist@[(Ast_c.Jump (Ast_c.ReturnExpr 
															  (((Ast_c.Ident (return_id_str)), typ), [])), i)]) t
            |  (Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.FunCall((((Ast_c.Ident (Ast_c.RegularName(s,ii3))), typ1), ii1), es)), typ2), ii2)),i) ->
                            let exp = (Ast_c.FunCall((((Ast_c.Ident (Ast_c.RegularName(s,ii3))), typ1), ii1), es)) in
			    assignment:=!assignment+1;
			    update_if_list_cng_return_inner return_id_str ((create_assignment_st typ2 
									      return_id_str exp)::newlist@[(Ast_c.Jump (Ast_c.ReturnExpr 
															  (((Ast_c.Ident (return_id_str)), typ2), [])), i)]) t
            |  (Ast_c.Jump (Ast_c.ReturnExpr (((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ), ii)),i) ->
                                                                                                      (* if id =~ "NULL" then*)
		            if not (compare_name (Ast_c.RegularName(id,ii2)) return_id_str) then
			       let exp = Ast_c.Ident (Ast_c.RegularName(id,ii2)) in
			       assignment:=!assignment+1;
                               update_if_list_cng_return_inner return_id_str ((create_assignment_st typ 
									      return_id_str exp)::newlist@[(Ast_c.Jump (Ast_c.ReturnExpr 
								              (((Ast_c.Ident (return_id_str)), typ), [])), i)]) t 
                            else  update_if_list_cng_return_inner return_id_str (newlist@[h]) t                           
            | _ -> update_if_list_cng_return_inner return_id_str (newlist@[h]) t
           )


let update_if_list_cng_return return_id if_list = 
   let rec update_if_list_cng_return_loop = function
       [] -> []
     | (a,b)::t -> 

	           let return_id_str = (match return_id with 
		                         None -> Ast_c.RegularName("rett",[])
		                       | Some (((Ast_c.Ident (Ast_c.RegularName(s,ii2))), typ), ii)-> if(is_return_id_used return_id (skip_return b)) then 
					                                                                   Ast_c.RegularName("rett",ii2) 
				                                                                      else Ast_c.RegularName(s,ii2)
                                        )
                   in (a, (update_if_list_cng_return_inner return_id_str [] b))::(update_if_list_cng_return_loop t)
   in update_if_list_cng_return_loop if_list






(* Create outer statement of inner if statement *)

let create_outer_stmt inner_prog inner_prog2 = function 
    (Ast_c.Selection  (Ast_c.Switch (e, st)),ii) ->
         (Ast_c.Selection(Ast_c.Switch(e,(Ast_c.Compound(stmtEle_add_list (inner_prog)),[]))),[])
  | (Ast_c.Selection  (Ast_c.If (e, st1, st2)),ii) ->          
         (Ast_c.Selection (Ast_c.If (e,(Ast_c.Compound(stmtEle_add_list (inner_prog)),[]), 
                                         inner_prog2)),[])
  | (Ast_c.Iteration  (Ast_c.While (e, st)),ii) ->
         (Ast_c.Iteration(Ast_c.While(e,(Ast_c.Compound(stmtEle_add_list (inner_prog)),[]))),[])
  | (Ast_c.Iteration  (Ast_c.DoWhile (st, e)),ii) ->
         (Ast_c.Iteration(Ast_c.DoWhile((Ast_c.Compound(stmtEle_add_list (inner_prog)),[]),e)),[])
  | (Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)),ii) ->
         (Ast_c.Iteration(Ast_c.For((e1opt,il1),(e2opt,il2),(e3opt, il3),(Ast_c.Compound(stmtEle_add_list (inner_prog)),[]))),[])
  | (Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)),ii)->
         (Ast_c.Iteration  (Ast_c.MacroIteration (s,es,(Ast_c.Compound(stmtEle_add_list (inner_prog)),[]))),[])
  

let rec create_lbl_prog = function
    []-> []
  | h::t -> (match Ast_c.unwrap h with 
                 Ast_c.Labeled (Ast_c.Label (name, st)) -> h::t
              | _ -> create_lbl_prog t
            )


let create_new_lbl lbl_no =
       ("out"^(string_of_int (lbl_no+1)))




let rec update_for_harder function_return_id new_lbl_name lbl_code new_lbl_prog = function
   []-> (match function_return_id with None -> new_lbl_prog@[(Ast_c.Jump (Ast_c.Return),[])]@(create_label_sts new_lbl_name lbl_code)
                                    | Some a ->  (create_label_sts new_lbl_name lbl_code)@new_lbl_prog)

  |   h::t->(match Ast_c.unwrap h with
          Ast_c.Labeled (Ast_c.Label (name, st)) -> (match Ast_c.unwrap st with
          |    Ast_c.Jump (Ast_c.Goto name) ->   new_lbl_prog@[h]@(create_label_sts new_lbl_name lbl_code)@t
          |    Ast_c.Jump (Ast_c.Return) -> new_lbl_prog@[h]@(create_label_sts new_lbl_name lbl_code)@t
          |    Ast_c.Jump (Ast_c.ReturnExpr e) -> new_lbl_prog@[h]@(create_label_sts new_lbl_name lbl_code)@t
          |    _ -> update_for_harder function_return_id new_lbl_name lbl_code (new_lbl_prog@[h]) t
                                                    )
    |   Ast_c.Jump (Ast_c.Goto name) -> new_lbl_prog@[h]@(create_label_sts new_lbl_name lbl_code)@t
    |   Ast_c.Jump (Ast_c.Return) -> new_lbl_prog@[h]@(create_label_sts new_lbl_name lbl_code)@t
    |   Ast_c.Jump (Ast_c.ReturnExpr e) -> new_lbl_prog@[h]@(create_label_sts new_lbl_name lbl_code)@t
    |    _ -> update_for_harder function_return_id new_lbl_name lbl_code (new_lbl_prog@[h]) t
         )

let rec is_harder lbl_code lbl_list  = function
    [] -> Some Harder
  |   (label, code)::t-> if (not (is_intersect  lbl_code code)) then
                          is_harder lbl_code lbl_list t
                        else None


let rec set_harder function_return_id lbl_prog lbl_list lbl_no new_if_list = function
    []-> (new_if_list, lbl_prog, lbl_no , lbl_list)
  |(line_no, if_code, lbl_code,status)::t-> (match status with 
                                               Incomplete ->
                                                     (match (is_harder lbl_code lbl_list lbl_list) with
                                                            None-> set_harder function_return_id lbl_prog lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
                                                          | Some Harder -> 
							                  if_branch_applied:= !if_branch_applied + 1;
                                                                           harder_count:= !harder_count+1;
                                                                           added_goto:= !added_goto+1;
							                   label_added:= !label_added+1;
							                   harder_function:= !harder_function +1;
							                   line_moved:= !line_moved + (List.length lbl_code);
                                                                           let new_lbl = create_new_lbl lbl_no in 
					                                   (*let new_lbl_prog = update_for_harder function_return_id new_lbl lbl_code [] lbl_prog in*)
                                                                           let new_lbl_prog = lbl_prog in
                                                                           let new_lbl_list = ((create_id_name new_lbl), lbl_code)::lbl_list in
                                                                           harder_list:= !harder_list@[lbl_code];
						                           let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl)),[])]) in
                                                set_harder function_return_id new_lbl_prog new_lbl_list (lbl_no+1) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t 
                                             )  
                                             |  Complete -> 
						 
						 set_harder function_return_id lbl_prog lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
                                            )

(* problem initial lbl code with lbl code *)

let rec update_simple_hard_end_code  lbl_list new_lbl lbl_code =  function
     []->[]
  |  h::t-> 
            if (compare_stmts_list lbl_code  (h::t)) then
               (create_label_sts new_lbl (h::t))
            else h::(update_simple_hard_end_code  lbl_list new_lbl lbl_code t)   


let rec set_simple_hard_end_code init_lbl_list lbl_list lbl_no new_if_list = function 
    []-> (new_if_list, init_lbl_list , lbl_no , lbl_list)
  | (line_no, if_code, lbl_code,status)::t->(match status with
                                              Incomplete ->
                                                if  (is_suffix  lbl_code  init_lbl_list) then
                                                 (
                                                   if_branch_applied:= !if_branch_applied + 1;
                                                   hard_count:= !hard_count+1;
                                                   hard_function:= !hard_function + 1;
                                                   added_goto:=!added_goto+1;
                                                   line_reduced:=!line_reduced+(List.length lbl_code);					      
                                                   label_added:= !label_added+1;
                                                   let new_lbl = create_new_lbl lbl_no in
                                                   (*let new_init_lbl_list = update_simple_hard_end_code lbl_list new_lbl lbl_code init_lbl_list in*)
					           let new_init_lbl_list = init_lbl_list in
                                                   let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl)),[])]) in
                                                   set_simple_hard_end_code init_lbl_list lbl_list (lbl_no+1) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t
                                                 ) 
                                                else set_simple_hard_end_code init_lbl_list lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
                                             | Complete -> set_simple_hard_end_code init_lbl_list lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
                                            )

let rec is_simple lbl_code lbl_list = function
   []-> None
  | (lbl,code)::t->(* print_string(" lbl_code Code :");print_string(string_of_int(List.length lbl_code));*)
                   (* Printf.printf "%s       \n\n\n\n" (Dumper.dump lbl_code);*)
                   (* print_string(" Code :");print_string(string_of_int(List.length code));*)
                   (* Printf.printf "%s       \n\n\n\n" (Dumper.dump code);*)
                    if (compare_stmts_list  lbl_code  code) then
                        Some lbl
                    else is_simple lbl_code lbl_list t



let rec set_simple  lbl_list  new_if_list = function
    []-> (new_if_list, lbl_list)
  | (line_no, if_code, lbl_code,status)::t-> (match status with
                                               Incomplete ->
                                                  (match (is_simple lbl_code lbl_list lbl_list) with
                                                    None-> set_simple lbl_list ((line_no,if_code,lbl_code,status)::new_if_list) t
						  | Some a->  
      						          if_branch_applied:= !if_branch_applied + 1;
						         simple_count:= !simple_count+1;
						         simple_function:= !simple_function + 1;
						         added_goto:=!added_goto+1;
                                                         line_reduced:= !line_reduced+(List.length lbl_code);
						      
						      let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto a),[])])in
        (*print_string(" IF Code :");print_string(string_of_int(List.length new_if_code));*)
    (*    Printf.printf "%s" (Dumper.dump new_if_code);*)
  (*      print_string(" LBL CODE   :");print_string(string_of_int(List.length lbl_code));*)
(*        Printf.printf "%s" (Dumper.dump lbl_code);*)

                                                        set_simple  lbl_list ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t
                                             )
    |   Complete -> set_simple lbl_list ((line_no,if_code,lbl_code,status)::new_if_list) t
                                            )


let rec update_for_hard_inner new_lbl lbl_code lbl_prog  =
  match (lbl_code,lbl_prog) with
  ([],[])-> []
  | (h::t, h1::t1) -> if (compare_stmts h h1) then (create_label_sts new_lbl (h1::t1))
                      else h1::(update_for_hard_inner new_lbl (h::t) t)

let rec update_for_hard old_lbl new_lbl lbl_code = function
  []->[]
  | h::t-> (match Ast_c.unwrap h with
                Ast_c.Labeled (Ast_c.Label (name, st)) -> if(compare_name name old_lbl ) then h::(update_for_hard_inner new_lbl lbl_code t)
		                                          else  h::(update_for_hard old_lbl new_lbl lbl_code t)
              | _ -> h::(update_for_hard old_lbl new_lbl lbl_code t) 
  
           )

let rec is_hard lbl_code lbl_list = function
  []->  None
  | (lbl,code)::t-> if(is_suffix lbl_code code) then
                       Some lbl
                    else is_hard lbl_code lbl_list t


let rec set_hard lbl_prog lbl_list lbl_no new_if_list = function
 []-> (new_if_list, lbl_prog, lbl_no , lbl_list)
  | (line_no, if_code, lbl_code,status)::t-> (match status with
                            Incomplete ->

                                         (match (is_simple lbl_code lbl_list lbl_list) with
                                                 None-> ( match (is_hard lbl_code lbl_list lbl_list) with
						            None-> set_hard lbl_prog lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
						          | Some a ->
                                                               if_branch_applied:= !if_branch_applied + 1;
                                                               hard_count:= !hard_count+1;
                                                               added_goto:=!added_goto+1;
                                                               line_reduced:=!line_reduced+(List.length lbl_code);
                                                               label_added:= !label_added+1;
							       let new_lbl = create_new_lbl lbl_no in
                                                               let new_lbl_prog = lbl_prog in
                                                               let new_lbl_list = ((create_id_name new_lbl), lbl_code)::lbl_list in
                                                               let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl)),[])]) in
                                                               set_hard new_lbl_prog new_lbl_list (lbl_no+1) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t
						         )
                                                | Some a ->let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto a),[])])in
						           set_hard lbl_prog lbl_list (lbl_no+1) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t

                                         )
(*                                                (Some a, _)-> let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto a),[])])in*)
(*						              set_hard new_lbl_prog new_lbl_list (lbl_no+1) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t*)
(*                                              | (None,None) -> set_hard lbl_prog lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t*)
(*			                      | (None,Some a) -> print_string("**********Hard***********");*)
(*                                                         if_branch_applied:= !if_branch_applied + 1;*)
(*                                                         hard_count:= !hard_count+1;*)
(*                                                         added_goto:=!added_goto+1;*)
(*						         line_reduced:=!line_reduced+(List.length lbl_code);*)
(*						         label_added:= !label_added+1;*)
						         
(*						  let new_lbl = create_new_lbl lbl_no in*)

(*                                                  let new_lbl_prog = lbl_prog in*)
(*                                                  let new_lbl_list = ((create_id_name new_lbl), lbl_code)::lbl_list in*)
(*                                                  let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl)),[])]) in*)
(*                                                  set_hard new_lbl_prog new_lbl_list (lbl_no+1) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t *)
                                          

                        |   Complete ->  set_hard lbl_prog lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
                                          )


let rec split new_lbl_code old_lbl_code lbl_list code = function
    []-> (new_lbl_code, old_lbl_code)
  |  h::t-> if(is_suffix  t code ) then ((new_lbl_code@[h]),(old_lbl_code@t))
           else split (new_lbl_code@[h]) old_lbl_code lbl_list code t

let rec is_hardest lbl_code lbl_list new_lbl_code old_lbl_code = function
   [] -> (new_lbl_code,old_lbl_code)
  |  (label,code)::t -> if ((is_intersect lbl_code  code))
                           && (not (is_suffix lbl_code code) ) then
                             let (update_new_lbl, update_old_lbl) = split [] []  lbl_list code lbl_code in
                             let  (new_lbl_code,old_lbl_code) = 
                                      (if (List.length update_old_lbl) > (List.length old_lbl_code) then (update_new_lbl, update_old_lbl)
                                       else (new_lbl_code,old_lbl_code)) in
                                  is_hardest lbl_code lbl_list new_lbl_code old_lbl_code t
                        else is_hardest lbl_code lbl_list new_lbl_code old_lbl_code t

let rec find_return_prec = function
  []-> false
  | h::t-> (match Ast_c.unwrap h with
               Ast_c.Labeled (Ast_c.Label (name, st)) -> (match Ast_c.unwrap st with
		                                            Ast_c.Jump (Ast_c.Return) -> true
	                                                  | Ast_c.Jump (Ast_c.ReturnExpr e) -> true
							  | _  -> find_return_prec t
		                                         )
             | Ast_c.Jump (Ast_c.Return) -> true
             | Ast_c.Jump (Ast_c.ReturnExpr e) -> true
             | _  -> find_return_prec t
           )



let rec set_hardest function_return_id lbl_prog lbl_list lbl_no new_if_list = function
 []-> (new_if_list, lbl_prog, lbl_no , lbl_list)
  |  (line_no, if_code, lbl_code,status)::t-> (match status with
                            Incomplete -> let (new_lbl_code,old_lbl_code)= is_hardest lbl_code lbl_list [] [] lbl_list in

                                           (match (new_lbl_code,old_lbl_code) with
                                                (h2::t2, h1::t1) ->
                                                     if (List.length old_lbl_code)< 2 then
						      ( 
                                                         let added_goto_mark = if(List.length lbl_prog)=0 then true else false in
                                                           let added_goto_mark =
                                                              (if  added_goto_mark = false then  find_return_prec lbl_prog
                                                               else added_goto_mark
                                                              )in
						         if_branch_applied:= !if_branch_applied + 1;
                                                         hardest_count:= !hardest_count+1;
							 hardest_harder:= !hardest_harder+1;
							 harder_function:= !harder_function + 1;
                                                         added_goto:=!added_goto+1;
                                                         label_added:=!label_added+1;
							 if(added_goto_mark) then
							   (line_reduced:=!line_reduced+(List.length old_lbl_code);
                                                           line_moved:=!line_moved + (List.length new_lbl_code);)
							 else
                                                           line_moved:=!line_moved + (List.length lbl_code);
                                                       let new_lbl = create_new_lbl lbl_no in
						       (*let new_lbl_prog = update_for_harder function_return_id new_lbl lbl_code [] lbl_prog in*)
                                                       let new_lbl_prog = lbl_prog in
                                                       let new_lbl_list = ((create_id_name new_lbl), (new_lbl_code@old_lbl_code))::lbl_list in
                                                       let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl)),[])]) in
                                            set_hardest function_return_id new_lbl_prog new_lbl_list (lbl_no+1) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t

                                                       )
                                                     else
						       (match (is_simple old_lbl_code lbl_list lbl_list) with
							  None -> (
                                                                     match (is_hard old_lbl_code lbl_list lbl_list) with
                                                                        None->set_hardest function_return_id lbl_prog lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
                                                                      | Some a ->(
                                                                              if_branch_applied:= !if_branch_applied + 1;
                   							      hardest_count:= !hardest_count+1;
									      line_moved:=!line_moved + (List.length new_lbl_code);
                                                                              added_goto:=!added_goto+2;
                                                                              line_reduced:=!line_reduced+(List.length old_lbl_code);
									      label_added:= !label_added+2;
									      hardest_hard:= !hardest_hard+1;
									      hardest_function:= !hardest_function + 1;
                                                                              let new_lbl1 = create_new_lbl lbl_no in
                                                                              let new_lbl2 = create_new_lbl (lbl_no+1) in
                (* let new_lbl_prog1 = update_for_harder function_return_id new_lbl1 (new_lbl_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl2)),[])]) [] lbl_prog in*)
                 (*let new_lbl_prog = update_for_hard  a new_lbl2  lbl_code new_lbl_prog1 in                      *)
                  let new_lbl_prog = lbl_prog in
                                             let new_lbl_list1 = ((create_id_name new_lbl1), (new_lbl_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl2)),[])]))::lbl_list in
                                             let new_lbl_list = ((create_id_name new_lbl2), old_lbl_code)::new_lbl_list1 in
                                             let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl1)),[])]) in
                                              set_hardest function_return_id  new_lbl_prog new_lbl_list (lbl_no+2) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t
					                                         )
                                                                    )

						       |  Some a -> (
                                                                     if_branch_applied:= !if_branch_applied + 1;
                                                                     hardest_count:= !hardest_count+1;
							              line_moved:=!line_moved + (List.length new_lbl_code);
							             let added_goto_mark = if(List.length lbl_prog)=0 then true else false in
								     let added_goto_mark =
								       (if  added_goto_mark = false then  find_return_prec lbl_prog 
								        else added_goto_mark
								       )in
							              added_goto:=   
							                   (if (added_goto_mark) then !added_goto+1
							                    else !added_goto+2);
                                                                     
							             line_reduced:=!line_reduced+(List.length old_lbl_code);
                                                                     label_added:= !label_added+1;
								     hardest_simple:= !hardest_simple+1;
								     hardest_function:= !hardest_function + 1;
							              let new_lbl = create_new_lbl lbl_no in
(*                       let new_lbl_prog = update_for_harder function_return_id new_lbl (new_lbl_code@[(Ast_c.Jump (Ast_c.Goto a),[])]) [] lbl_prog in*)
								      let new_lbl_prog = lbl_prog in
                                                                      let new_lbl_list = ((create_id_name new_lbl), new_lbl_code)::lbl_list in
								      let new_if_code = (if_code@[(Ast_c.Jump (Ast_c.Goto (create_id_name new_lbl)),[])]) in
					      set_hardest function_return_id  new_lbl_prog new_lbl_list (lbl_no+1) ((line_no, new_if_code, lbl_code,Complete)::new_if_list) t
							            )
                                                       )
                                                       
                                       
                                              | (_,_) -> set_hardest function_return_id lbl_prog lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
                                                

                                          )

                        |    Complete -> set_hardest function_return_id lbl_prog lbl_list lbl_no ((line_no,if_code,lbl_code,status)::new_if_list) t
                                            )


let rec count_assignment_inner = function
    []-> 0
  | h::t -> (match h with 
                 (Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)),ii2) -> if (List.length ii2)=0 then (0)
		                                                                                  else count_assignment_inner t
                | _ -> count_assignment_inner t 
            ) 



let rec count_assingment  = function
      []-> []
  |   (line_no, if_code, lbl_code,status)::t-> (count_assignment_inner if_code)::(count_assingment t)

let rec chk_return_id_if return_id = function
  []-> false
  | h::t-> (match Ast_c.unwrap h with
               Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.Ident (ident)), typ), ii), op, e2)), typ1), ii1)) ->
		    if(compare_name ident return_id) then true else chk_return_id_if return_id t
            | _ -> chk_return_id_if return_id t 
           )
                           
let rec count_return_cng return_id = function 
    []-> !count_return_apply
  | (line_no,if_code,lbl_code,status)::t -> if (chk_return_id_if return_id if_code) then
                                                (
                                                  count_return_apply:= !count_return_apply+1;
                                                  count_return_cng return_id t
                                                )
                                           else (
                                                  count_return_apply := !count_return_apply;
                                                  count_return_cng return_id t
                                                )

let rec find_function_return_id = function
  []->None
  | h::t-> (match Ast_c.unwrap h with
                Ast_c.Labeled (Ast_c.Label (name, st)) ->None
              | Ast_c.Jump (Ast_c.Goto name) -> Some h
              | Ast_c.Jump (Ast_c.Return) -> Some h
              | Ast_c.Jump (Ast_c.ReturnExpr e) -> Some h
              |	_ -> find_function_return_id t
           )
               

let rec check_f_bug h new_f_list f_list = function
  []-> new_f_list
  | h1::t1 -> if(compare_stmts h h1) then (if (compare_stmts (List.hd f_list) h1) then check_f_bug h new_f_list f_list t1
                                           else (unordr_rels_mark:=1; check_f_bug h new_f_list f_list t1)
                                          )
              
              else check_f_bug h (new_f_list@[h1]) f_list t1

let rec check_missing_inner h = function
  []-> false
  | h1::t1 -> if(compare_stmts h h1) then true
              else check_missing_inner h t1




let rec prepare_st2 = function
  []->[(Ast_c.ExprStatement (None),[])]
  | h::t-> (match Ast_c.unwrap h with
            |  Ast_c.ExprStatement (None) -> [(Ast_c.ExprStatement (None),[])]
            | _ -> (h::t)
           )




let rec args_exists_inner h = function
  []-> false
  | h1::t1-> if (is_return_id_used_inner h1 h) then true
             else args_exists_inner h t1

let rec args_exists lbl_code = function
  []-> true
  | h::t-> if(args_exists_inner h lbl_code) then ((*print_string("\nExists \n");*)args_exists lbl_code t)
           else false


let  assing_inside_expr_inner h  = function 
   
   (((Ast_c.Assignment (e1, op, e2)), typ), ii) ->  if(compare_exp e1 h) then true else false
   |  _-> false

let rec assing_inside_expr e = function
  []-> false
  | (Some h)::t -> if(assing_inside_expr_inner h e) then true
            else assing_inside_expr  e t

let rec arg_used_for_assign arg line_no args = function
  []->false
  | h::t -> let start_line = find_startline_no (create_compound_st h) in
            let end_line = find_endline_no (create_compound_st h) in 
            if start_line > line_no then false
            else (   
                     match Ast_c.unwrap h with
                        
                     | Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) -> if(is_return_id_used_inner  h  arg) && 
		                                                                               ((List.length args) =  (List.length (check_arguments_bug [] es))) then true 
			                                                                   else  arg_used_for_assign arg line_no args t
                     | Ast_c.ExprStatement (Some ((( Ast_c.Assignment (e1, op, e2)), typ), ii)) ->(match arg with
			                                                                            Some a->
			                                                                                  if (compare_exp e1 a) then true
			                                                                                  else arg_used_for_assign arg line_no args t
		                                                                                  | None -> arg_used_for_assign arg line_no args t
												  )
                     | Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->(*print_string("\nGGGGGG\n"); print_string(string_of_int line_no); print_string(string_of_int start_line);*)

			                                           let start_line_st1 = find_startline_no (create_compound_st st1) in
		                                                   let end_line_st1   = find_endline_no (create_compound_st st1) in
(*print_string(string_of_int start_line_st1);*)
								   let (start_line_st2, end_line_st2) =
								     if(has_multiple_st (create_compound_st st2)) then 
								          (find_startline_no (create_compound_st st2),find_endline_no (create_compound_st st2)) 
								     else
								          (find_startline_no (create_compound_st st1),
									  find_endline_no (create_compound_st st1))
								   in
                                                                   if line_no = start_line_st1 then false 
								   else if (assing_inside_expr e args) then true  
								   else if line_no > start_line_st1 && line_no <= end_line_st1 then
								           arg_used_for_assign arg line_no args (create_compound_st st1)
								   else if line_no > start_line_st2 && line_no <= end_line_st2 then
								           arg_used_for_assign arg line_no args (create_compound_st st2)
								   else arg_used_for_assign arg line_no args t

                     | Ast_c.Selection  (Ast_c.Switch (e, st)) -> if line_no > start_line && line_no <= end_line then
                                                                        arg_used_for_assign arg line_no args (create_compound_st st)
                                                                  else arg_used_for_assign arg line_no args t

                     |  Ast_c.Iteration  (Ast_c.While (e, st)) -> if line_no > start_line && line_no <=end_line then
			                                                arg_used_for_assign arg line_no args (create_compound_st st)
			                                          else arg_used_for_assign arg line_no args t 

		     |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) -> if line_no > start_line && line_no <=end_line then
                                                 		       arg_used_for_assign arg line_no args (create_compound_st st)
		                                                    else arg_used_for_assign arg line_no args t

		     |  Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->  if line_no > start_line && line_no <=end_line then
	                                                                                  		 arg_used_for_assign arg line_no args (create_compound_st st)
  			                                                                           else arg_used_for_assign arg line_no args t

		     |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> if line_no > start_line && line_no <=end_line then
                                                           			 arg_used_for_assign arg line_no args (create_compound_st st)
			                                                     else arg_used_for_assign arg line_no args t


                     | _ -> arg_used_for_assign arg line_no args t
                
                 )



let rec any_arg_used_for_assign line_no prog args  = function
    []->(*print_string("\n\n\nEnd\n\n\n");*)false
  | h::t -> if (arg_used_for_assign h line_no  args prog) then ((*print_string("\n\n\nStart\n\n\n");*)true)
            else any_arg_used_for_assign line_no prog  args t

let rec any_arg_used_for_return_inner h = function
  []-> false
  | h1::t1-> (match Ast_c.unwrap h1 with
                Ast_c.Jump (Ast_c.ReturnExpr e) ->  if(is_return_id_used_inner h1 h) then true else false
               | _-> any_arg_used_for_return_inner h t1
              ) 

let rec any_arg_used_for_return if_brnch = function
    []-> false
  |  h::t -> (match Ast_c.unwrap if_brnch with
                Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> if(any_arg_used_for_return_inner h (create_compound_st st1)) then true
		                                             else false
               | _ -> false 

              )  

let rec any_arg_used_for_exp if_brnch = function
      []-> false
  |   (Some h)::t -> (match Ast_c.unwrap if_brnch with
                Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> if(is_return_id_in_expr e h) then true
                                                             else any_arg_used_for_exp if_brnch t
               |  _ -> false

              )


let  satisfy_condition if_brnch line_no prog h lbl_code= 
  match Ast_c.unwrap h with
    Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii)) ->(*print_string("\nargument list length \n");*)
(*Printf.printf "%s" (Dumper.dump (check_arguments_bug [] args));*)
  (*                                                                      print_string(string_of_int (List.length (check_arguments_bug [] args)));*)
                                                                if (List.length (check_arguments_bug [] args)) < 1 then false
                                                                else if (args_exists lbl_code (check_arguments_bug [] args)) then ((*print_string("\nGGGGGG\n");*)false)
                                                                else if (any_arg_used_for_assign line_no prog 
                                                                        (check_arguments_bug [] args)  (check_arguments_bug [] args)) = false then false
								else if (any_arg_used_for_return if_brnch (check_arguments_bug [] args)) then false
                                                                else if (any_arg_used_for_exp if_brnch (check_arguments_bug [] args)) then false
                                                                else if (any_arg_used_for_assign line_no prog
                                                                        (check_arguments_bug [] args)  (check_arguments_bug [] args)) = true then true
	                                                        else true
  | _ -> false

let rec check_missing_st if_brnch line_no prog iifunc1 missing_st useless_st lbl_code = function
  []-> (missing_st,useless_st)
  | h::t -> if (check_missing_inner h lbl_code) then ((*print_string("\nCheck missing st inner true \n");*)(check_missing_st if_brnch line_no prog  iifunc1 missing_st useless_st lbl_code t))
            else ((*print_string("\nCheck missing st inner false \n");*)
                    if(satisfy_condition if_brnch line_no prog h lbl_code) then ( (*print_string("\nSatisfy condition true \n");*)
                       let start_line = find_startline_no (create_compound_st h) in
                       
                       let filename = Ast_c.file_of_info iifunc1 in
                       let message = "Missing stmt" in
                       let lcol = find_lcol (create_compound_st h) in
                       let rcol = find_rcol (create_compound_st h) in
                       bug_count := !bug_count + 1;
            (*           if(!x = start_line) then *)
            (*                 print_string("** TODO [[")*)
            (*           else ( *)
            (*                 print_string("* TODO [[");*)
            (*                 x:= start_line;*)
            (*                );*)
            (*           print_string(filename);*)
            (*           print_string("::face=ovl-face1::linb=");*)
            (*           print_string(string_of_int(line_no));*)
            (*           print_string("::colb=");*)
            (*           print_string(string_of_int(lcol));*)
            (*           print_string("::cole=");*)
            (*           print_string(string_of_int(rcol));*)
            (*           print_string("][");*)
            (*           print_string(message);*)
            (*           print_string(" ");*)
            (*           print_string(filename);*)
            (*           print_string("::");*)
                      
            (*           print_string(string_of_int(start_line));*)
            (*           print_string("\n");*)


                       (*print filename start_line lcol rcol message;*)
                       (*print_string("\nMissing Bug \n");*)
                       check_missing_st if_brnch line_no prog iifunc1 (missing_st@[h]) useless_st lbl_code t
                     )
                    else ((*print_string("\nSatisfy condition false \n");*)(check_missing_st if_brnch line_no prog iifunc1 (missing_st@[h]) (useless_st@[h]) lbl_code t))
                 )

let rec update_f_list_inner new_f_list f_list = function
  []-> new_f_list
  | h::t -> if (compare_stmts_list (h::t) f_list) then ( new_f_list@(h::t))
            else update_f_list_inner (new_f_list@[h]) f_list t 


let rec update_f_list_inner_nonmissing missing_st  f_list = function
  []-> (missing_st@f_list)
  |  h::t -> if (check_missing_inner h f_list) then update_f_list_inner_nonmissing missing_st f_list t
	     else update_f_list_inner_nonmissing  (missing_st@[h]) f_list  t


let rec remove_useless_st_inner new_f_list h = function
  []->new_f_list
  | h1::t1-> if(compare_stmts h h1) then remove_useless_st_inner new_f_list h t1
             else remove_useless_st_inner (new_f_list@[h1]) h t1  


let rec remove_useless_st  f_list = function
  []->f_list
  | h::t -> remove_useless_st (remove_useless_st_inner [] h f_list) t

let  update_f_list   if_brnch  prog line_no iifunc1 lbl_code f_list = 
if (is_suffix f_list lbl_code) then 
  (if !unordr_rels_mark = 1 then ((*print_string("\nUnorder Bug \n");*)
        
          let start_line =
                          find_startline_no (create_compound_st (List.hd f_list)) 
                    
          in
          let filename = Ast_c.file_of_info iifunc1 in
          let message = "release unorderly" in
          let lcol =
                        find_lcol (create_compound_st (List.hd f_list)) 
	            
          in
          let rcol = 
                        find_rcol (create_compound_st (List.hd f_list)) 
                   
          in

          bug_count := !bug_count + 1; 

	      (*   if(!x = start_line) then*)
              (*                print_string("** TODO [[")*)
              (*          else (  *)
              (*                print_string("* TODO [[");*)
              (*                x:= start_line;*)
              (*               );*)
    (* print_string(filename);*)
    (* print_string("::face=ovl-face1::linb=");*)
    (* print_string(string_of_int(line_no));*)
    (* print_string("::colb=");*)
    (* print_string(string_of_int(lcol));*)
    (* print_string("::cole=");*)
    (* print_string(string_of_int(rcol));*)
    (* print_string("][");*)
    (* print_string(message);*)
    (* print_string(" ");*)
    (* print_string(filename);*)
    (* print_string("::");*)
    (* print_string(string_of_int(start_line));*)
    (* print_string("\n");*)

          
          update_f_list_inner [] f_list lbl_code)
    else
      ((*print_string("Suffix and ordered"); print_string(string_of_int(List.length f_list));*)update_f_list_inner [] f_list  lbl_code)
  )
else (
       let (missing,useless) = check_missing_st if_brnch line_no prog iifunc1 [] [] lbl_code f_list in
       if(List.length missing = 0) then
         ( (*print_string("\nMissing\n");*)
         let start_line = 
                          find_startline_no (create_compound_st (List.hd f_list))
	           
	       in
          let filename = Ast_c.file_of_info iifunc1 in
          let message = "Not in Same order" in
          let lcol = 
            find_lcol (create_compound_st (List.hd f_list))
	         
	        in
          let rcol = 
            find_rcol (create_compound_st (List.hd f_list))
	        in
          (*print_string("\nUnorder Bug \n");*)
          bug_count := !bug_count + 1;
	(*         if(!x = start_line) then*)
        (*                      print_string("** TODO [[")*)
        (*                else ( *)
        (*                      print_string("* TODO [[");*)
        (*                      x:= start_line;*)
        (*                     );*)
        (*   print_string(filename);*)
        (*   print_string("::face=ovl-face1::linb=");*)
        (*   print_string(string_of_int(line_no));*)
        (*   print_string("::colb=");*)
        (*   print_string(string_of_int(lcol));*)
        (*   print_string("::cole=");*)
        (*   print_string(string_of_int(rcol));*)
        (*   print_string("][");*)
        (*   print_string(message);*)
        (*   print_string(" ");*)
        (*   print_string(filename);*)
        (*   print_string("::");*)
        (*   print_string(string_of_int(start_line));*)
        (*   print_string("\n");  *)


          
          update_f_list_inner_nonmissing [] f_list  lbl_code
         )
       else
         ((*print_string("\nNot Missing\n");*)update_f_list_inner_nonmissing   [] f_list  lbl_code)
         
     )
   

let rec whole_f_list  new_f_list old_f_list = function
  []-> (new_f_list@old_f_list) 
  | h::t-> if (check_missing_inner h old_f_list) then whole_f_list new_f_list old_f_list t
           else whole_f_list (new_f_list@[h]) old_f_list t
           



let find_bug_inner iifunc1 line_no lbl_code f_list prog =
unordr_rels_mark:= 0;
(*print_string("\nlbl_code length in inner  "); print_string(string_of_int(List.length lbl_code));*)
let old_f_list = f_list in
let rec find_bug_inner_loop f_list  = function
  []-> f_list
  | h::t-> let start_line = find_startline_no (create_compound_st h) in
           let end_line = find_endline_no (create_compound_st h) in
           
          (match Ast_c.unwrap h with
                Ast_c.Labeled (Ast_c.Label (name, st)) ->
		                                         if line_no > start_line && line_no <= end_line then
                                                              find_bug_inner_loop (find_bug_inner_loop f_list (create_compound_st st)) t
                                                         else find_bug_inner_loop f_list t


           |    Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) -> if line_no > start_line then (
						  
                                                  find_bug_inner_loop (check_f_bug  h [] f_list f_list) t)
                                              else f_list
           |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->   


(*print_string(string_of_int(List.length (create_compound_st (st2))));*)


if(has_multiple_st (create_compound_st st2)) then
(
                               if line_no = find_startline_no (create_compound_st st1) && (has_return_bug e (create_compound_st st1)) then  (

				        (*print_string ("\nif equal \n");*)
                                       (whole_f_list [] old_f_list (update_f_list h  prog line_no iifunc1 lbl_code f_list)) )
                            

                         else if line_no >find_endline_no (create_compound_st st1) && (not (has_return_all (create_compound_st st1))) &&
				 line_no >find_endline_no (create_compound_st st2) && (not (has_return_all (create_compound_st st2))) then
                                       let new_f_list1 = find_bug_inner_loop f_list (create_compound_st st1) in

				       let new_f_list2 = find_bug_inner_loop f_list (create_compound_st st2) in


				       if (compare_stmts_list new_f_list1 new_f_list2) then
					 ((*print_string ("\n>if >else if no return else no return \n");*)find_bug_inner_loop  new_f_list1 t)
				       else ((*print_string("\nProblem with if and else \n");*)new_f_list1)

			       else if line_no >find_endline_no (create_compound_st st1) && (has_return_all (create_compound_st st1))&&
                                  line_no >find_endline_no (create_compound_st st2) && (not (has_return_all (create_compound_st st2))) then
				      ( let new_f_list2 = find_bug_inner_loop f_list (create_compound_st st2) in
				         (*print_string ("\n>if >else  else no return \n");*)
                                         find_bug_inner_loop  new_f_list2 t)

                               else if line_no >find_endline_no (create_compound_st st1) && (not (has_return_all (create_compound_st st1)))&&
                                       line_no >find_endline_no (create_compound_st st2) && (has_return_all (create_compound_st st2)) then(
				        let new_f_list1 = find_bug_inner_loop f_list (create_compound_st st1) in
					(*print_string ("\n>if >else if no return  \n");*)
                                         find_bug_inner_loop  new_f_list1 t)
                             
	                       else if line_no >= find_startline_no (create_compound_st st1)  && 
				                 line_no <= find_endline_no (create_compound_st st1)  &&
			                         line_no != find_startline_no (create_compound_st st1) then (
                                      let new_f_list = find_bug_inner_loop f_list (create_compound_st st1) in
				      (*print_string ("\nif between \n");*)
                                      find_bug_inner_loop  new_f_list t)

                               else if line_no >= find_startline_no (create_compound_st st2) &&
                                                 line_no <= find_endline_no (create_compound_st st2) then (
				            let new_f_list = find_bug_inner_loop f_list (create_compound_st st2) in
					    (*print_string ("\nelse between \n");*)
                                             find_bug_inner_loop  new_f_list t)
                               
                               else ((*print_string ("\nothers \n");*)find_bug_inner_loop f_list t)
)

else
(

                               if line_no = find_startline_no (create_compound_st st1) && (has_return_bug e (create_compound_st st1)) then  (

                                        (*print_string ("\nif equal in else \n");*)
                                       (whole_f_list [] old_f_list (update_f_list h  prog  line_no iifunc1 lbl_code f_list)) )


                         else if line_no >find_endline_no (create_compound_st st1) && (not (has_return_all (create_compound_st st1))) then

                                       let new_f_list1 = find_bug_inner_loop f_list (create_compound_st st1) in
                                         ((*print_string ("\n>if >else if no return else no return \n");*)find_bug_inner_loop  new_f_list1 t)



                               else if line_no >= find_startline_no (create_compound_st st1)  &&
                                                 line_no <= find_endline_no (create_compound_st st1)  &&
                                                 line_no != find_startline_no (create_compound_st st1) then (
                                      let new_f_list = find_bug_inner_loop f_list (create_compound_st st1) in
                                       (*print_string ("\nif between \n");*)
                                      find_bug_inner_loop  new_f_list t)


                               else ((*print_string ("\nothers \n");*)find_bug_inner_loop f_list t)



)



	   |  Ast_c.Selection  (Ast_c.Switch (e, st)) -> if line_no > start_line  then
                                                              find_bug_inner_loop (find_bug_inner_loop f_list (create_compound_st st)) t
                                                         else find_bug_inner_loop f_list t
	   |  Ast_c.Iteration  (Ast_c.While (e, st)) -> if line_no > start_line  then
                                                              find_bug_inner_loop (find_bug_inner_loop f_list (create_compound_st st)) t
                                                         else find_bug_inner_loop f_list t

	   |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) -> if line_no > start_line  then
                                                              find_bug_inner_loop (find_bug_inner_loop f_list (create_compound_st st)) t
                                                         else find_bug_inner_loop f_list t

	   |  Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) -> if line_no > start_line  then
                                                              find_bug_inner_loop (find_bug_inner_loop f_list (create_compound_st st)) t
                                                         else find_bug_inner_loop f_list t

	   |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> if line_no > start_line  then
                                                              find_bug_inner_loop (find_bug_inner_loop f_list (create_compound_st st)) t
                                                         else find_bug_inner_loop f_list t


           |  _ ->   find_bug_inner_loop f_list t 
           ) 
  in find_bug_inner_loop f_list prog


let rec concate_flist_lblcode f_list = function
  []->f_list
  | h::t-> (match Ast_c.unwrap h with
                Ast_c.Jump (Ast_c.ReturnExpr e) -> f_list@[h]
              | _ -> concate_flist_lblcode f_list t 
           )

let rec find_bug iifunc1 f_list prog  = function
    [] -> []
  | (line_no,if_code,lbl_code,status)::t -> let new_f_list = (find_bug_inner iifunc1 line_no (skip_return lbl_code) f_list  prog ) in
                                           (* print_string("\nf_list length   "); print_string(string_of_int(List.length new_f_list));*)
                                           (* print_string("\nlbl_code length   "); print_string(string_of_int(List.length lbl_code));*)
        (line_no,if_code,(concate_flist_lblcode new_f_list lbl_code),status)::(find_bug iifunc1 new_f_list prog t)




let is_expr_used_expr e e1 =
match e with
   None ->(*print_string("\nFalse\n");*)false
|  Some e->(
              let flag = ref false in
               (Visitor_c.vk_expr
                { Visitor_c.default_visitor_c with
                  Visitor_c.kexpr =
                  (function (k,bigf) -> function exp -> k exp;
                   match e1 with
                     None -> ()
                   |  Some a -> if (compare_exp exp a) then flag:= true else ()
                ) }
              e);
((!flag))
 )


let rec make_id_argslist args_list = function
     []-> args_list
   | h::t-> (match Ast_c.unwrap h with
               Common.Left es -> (match  es with
                                   (((Ast_c.Ident (ident)), typ), ii) ->
                                             (make_id_argslist (args_list@[Some (((Ast_c.Ident (ident)), typ), ii)]) t)
                 | _ -> make_id_argslist args_list t )
             | _ -> make_id_argslist args_list t
             )



let rec is_id_in_args id = function
  []-> false
  | (Some h)::t-> if(compare_exp id h) then true else is_id_in_args id t

let  is_any_arg_used_for_exp_inner id if_exp =
   match if_exp with
     (((Ast_c.Ident (ident)), typ), ii) -> if (compare_exp (((Ast_c.Ident (ident)), typ), ii) id ) then
                                              true
                                           else false

   |  ((( Ast_c.Postfix  ((((Ast_c.Ident (ident)), typ), ii), op)), typ1), ii1)->
                               if (compare_exp (((Ast_c.Ident (ident)), typ), ii) id ) then
                                              true
                               else false
   |   ((( Ast_c.Infix  ((((Ast_c.Ident (ident)), typ), ii), op)), typ1), ii1)->
                               if (compare_exp (((Ast_c.Ident (ident)), typ), ii) id ) then
                                              true
                               else false
   |   ((( Ast_c.Unary  ((((Ast_c.Ident (ident)), typ), ii), op)), typ1), ii1)->
                               if (compare_exp (((Ast_c.Ident (ident)), typ), ii) id ) then
                                              true
                               else false
   |  (((Ast_c.Binary   ((((Ast_c.Ident (ident)), typ), ii), op, e2)), typ1), ii1)->
                               if (compare_exp (((Ast_c.Ident (ident)), typ), ii) id ) then
                                              true
                               else false
   |   (((Ast_c.Binary   (e1, op, (((Ast_c.Ident (ident)), typ), ii))), typ1), ii1)->
                               if (compare_exp (((Ast_c.Ident (ident)), typ), ii) id ) then
                                              true
                               else false
   |    (((Ast_c.Binary   (e1, op, (((Ast_c.FunCall  (e, es)), typ), ii))), typ1), ii1)->
                                        let argslist2 = make_argslist [] es in
                                             if(is_id_in_args id argslist2) then true
                                             else false


   |  _ -> false




let rec is_any_arg_used_for_exp if_exp = function
    []-> false
  |  (Some h)::t ->(match h with
                          (((Ast_c.Ident (ident)), typ), ii)-> if (is_any_arg_used_for_exp_inner  h if_exp ) then true
                                                               else is_any_arg_used_for_exp if_exp t

    |  _ ->  if(is_return_id_in_expr if_exp h) then true
                                 else is_any_arg_used_for_exp if_exp t
                  )
let is_any_arg_used_for_return_exp return_exp arglist =
match return_exp with
  None -> false
|  Some a -> (
              let rec is_any_arg_used_for_return_exp_loop = function
                 []-> false
		|   (Some h)::t ->(match h with
                                    (((Ast_c.Ident (ident)), typ), ii)-> if(compare_exp a h) then true
                                                                         else is_any_arg_used_for_return_exp_loop t
                  |  _ -> if(is_return_id_in_expr a h) then true
                                         else is_any_arg_used_for_return_exp_loop t
                                 )
              in is_any_arg_used_for_return_exp_loop arglist
            )

let rec is_any_arg_in_idlist_inner e1 = function
  []-> false
  |   (Some h)::t -> if(compare_exp h e1) then true else is_any_arg_in_idlist_inner e1 t

let rec is_any_arg_in_idlist idlist = function
    []-> true
  |  [Some x]->  if(is_any_arg_in_idlist_inner x idlist) then true else false
  |  (Some h)::t -> if(is_any_arg_in_idlist_inner h idlist) then true else is_any_arg_in_idlist idlist t


let rec is_diff_name_same_arg arglist1 = function
  []-> false
  | h::t -> match h with
              (((Ast_c.FunCall  (e, es)), typ), ii)-> let arglist2 = make_argslist [] es in
(*Printf.printf "\n\n\n%s\n\n" (Dumper.dump arglist1 );*)
(*Printf.printf "\n\n\n%s\n\n" (Dumper.dump arglist2 );*)
                                                       if (clean_exp_list (remove_option arglist1)) = 
                                                                    (clean_exp_list (remove_option arglist2)) then true
                                                       else is_diff_name_same_arg arglist1 t
            | _ -> is_diff_name_same_arg arglist1 t 
  


let rec is_arg_exists_is_df_eq arg = function
     []-> false
  |     h::t-> ( match Ast_c.unwrap h with
                 Ast_c.ExprStatement (Some (((Ast_c.Assignment
                                                (e1, op, (((Ast_c.FunCall  (e, es)), typ1), ii1))), typ), ii)) ->
                     if(compare_exp arg e1) then true
                     else is_arg_exists_is_df_eq arg t
    |    _ ->  is_arg_exists_is_df_eq arg t
  )



let rec refine_arglist df_eq = function
  []-> []
  |  h::t-> if(is_arg_exists_is_df_eq h df_eq) then h::(refine_arglist df_eq t)
               else refine_arglist df_eq t


let rec fetch_st_same_arg_diff_func_name arglist1  df_eq tmp = function
     []-> tmp
  |   h::t -> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ1), ii1), es)), typ), ii))->
              if  id =~ "^[a-z_][a-z_0-9]*$"  then
                let arglist2 = remove_option (make_argslist [] es) in
                  if (clean_exp_list (remove_integer arglist1)) =
                       (clean_exp_list (remove_integer arglist2)) && (List.length arglist1 !=0)
                      && (List.length arglist2 !=0)then
                           (Some h)
                  else fetch_st_same_arg_diff_func_name arglist1 df_eq tmp  t
	      else fetch_st_same_arg_diff_func_name arglist1 df_eq tmp  t
    |   _ -> fetch_st_same_arg_diff_func_name arglist1 df_eq tmp  t



let rec same_arg_diff_func_name arglist1  df_eq = function
     []-> false
  |  h::t -> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ1), ii1), es)), typ), ii))-> 
	      if  id =~ "^[a-z_][a-z_0-9]*$"  then
		let arglist2 = remove_option (make_argslist [] es) in
(*		let arglist1 = remove_integer arglist1 in*)
(*                let arglist2 = remove_integer arglist2 in*)

                  if (clean_exp_list (remove_integer arglist1)) =
                       (clean_exp_list (remove_integer arglist2)) && (List.length arglist1 !=0) 
		      && (List.length arglist2 !=0)then 
		       ((*print_string(string_of_int(find_startline_no [h]));print_string("  ");*)true)
                  else same_arg_diff_func_name arglist1 df_eq t
               else same_arg_diff_func_name arglist1 df_eq t
    |  _ -> same_arg_diff_func_name arglist1 df_eq t



let rec same_arg_diff_func_name_simple arglist1  df_eq = function
     []-> false
  |   h::t -> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
                let arglist2 =  (remove_option (make_argslist [] es)) in

                  if (clean_exp_list arglist1) =
                       (clean_exp_list arglist2) && (List.length arglist2 !=0) then
                       true
                  else same_arg_diff_func_name_simple arglist1 df_eq t
    |   _ -> same_arg_diff_func_name_simple arglist1 df_eq t


let rec find_diff_name_same_func arglist1 count = function
     []-> count
  |   h::t ->
            match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ1), ii1), es)), typ), ii))->
             if  id =~ "^[a-z_][a-z_0-9]*$"  then
                let arglist2 = (remove_option (make_argslist [] es)) in
                  if (clean_exp_list (remove_option arglist1)) =
		           (clean_exp_list arglist2) && (List.length arglist2 !=0) then
                         find_diff_name_same_func arglist1 (count+1) t
                  else find_diff_name_same_func arglist1 count t
             else find_diff_name_same_func arglist1 count t
    |   _ -> find_diff_name_same_func arglist1 count t



let decrease_false_positive expr if_exp return_exp_bug line_no idlist flist lbl_code  =
  match expr with
     (((Ast_c.FunCall  (e, args)), typ), ii)->
(*print_string(string_of_int(List.length((make_argslist [] args))));*)
(*print_string("\narglist\n");*)
(*Printf.printf "\n\n\n%s\n\n" (Dumper.dump (make_argslist [] args) );*)

         if(is_any_arg_used_for_exp if_exp (make_argslist [] args)) then ((*print_string("\nIf expression false\n");*)false)
         else if (is_any_arg_used_for_return_exp return_exp_bug (make_argslist [] args)) then false
         else if (is_diff_name_same_arg (make_argslist [] args) lbl_code ) then false
         else if (is_any_arg_in_idlist idlist (make_argslist [] args))==true then true
         else if (is_any_arg_in_idlist idlist (make_argslist [] args))==false then false
         else true

  |_ -> false



let rec remove_function_flist line_no iifunc1 func_name flist = function
  []-> []
  | h::t -> if(compare_exp func_name h) then 
                    (if (flist= (h::t)) then remove_function_flist line_no iifunc1 func_name flist t
                     else (

           (
                let start_line = 0 in
		let filename = Ast_c.file_of_info iifunc1 in
                let message = "Release Unorderly" in
                let lcol = 0 in
                let rcol = 0 in
                     print_string("* TODO [[");
                     print_string(filename);
                     print_string("::face=ovl-face1::linb=");
                     print_string(string_of_int(line_no));
                     print_string("::colb=");
                     print_string(string_of_int(lcol));
                     print_string("::cole=");
                     print_string(string_of_int(rcol));
                     print_string("][");
                     print_string(message);
                     print_string(" ");
                     print_string(filename);
                     print_string("::");
                     print_string(string_of_int(line_no));
                     print_string("\n");
);


                          remove_function_flist line_no iifunc1 func_name flist t)
                    )
            else h::(remove_function_flist line_no iifunc1 func_name flist t) 


let rec func_exists_flist func_name  = function
  []-> false
  | h::t-> if(compare_exp func_name h) then true
           else func_exists_flist func_name t
 
let rec insert_function_flist  flist = function
  []->flist
  | h::t -> (match Ast_c.unwrap h with
                 Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) -> 
                              if(func_exists_flist (((Ast_c.FunCall  (e, es)), typ), ii)  flist) then ((*print_string("KKKK");*) insert_function_flist flist t)
                              else ((*print_string("BBBBBBBB");*)insert_function_flist (flist@[(((Ast_c.FunCall  (e, es)), typ), ii)]) t)
                | _-> insert_function_flist flist t
            )

let rec update_function_list stmtlist flist = function
  []->(stmtlist@flist)
  | h::t ->(match Ast_c.unwrap h with
                 Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
                              if(func_exists_flist (((Ast_c.FunCall  (e, es)), typ), ii)  flist) then  update_function_list stmtlist flist t
                              else update_function_list (stmtlist@[(((Ast_c.FunCall  (e, es)), typ), ii)]) flist t
                 | _ -> update_function_list stmtlist flist t
           ) 


let rec is_unorder_arranged_inner h = function
  []-> false
  |  h1::t1 -> if(compare_exp h h1) then true
              else is_unorder_arranged_inner  h t1


let rec is_unorder_arranged lbl_code = function
  []-> true
  |  h::t -> if(is_unorder_arranged_inner h lbl_code) then is_unorder_arranged lbl_code t
             else false

let rec find_missing_st_inner st = function
  []-> true
  | h::t -> if(compare_exp h st) then false
            else find_missing_st_inner st t






let rec find_missing_st if_exp return_exp_bug line_no iifunc1 idlist flist lbl_code = function
  []-> []
  | h::t -> if(find_missing_st_inner h lbl_code) 
               && (decrease_false_positive h if_exp return_exp_bug line_no idlist flist lbl_code) 
              then 
               ((
(*print_string("\expression\n");*)
(*Printf.printf "\n\n\n%s\n\n" (Dumper.dump h );*)
(*print_string("\nreturn\n");*)
(*Printf.printf "\n\n\n%s\n\n" (Dumper.dump return_exp_bug );*)

                let start_line = 0 in
                let filename = Ast_c.file_of_info iifunc1 in
                let message = "Missing Bug" in
                let lcol = 0 in
                let rcol = 0 in
                     print_string("* TODO [[");
                     print_string(filename);
                     print_string("::face=ovl-face1::linb=");
                     print_string(string_of_int(line_no));
                     print_string("::colb=");
                     print_string(string_of_int(lcol));
                     print_string("::cole=");
                     print_string(string_of_int(rcol));
                     print_string("][");
                     print_string(message);
                     print_string(" ");
                     print_string(filename);
                     print_string("::");
                     print_string(string_of_int(line_no));
                     print_string("\n");
                 );


               (h::(find_missing_st if_exp return_exp_bug line_no iifunc1 idlist flist lbl_code t)))


            else find_missing_st if_exp return_exp_bug line_no iifunc1 idlist flist lbl_code t

let rec stmtlist_to_explist  return_exp_bug lblcode_exp = function
  []->(None, lblcode_exp)
  | h::t-> match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))-> 
                             stmtlist_to_explist  return_exp_bug  (lblcode_exp@[(((Ast_c.FunCall  (e, es)), typ), ii)]) t
           | Ast_c.Jump (Ast_c.ReturnExpr e) -> ((Some e),lblcode_exp) 
           | _ -> stmtlist_to_explist return_exp_bug lblcode_exp t



let is_suffix_exp e1 e2 =
if (List.length e1)= 0 then true
else
(
  let rec is_suffix_loop = function
      [] -> false
    |  h::t ->  if (clean_exp_list e1) = (clean_exp_list (h::t)) then (true) else is_suffix_loop t
in is_suffix_loop e2
)

let check_bug_inside if_exp flist lbl_code idlist iifunc1 line_no =
let (return_exp_bug , lblcode_exp) = stmtlist_to_explist None  [] lbl_code  in


(*print_string("\flist\n");*)
(*Printf.printf "\n\n\n%s\n\n" (Dumper.dump flist );*)
(*print_string("\nlbl_code\n");*)
(*Printf.printf "\n\n\n%s\n\n" (Dumper.dump lblcode_exp );*)
  if (is_suffix_exp flist lblcode_exp) then []
  else if (is_unorder_arranged lblcode_exp flist) then (

           (        
                let start_line = 0 in
                let filename = Ast_c.file_of_info iifunc1 in
                let message = "Not in Same order" in
                let lcol = 0 in
                let rcol = 0 in
                     print_string("* TODO [[");
                     print_string(filename);
                     print_string("::face=ovl-face1::linb=");
                     print_string(string_of_int(line_no));
                     print_string("::colb=");
                     print_string(string_of_int(lcol));
                     print_string("::cole=");
                     print_string(string_of_int(rcol));
                     print_string("][");
                     print_string(message);
                     print_string(" ");
                     print_string(filename);
                     print_string("::");
                     print_string(string_of_int(line_no));
                     print_string("\n");  
);

           [])
  else (find_missing_st if_exp return_exp_bug line_no iifunc1 idlist flist lblcode_exp flist)



let update_lblcode_flist idlist flist exe_path iifunc1 line_no lbl_code expression_stmt =
  if (List.length flist) = 0 then (lbl_code,flist )
  else (lbl_code,flist)


let rec create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list = function
  []->(lbl_code, idlist, flist, exe_path )
  | h::t-> let start_lineno = find_startline_no (create_compound_st h) in
           let end_lineno = find_endline_no (create_compound_st h) in
           if (line_no >= start_lineno && line_no <= end_lineno) || line_no >= end_lineno then 
            (
              match Ast_c.unwrap h with
              |	 Ast_c.Labeled (Ast_c.Label (name, st)) -> let (new_lbl_code, new_idlist, new_flist, new_exe_path ) =
		                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t


	      |	 Ast_c.Labeled (Ast_c.Case  (e, st)) ->  let (new_lbl_code, new_idlist, new_flist, new_exe_path ) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list  (create_compound_st st) in
                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t

	      |	 Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->  let (new_lbl_code, new_idlist, new_flist, new_exe_path ) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list  t

	      |	 Ast_c.Labeled (Ast_c.Default st) ->  let (new_lbl_code, new_idlist, new_flist, new_exe_path ) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list  (create_compound_st st) in
                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t

              |	 Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
		                 
	   create_flist_idlist idlist (*(remove_function_flist line_no iifunc1 (((Ast_c.FunCall  (e, es)), typ), ii) flist flist)*) flist  (exe_path@[h]) iifunc1 line_no lbl_code if_list  t
	      |	 Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ1), ii1))), typ2), ii2)) -> 
                                                                                     
     create_flist_idlist (*(idlist@[Some e1])*) idlist (*(remove_function_flist line_no iifunc1 (((Ast_c.FunCall  (e, es)), typ1), ii1) flist flist)*) flist  (exe_path@[h]) iifunc1 line_no lbl_code if_list  t

	      |	 Ast_c.ExprStatement (Some e) -> create_flist_idlist idlist flist (exe_path@[h]) iifunc1 line_no lbl_code if_list t

	      |	 Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> let start_line_st1 = find_startline_no (create_compound_st st1) in
                                                              let end_line_st1 = find_endline_no (create_compound_st st1) in
							      let expression_stmt = ((Ast_c.ExprStatement (Some e)),[]) in
                                                              let exe_path = exe_path@[expression_stmt] in
							      if line_no = start_line_st1 then(
								 (*let missing_st = check_bug_inside e flist lbl_code idlist iifunc1 line_no in*)
						  
								 (lbl_code, idlist, (*(update_function_list exe_path [] flist lbl_code)*) flist , exe_path )
								)
							      else if  line_no > start_line_st1 && line_no <= end_line_st1 then
                                                                  let (new_lbl_code, new_idlist, new_flist, new_exe_path ) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st1) in
                                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t
							      else if (has_multiple_st (create_compound_st st2)) then
								   (  
								      let start_line = find_startline_no (create_compound_st st2) in
                                                                      let end_line = find_endline_no (create_compound_st st2) in
                                                                      if line_no > start_line && line_no <= end_line then
                                                                         let (new_lbl_code, new_idlist, new_flist, new_exe_path) =
                                                                            create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st2) in
                                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                                      else if (not(has_return_all(create_compound_st st2))) && (not(has_return_all(create_compound_st st1))) then
                                                                         let (new_lbl_code1, new_idlist1, new_flist1, new_exe_path1) =
                                                                            create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st1) in
                                                                         let (new_lbl_code2, new_idlist2, new_flist2, new_exe_path2) =
                                                                            create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st2) in
									    ( if (new_flist1 = new_flist2) then 
                                                                                 create_flist_idlist new_idlist1 new_flist1 new_exe_path1 iifunc1 line_no new_lbl_code1 if_list t   
									      else (print_string("\nProblem........\n");
										   create_flist_idlist new_idlist1 new_flist1 new_exe_path1 iifunc1 line_no new_lbl_code1 if_list t
									      ))
								      else if (not(has_return_all(create_compound_st st1))) then
                                                                         let (new_lbl_code, new_idlist, new_flist, new_exe_path) =
                                                                            create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st1) in
                                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t

								      else if (not(has_return_all(create_compound_st st2))) then
                                                                         let (new_lbl_code, new_idlist, new_flist, new_exe_path) =
                                                                            create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st2) in
                                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t
								      else create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list t
								   )
							      else if (not(has_return_all(create_compound_st st1))) then
                                                                         let (new_lbl_code, new_idlist, new_flist, new_exe_path) =
                                                                            create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st1) in
                                                                            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t

                                                              else create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list t


	      |	 Ast_c.Selection  (Ast_c.Switch (e, st)) ->  let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
							          if line_no > start_line && line_no <= end_line then
                                                                  let (new_lbl_code, new_idlist, new_flist, new_exe_path) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
								            create_flist_idlist new_idlist new_flist new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else if (not(has_return_all(create_compound_st st))) then
                                                                  let (new_lbl_code, new_idlist, new_flist, new_exe_path) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
                                                                  create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list t

	      |	 Ast_c.Iteration  (Ast_c.While (e, st)) ->  let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
							            if line_no > start_line && line_no <= end_line then
                                                                  let (new_lbl_code, new_idlist, new_flist,  new_exe_path) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
								            create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else if (not(has_return_all(create_compound_st st))) then
                                                                  let (new_lbl_code, new_idlist, new_flist,  new_exe_path ) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
                                                                  create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list t

              |	 Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->  let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
		                                              if line_no > start_line && line_no <= end_line then
                                                                  let (new_lbl_code, new_idlist, new_flist,  new_exe_path) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
                                                                  create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t   
                                                              else if (not(has_return_all(create_compound_st st))) then
								  let (new_lbl_code, new_idlist, new_flist,  new_exe_path) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
                                                                  create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list t

	      |	 Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->  let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
							            if line_no > start_line && line_no <= end_line then
                                                                  let (new_lbl_code, new_idlist, new_flist,  new_exe_path) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
								            create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else if (not(has_return_all(create_compound_st st))) then
                                                                  let (new_lbl_code, new_idlist, new_flist,  new_exe_path) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
                                                                  create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list t


	      |	 Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) ->  let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
							            if line_no > start_line && line_no <= end_line then
                                                                  let (new_lbl_code, new_idlist, new_flist,  new_exe_path ) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
								            create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else if (not(has_return_all(create_compound_st st))) then
                                                                  let (new_lbl_code, new_idlist, new_flist,  new_exe_path ) =
                                                                  create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list (create_compound_st st) in
                                                                  create_flist_idlist new_idlist new_flist  new_exe_path iifunc1 line_no new_lbl_code if_list t
                                                              else create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list t


              |	 _ -> create_flist_idlist idlist flist exe_path iifunc1 line_no lbl_code if_list t
                                                           
          ) 

         else (lbl_code, idlist, flist, exe_path)


let rec find_bug_iflist flist iifunc1 prog if_list = function
    []-> []
  | (line_no, if_code, lbl_code, status)::t -> let (new_lbl_code,idlist, new_flist, new_exe_path) = create_flist_idlist [] flist [] iifunc1 line_no lbl_code if_list prog in
                                               (* print_string("\nExecution Path\n");*)
                                               (* Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_exe_path );*)

                                                (*print_string("\nFunction list\n");*)
                                                (*Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_flist );*)
                                                (*print_string("\nID List\n");*)
                                                (*Printf.printf "\n\n\n%s\n\n" (Dumper.dump idlist );*)
                                               (line_no, if_code, new_lbl_code, status)::(find_bug_iflist new_flist iifunc1 prog if_list t)  







(*let rec find_execution_path iifunc1 prog = function*)
(*  []->[] *)
(*  | (line_no, if_code, lbl_code, status)::t -> *)
(*               (line_no, if_code, lbl_code, status, (find_execution_path_inner line_no prog))::(find_execution_path iifunc1 prog t)*)



let e1_exists_in_st e1 st = 
 match Ast_c.unwrap st with
     Ast_c.ExprStatement (Some (((Ast_c.Assignment (e11, op, e2)), typ), ii)) ->  if (compare_exp e1 e11) then true
                                                                                else false
  |  Ast_c.ExprStatement (Some (((Ast_c.Unary    (e, Ast_c.Not)), typ), ii)) -> if(compare_exp e1 e) then true else false
  |  Ast_c.ExprStatement (Some (((Ast_c.Binary   (e11, (Ast_c.Logical Ast_c.Eq), e2)), typ), ii)) -> if(compare_exp e1 e11) then true else false
  |  Ast_c.ExprStatement (Some (((Ast_c.Binary   (e11, (Ast_c.Logical Ast_c.NotEq), e2)), typ), ii)) -> if(compare_exp e1 e11) then true else false
  |  Ast_c.ExprStatement (Some ((((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(s,ii))), typ), ii1), es)), typ1), ii2))) ->
      let arglist = remove_option (make_argslist [] es) in
                                                       if s =~ "IS_ERR" && (exp_exists_in_list e1 arglist) then true else  false
  |  Ast_c.ExprStatement (Some e)-> if(compare_exp e1 e) then true else false
  |  _ -> false



let rec exp_defined_in_df_eq_st_inner e11 exp new_df_eq  = function
  []->( match exp with
         None -> new_df_eq
      |  Some a-> (new_df_eq@[a]))
  |  h::t-> match Ast_c.unwrap h with
                 Ast_c.ExprStatement (Some (((Ast_c.Assignment
                       (e1, op, (((Ast_c.FunCall  (((((Ast_c.Ident (Ast_c.RegularName(id, ii3))), typ2), ii2)), 
						   es)), typ1), ii1))), typ), ii)) ->
			 if id =~"^[A-Z_][A-Z_0-9]*$" then 
			   exp_defined_in_df_eq_st_inner e11 None new_df_eq t
		         else if(compare_exp e1 e11)   then
                             exp_defined_in_df_eq_st_inner e11 None new_df_eq t
                         else exp_defined_in_df_eq_st_inner e11 exp (new_df_eq@[h]) t
                       
           | _ -> exp_defined_in_df_eq_st_inner e11 exp new_df_eq t

let exp_defined_in_df_eq_st st exp new_df_eq  df_eq = 
  match Ast_c.unwrap st with
    Ast_c.ExprStatement (Some (((Ast_c.Assignment (e11, op, e22)), typ), ii)) ->
      exp_defined_in_df_eq_st_inner e11 exp new_df_eq df_eq 
  | _-> df_eq

 



let rec update_data_flow_eq st new_df_eq = function
    []-> (new_df_eq@[st])
  | h::t->( match Ast_c.unwrap h with 		    
               Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
                                    if (e1_exists_in_st e1 st) then 
                                              ( (*print_string("\nDDDDDDDDDDDDDDDDDD11111\n"); *)
				                 match Ast_c.unwrap st with
						     Ast_c.ExprStatement (Some (((Ast_c.Assignment (e11, op1, e22)), typ1), ii1)) ->(new_df_eq@[st]@t)

                                                   | Ast_c.ExprStatement (Some (((Ast_c.Unary (e, Ast_c.Not)), typ1), ii1)) ->
                                                       (new_df_eq@[((Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op,
                                                                               (((Ast_c.Ident (Ast_c.RegularName("NULL",[]))), typ1), []))), typ), ii))),[])]@t)

						   |   Ast_c.ExprStatement (Some ((((Ast_c.FunCall  
									  ((((Ast_c.Ident (Ast_c.RegularName("IS_ERR",ii))), typ), ii1), es)), typ1), ii2))) ->
						       (new_df_eq@[((Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, 
									       (((Ast_c.Ident (Ast_c.RegularName("NULL",[]))), typ1), []))), typ), ii))),[])]@t)

                                                   | Ast_c.ExprStatement (Some (((Ast_c.Binary   (e11, (Ast_c.Logical Ast_c.NotEq), 
									       (((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ2), ii2))), typ1), ii1))->
						      (new_df_eq@[h]@t)

						   | Ast_c.ExprStatement (Some ((((Ast_c.FunCall  
										     ((((Ast_c.Ident (Ast_c.RegularName(s,ii2))), typ1), ii1), es)), typ2), ii3))) ->                                                                  
                                                       (new_df_eq@[((Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op,
                                                                               (((Ast_c.Ident (Ast_c.RegularName("NULL",[]))), typ1), []))), typ2), ii))),[])]@t)

                                                   |  Ast_c.ExprStatement (Some (((Ast_c.Binary   (e11, (Ast_c.Logical Ast_c.Eq),
                                                                               (((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ2), ii2))), typ1), ii1))->

                                                       (new_df_eq@[((Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op,
                                                                               (((Ast_c.Ident (Ast_c.RegularName("NULL",[]))), typ2), []))), typ), ii))),[])]@t)
                                                   | Ast_c.ExprStatement (Some e)->
						       (new_df_eq@[h]@t)
						       
                                                   | _ -> (*print_string("\nDDDDDDDDDDDDDDDDDD222222\n");  *)
						       update_data_flow_eq st (new_df_eq@[h]) t

					      )
		                    else ( (*print_string("\nDDDDDDDDDDDDDDDDDD333333\n");*)update_data_flow_eq st (new_df_eq@[h]) t)
               | _ -> (*print_string("\nDDDDDDDDDDDDDDDDDD444444\n");   *)
		   update_data_flow_eq st (new_df_eq@[h]) t
           ) 


let rec append_exe_path_inner st  = function  
     []-> []
   | h1::t1-> (h1@st)::(append_exe_path_inner st t1)


let rec append_exe_path st = function
  []->[[st]]
  | h::t -> (h@[st])::(append_exe_path st t)



let rec ass_exists_in_list st = function
    []-> false
  | h::t-> ( match Ast_c.unwrap h with
                 Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) -> if (e1_exists_in_st e1 st) then true
                                                                                            else ass_exists_in_list st t
               |  _ ->  ass_exists_in_list st t
           )



let rec intigrate_df_eq df_eq1  = function
    []-> []
  | h::t-> if  (st_exists_in_list h df_eq1) then intigrate_df_eq df_eq1 t
           else h::(intigrate_df_eq df_eq1 t)



let rec find_st st = function
  []-> None
  | h::t-> 
           ( match Ast_c.unwrap h with
                 Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) -> if (e1_exists_in_st e1 st) then Some h
                                                                                            else find_st st t
              |   _ ->  find_st st t
           )


let rec find_kill_frm df_eq = function
  []-> []
  |  h::t-> if(ass_exists_in_list h df_eq) &&(not(st_exists_in_list h df_eq)) then (match (find_st h df_eq) with
                                                                                      None -> find_kill_frm df_eq t
                                                                                    | Some a -> a::(find_kill_frm df_eq t)
                                                                                   )
                                                            
           else find_kill_frm df_eq t




let rec find_gen_frm df_eq = function
  []-> []
  | h::t-> if(ass_exists_in_list h df_eq) then find_gen_frm df_eq t
           else h::(find_gen_frm df_eq t)

let rec find_df_gen_1_2 df_eq1_gen = function
  []-> []
  | h::t-> if(st_exists_in_list h df_eq1_gen) then h::(find_df_gen_1_2 df_eq1_gen t)
           else find_df_gen_1_2 df_eq1_gen t 

let rec find_h_frm_df_eq st = function
  []-> []
  | h::t->  ( match Ast_c.unwrap h with
                 Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) -> if (e1_exists_in_st e1 st) then h::(find_h_frm_df_eq st t)
                                                                                            else find_h_frm_df_eq st t
               |  _ ->  find_h_frm_df_eq st t
           )

let rec integrate_ifelse_df_eq new_df_eq df_eq_kill_1_2 = function
  []-> new_df_eq
  | h::t-> let l = find_h_frm_df_eq h df_eq_kill_1_2 in
              if (List.length l) = 0 then integrate_ifelse_df_eq (new_df_eq@[h]) df_eq_kill_1_2 t
              else integrate_ifelse_df_eq (new_df_eq@l) df_eq_kill_1_2 t 


let rec update_df_eq_for_if_test e11 = function
  []->[]
  | h::t-> (match Ast_c.unwrap h with
                Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) -> 
		   if(compare_exp e11 e1) then 
		     update_df_eq_for_if_test e11 t
                   else h::(update_df_eq_for_if_test e11 t)
                |_-> h::(update_df_eq_for_if_test e11 t) 
           )

let exp_chk e = 
  match e with 
    (((Ast_c.Ident (ident)), typ), ii) -> true
  | (((Ast_c.FunCall  (e, es)), typ), ii)->true
  | (((Ast_c.RecordAccess   (e, name)), typ), ii) -> true
  | (((Ast_c.RecordPtAccess (e, name)), typ), ii) -> true
  | _ -> false
  

let rec insert_exp_into_ref_list e  re e2 new_ref_list = function
     []-> (new_ref_list@[(e,[e2])]@[(re,[e2])])
  |  (a,b)::t -> if(compare_exp a e) then  (new_ref_list@[(a,(b@[e2]))]@t)
                 else insert_exp_into_ref_list e re e2 (new_ref_list@[(a,b)]) t

let rec insert_exp_into_ref_list_multi_loop_2 e arg_list new_ref_list = function
   []->new_ref_list
  | (a,b)::t -> if(compare_exp a e) then (new_ref_list@[(a,(b@arg_list))]@t)
                else insert_exp_into_ref_list_multi_loop_2 e arg_list (new_ref_list@[(a,b)]) t

let rec insert_exp_into_ref_list_multi_loop e = function
  []-> false
  | (a,b)::t -> if(compare_exp a e) then true
                else insert_exp_into_ref_list_multi_loop e t

let rec insert_exp_into_ref_list_multi head tail new_arg_list ref_list = function
  []->(ref_list@[(head,tail)])
  | h::t-> if(insert_exp_into_ref_list_multi_loop h ref_list) then
              insert_exp_into_ref_list_multi_loop_2 h (new_arg_list@t) [] ref_list
           else insert_exp_into_ref_list_multi head tail (new_arg_list@[h]) ref_list t

let rec intersect_list a = function
  []->[]
  | h::t-> if(exp_exists_in_list h a) then h::(intersect_list a t)
           else intersect_list a t

let rec intersect_ref_list_inner a b = function
  []->[]
  | (c,d)::t-> if(compare_exp a c) then [(a, (intersect_list b d))]
	       else intersect_ref_list_inner a b t

let rec intersect_ref_list ref_list1 new_ref_list = function 
  []-> new_ref_list
  | (a,b)::t-> let st  = intersect_ref_list_inner a b ref_list1 in
               intersect_ref_list ref_list1 (new_ref_list@st) t

let rec intersect_df_eq df_eq1 new_df_eq = function
   []-> new_df_eq
  | h::t-> if(st_exists_in_list h df_eq1) then intersect_df_eq df_eq1 (new_df_eq@[h]) t
	   else intersect_df_eq df_eq1 new_df_eq t
          

let rec exe_path_for_switch_inner1  path = function
  []-> path
  | h::t-> exe_path_for_switch_inner1 (path@[h]) t


let rec exe_path_for_switch_inner new_path path = function
  []-> new_path
  | h::t-> let new_path1 = exe_path_for_switch_inner1  path h in
            exe_path_for_switch_inner (new_path@[new_path1]) path t

let rec exe_path_for_switch new_path path1 = function
  []-> new_path
  | h::t-> let new_path1 = exe_path_for_switch_inner [] h path1 in
           exe_path_for_switch (new_path@new_path1) path1 t


let rec find_case_block line_no st  = function
  []->st
  | h::t-> let start_line = find_startline_no [h] in
           let end_line = find_endline_no [h] in
           if(line_no>= start_line && line_no<=end_line) then h
           else find_case_block line_no st t 

let rec find_case_stmts line_no case_stmts = function
  []->case_stmts
  | h::t-> let start_line = find_startline_no [h] in
           let end_line   = find_endline_no [h] in
	   if(line_no>= start_line && line_no<= end_line) then
	     match Ast_c.unwrap h with
	       Ast_c.Labeled (Ast_c.Case  (e, st)) -> ([((Ast_c.ExprStatement (Some e)),[])]@[st])
	     | Ast_c.Labeled (Ast_c.Default st) -> [st]
	     | _ -> (case_stmts@[h])
	   else
                 match Ast_c.unwrap h with
                   Ast_c.Labeled (Ast_c.Case  (e, st)) -> 
	               find_case_stmts line_no  ([((Ast_c.ExprStatement (Some e)),[])]@[st]) t
		 | Ast_c.Labeled (Ast_c.Default st) ->
		       find_case_stmts line_no [st] t
		 | _ -> find_case_stmts line_no (case_stmts@[h]) t

let rec find_case_paths case_paths  case_stmts = function
  []-> (case_paths@[case_stmts])
  | h::t-> match Ast_c.unwrap h with
            Ast_c.Labeled (Ast_c.Case  (e, st)) -> 
	      if(List.length case_stmts = 0) then 
		find_case_paths case_paths ([((Ast_c.ExprStatement (Some e)),[])]@[st]) t 
	      else
                find_case_paths (case_paths@[case_stmts]) ([((Ast_c.ExprStatement (Some e)),[])]@[st]) t
          | Ast_c.Labeled (Ast_c.Default st) ->
	      if(List.length case_stmts= 0) then
		find_case_paths case_paths [st] t
              else
                find_case_paths (case_paths@[case_stmts]) [st] t
          | _ -> find_case_paths case_paths (case_stmts@[h]) t



let rec find_execution_path  line_no exe_path  if_exp  lcol rcol df_eq ref_list if_exp_list = function
  []-> (exe_path,if_exp, lcol, rcol, df_eq, ref_list, if_exp_list)
  |  h::t-> let start_lineno = find_startline_no (create_compound_st h) in
            let end_lineno = find_endline_no (create_compound_st h) in
           if (line_no >= start_lineno && line_no <= end_lineno) || line_no >= end_lineno then
            (
              match Ast_c.unwrap h with

              |	  Ast_c.Labeled (Ast_c.Label (name, st)) ->
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list ) =
                                    find_execution_path line_no exe_path  if_exp lcol rcol df_eq ref_list if_exp_list (create_compound_st st) in
                                    find_execution_path line_no new_exe_path new_if_exp lcol rcol df_eq ref_list  if_exp_list t
                              else if (not(has_return_all(create_compound_st st))) && (not(has_goto(create_compound_st st))) then
                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list  ) =
                                    find_execution_path line_no exe_path  if_exp lcol rcol df_eq  ref_list   if_exp_list (create_compound_st st) in
                                    find_execution_path line_no new_exe_path new_if_exp lcol rcol df_eq  ref_list  if_exp_list t
                              else find_execution_path line_no exe_path if_exp lcol rcol df_eq  ref_list  if_exp_list t

              |	  Ast_c.Labeled (Ast_c.Case  (e, st)) ->  (*print_string("\nMMMMMMMMMMM PPPP\n");*)
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                               
                              if line_no > start_line && line_no <= end_line then 
                                  let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) = 
                                find_execution_path line_no exe_path  if_exp lcol rcol df_eq  ref_list  if_exp_list (create_compound_st st) in 
(*                                 print_string(string_of_int(List.length new_exe_path));*)
(*                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_exe_path);	*)			 
                                find_execution_path line_no new_exe_path new_if_exp lcol rcol df_eq  ref_list  if_exp_list t
                              else if (not(has_return_all(create_compound_st st))) && (not(has_goto(create_compound_st st))) then
( (*print_string("\nMMMMMMMMMMM KKKKK\n"); *)

                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                find_execution_path line_no exe_path  if_exp lcol rcol df_eq  ref_list  if_exp_list (create_compound_st st) in
                                 print_string(string_of_int(List.length new_exe_path));
                                 (*Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_exe_path); *)
                                    find_execution_path line_no (exe_path@new_exe_path) new_if_exp lcol rcol df_eq  ref_list  if_exp_list t )
                              else find_execution_path line_no exe_path if_exp lcol rcol df_eq  ref_list  if_exp_list t

              |	  Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) -> (*print_string("\nMMMMMMMMMMM\n");*)
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                                                              let expression_stmt = ((Ast_c.ExprStatement (Some e)),[]) in
                                                              let exe_path = (  if (List.length exe_path) = 0 then
                                                                                    (exe_path@[[expression_stmt]])
                                                                                else
                                                                                (append_exe_path_inner [expression_stmt] exe_path)
                                                                             ) in

                              if line_no > start_line && line_no <= end_line then
                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                    find_execution_path line_no exe_path  if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                    find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                              else if (not(has_return_all(create_compound_st st))) && (not(has_goto(create_compound_st st))) then
                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                    find_execution_path line_no exe_path  if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                    find_execution_path line_no (exe_path@new_exe_path) new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                              else find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t

              |	  Ast_c.Labeled (Ast_c.Default st) -> (*print_string("\nMMMMMMMMMMM  CCCCCCC\n");*)
                              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in
                              if line_no > start_line && line_no <= end_line then
                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                    find_execution_path line_no exe_path  if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                    find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                              else if (not(has_return_all(create_compound_st st))) && (not(has_goto(create_compound_st st))) then
                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                    find_execution_path line_no []  if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                    find_execution_path line_no (exe_path@new_exe_path) new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                              else find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t

              |	   Ast_c.Compound statxs -> let st = stmtEle_remove statxs in
                              let start_line = find_startline_no st in
                              let end_line = find_endline_no st in
                              if line_no > start_line && line_no <= end_line then
                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                    find_execution_path line_no exe_path  if_exp lcol rcol  df_eq  ref_list  if_exp_list st in
                                    find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                              else if (not(has_return_all st)) && (not(has_goto  st)) then
                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                    find_execution_path line_no exe_path  if_exp lcol rcol  df_eq  ref_list  if_exp_list st in
                                    find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                              else find_execution_path line_no exe_path if_exp lcol rcol  df_eq ref_list  if_exp_list t

	      |	 Ast_c.ExprStatement (None) ->
                             if (List.length exe_path) = 0 then find_execution_path line_no (exe_path@[[h]]) if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                             else
                             find_execution_path line_no (append_exe_path_inner [h] exe_path) if_exp lcol rcol  df_eq  ref_list  if_exp_list t



              |	  Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.RecordAccess (e, name)), typ1), ii1), op, 
								(((Ast_c.Ident (ident)), typ2), ii2) )), typ), ii)) ->


(*                                                print_string("\nref_list before\n");  *)
(*                                                 print_string(string_of_int(List.length ref_list));  *)
(*                                                Printf.printf "\n\n\n%s\n\n" (Dumper.dump ref_list );  *)


                 let ref_list = insert_exp_into_ref_list (((Ast_c.RecordAccess (e, name)), typ1), ii1)  
(((Ast_c.RecordAccess (e, name)), typ1), ii1) (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in

(*                                                print_string("\nref_list after\n");  *)
(*                                                 print_string(string_of_int(List.length ref_list));  *)
(*                                               Printf.printf "\n\n\n%s\n\n" (Dumper.dump ref_list );  *)



                 find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t


(*             |	Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) -> *)
(*                 let ref_list =  insert_exp_into_ref_list e1 e2 [] ref_list  in *)
(*                 find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list t *)

	     |	Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.RecordPtAccess (e, name)), typ1), ii1), op, 
							       (((Ast_c.Ident (ident)), typ2), ii2) )), typ), ii)) ->
                 let ref_list = insert_exp_into_ref_list e (((Ast_c.RecordPtAccess (e, name)), typ1), ii1) 
		         (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in 
		 find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t

	     |	Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
		  let df_eq = update_data_flow_eq h [] df_eq   in 
                  if(List.length exe_path) = 0 then find_execution_path line_no (exe_path@[[h]]) if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                  else  find_execution_path line_no (append_exe_path_inner [h] exe_path) if_exp lcol rcol  df_eq  ref_list  if_exp_list t


             |  Ast_c.ExprStatement (Some e) ->

 		  if(List.length exe_path) = 0 then find_execution_path line_no (exe_path@[[h]]) if_exp lcol rcol  df_eq  ref_list  if_exp_list t 
  		  else find_execution_path line_no (append_exe_path_inner [h] exe_path) if_exp lcol rcol  df_eq  ref_list  if_exp_list t


              |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> 
		  let start_line_st1 = find_startline_no (create_compound_st st1) in
                  let end_line_st1 = find_endline_no (create_compound_st st1) in
                  let expression_stmt = ((Ast_c.ExprStatement (Some e)),[]) in
(*                  let exe_path = (  if (List.length exe_path) = 0 then    *)
(*				       (exe_path@[[expression_stmt]])   *)
(*				    else   *)
(*				       (append_exe_path_inner [expression_stmt] exe_path)  *)
(*				 ) in *)



                  let df_eq1 = update_data_flow_eq expression_stmt [] df_eq   in
                  let df_eq2 =
                              ( match Ast_c.unwrap expression_stmt with
                                 Ast_c.ExprStatement (Some (((Ast_c.Unary (e11, Ast_c.Not)), typ), ii))  when exp_chk e11 -> df_eq
			      
                              |	 Ast_c.ExprStatement (Some ((((Ast_c.FunCall  ((((Ast_c.Ident (Ast_c.RegularName(s,ii))), typ), ii1), es)), typ1), ii2))) -> 
						       if s =~ "IS_ERR" then df_eq else  df_eq
                              |	 Ast_c.ExprStatement (Some (((Ast_c.Binary (e11, (Ast_c.Logical Ast_c.Eq),
                                                            (((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ2), ii2))), typ1), ii1)) when exp_chk e11 -> df_eq
                              |	 Ast_c.ExprStatement (Some (((Ast_c.Binary   (e11, (Ast_c.Logical Ast_c.NotEq),
                                                            (((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ2), ii2))), typ1), ii1)) when exp_chk e11 ->
                                                        update_data_flow_eq ((Ast_c.ExprStatement (Some (((Ast_c.Unary (e11, Ast_c.Not)), typ1), []))),[]) [] df_eq
                              |	 Ast_c.ExprStatement (Some ((exp, typ), ii)) when exp_chk ((exp, typ), ii) ->
                                                               update_data_flow_eq ((Ast_c.ExprStatement (Some (((Ast_c.Unary (((exp, typ), ii),
                                                                                                                               Ast_c.Not)), typ), []))),[]) [] df_eq
                              |	 _ -> df_eq
                              ) in

							      
                 if line_no = start_line_st1 then ( (*print_string("\nif match\n");*)
                       let if_exp_list = (if_exp_list@[Some e]) in
		       let rcol = find_rcol (create_compound_st st1) in
		       let lcol = find_lcol (create_compound_st st1) in
                           (exe_path, Some e, Some lcol, Some rcol ,  df_eq1, ref_list, if_exp_list  ))
                                                                
                 else if  line_no > start_line_st1 && line_no <= end_line_st1 then
                       let if_exp_list = (if_exp_list@[Some e])in
		       
                       let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list    )  =
                           find_execution_path line_no exe_path if_exp lcol rcol  df_eq1  ref_list  if_exp_list (create_compound_st st1) in
                           find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                 else if (has_multiple_st (create_compound_st st2)) then
                   (
                       let start_line = find_startline_no (create_compound_st st2) in
                       let end_line = find_endline_no (create_compound_st st2) in
                       if line_no >= start_line && line_no <= end_line then
			 let if_exp_list = (if_exp_list@[Some e])in
                           let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   )  =
                                   find_execution_path line_no exe_path if_exp lcol rcol  df_eq2  ref_list  if_exp_list (create_compound_st st2) in
                                   find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                                                             
                       else if (not(has_return_all(create_compound_st st2))) && (not(has_return_all(create_compound_st st1))) 
                                && (not(has_goto(create_compound_st st2))) && (not(has_goto(create_compound_st st1))) then
                           let (new_exe_path1,new_if_exp1, lcol,  rcol,  df_eq1, ref_list, if_exp_list   ) =
                                   find_execution_path line_no exe_path if_exp lcol rcol  df_eq1  ref_list  if_exp_list (create_compound_st st1) in
                           let df_eq1_gen = find_gen_frm df_eq df_eq1 in
                           let df_eq1_kill = find_kill_frm df_eq1 df_eq in
                                                                 
                           let (new_exe_path2,new_if_exp2, lcol, rcol,  df_eq2, ref_list, if_exp_list   ) =
                                   find_execution_path line_no exe_path if_exp lcol rcol  df_eq2  ref_list  if_exp_list (create_compound_st st2) in
                           let df_eq2_gen = find_gen_frm df_eq df_eq2 in
			   let df_eq2_kill = find_kill_frm df_eq2 df_eq in
                 
                           let df_eq_kill_1_2 = (df_eq1_kill@(intigrate_df_eq df_eq1_kill df_eq2_kill)) in
                           let df_eq_gen_1_2 = find_df_gen_1_2 df_eq1_gen df_eq2_gen in
                           let df_eq = ((integrate_ifelse_df_eq [] df_eq_kill_1_2 df_eq)@df_eq_gen_1_2 ) in
                                   find_execution_path line_no (new_exe_path1@new_exe_path2) new_if_exp1 lcol rcol  df_eq  ref_list  if_exp_list t
                       else if (not(has_return_all(create_compound_st st1))) && (not(has_goto(create_compound_st st1))) then
                           let (new_exe_path,new_if_exp, lcol,  rcol,  df_eq, ref_list, if_exp_list   ) =
                                   find_execution_path line_no exe_path if_exp lcol rcol  df_eq1  ref_list  if_exp_list (create_compound_st st1) in
                                   find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t

                       else if (not(has_return_all(create_compound_st st2))) && (not(has_goto(create_compound_st st2))) then
                           let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                   find_execution_path line_no exe_path if_exp lcol rcol  df_eq2  ref_list  if_exp_list (create_compound_st st2) in
                                   find_execution_path line_no new_exe_path  new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                       else find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                   )
                 else if (not(has_return_all(create_compound_st st1))) && (not(has_goto(create_compound_st st1))) then (
(*		   print_string("\nif does not have return\n"); *)
                       let (new_exe_path,new_if_exp, lcol, rcol,  df_eq1, ref_list, if_exp_list   ) =   
                           find_execution_path line_no exe_path if_exp lcol rcol  df_eq1  ref_list  if_exp_list (create_compound_st st1) in   

 		       let df_eq1_gen = find_gen_frm df_eq df_eq1 in   
                       let df_eq1_kill = find_kill_frm df_eq1 df_eq in   
                       let df_eq2_gen = find_gen_frm df_eq df_eq2 in   
                       let df_eq2_kill = find_kill_frm df_eq2 df_eq in   
                       let df_eq_kill_1_2 = (df_eq1_kill@(intigrate_df_eq df_eq1_kill df_eq2_kill)) in   
                       let df_eq_gen_1_2 = find_df_gen_1_2 df_eq1_gen df_eq2_gen in  
                       let df_eq = ((integrate_ifelse_df_eq [] df_eq_kill_1_2 df_eq)@df_eq_gen_1_2 ) in   
                           find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t )
                 else find_execution_path line_no exe_path if_exp lcol rcol  df_eq2  ref_list  if_exp_list t


              |  Ast_c.Selection  (Ast_c.Switch (e, st)) ->(* print_string("\nMMMMMMMMMMM 1111\n");  *)
		              let start_line = find_startline_no (create_compound_st st) in
                              let end_line = find_endline_no (create_compound_st st) in

                              if line_no > start_line && line_no <= end_line then
                              let if_exp_list = (if_exp_list@[Some e]) in
			      let case_block = find_case_block line_no st (create_compound_st st) in 
                                   (*print_string(string_of_int(List.length (create_compound_st case_block)));*)


                                 let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) = 
                            find_execution_path line_no exe_path  if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st case_block) in 
                                 (*let switch_exe = exe_path_for_switch [] new_exe_path exe_path in*)


                            find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                               else if (not(has_return_all(create_compound_st st))) && (not(has_goto(create_compound_st st))) then 
				   let new_exe_path = [] in
                                   let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) = 
                        find_execution_path line_no []  if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in 
  (*                               print_string(string_of_int(List.length exe_path));*)
(*                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump exe_path);*)
				   let switch_exe = exe_path_for_switch [] new_exe_path exe_path in

                        find_execution_path line_no switch_exe new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t 
                              else find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t

              |  Ast_c.Iteration  (Ast_c.While (e, st)) ->  
		        let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
                                                              let expression_stmt = ((Ast_c.ExprStatement (Some e)),[]) in
                                                              let exe_path = (  if (List.length exe_path) = 0 then
                                                                                    (exe_path@[[expression_stmt]])
                                                                                else
                                                                                (append_exe_path_inner [expression_stmt] exe_path)
                                                                             ) in

                                                                    if line_no > start_line && line_no <= end_line then
								      let if_exp_list = (if_exp_list@[Some e]) in
                                                                  let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                                                  find_execution_path line_no exe_path  if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                                                            find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                                                              else if (not(has_return_all(create_compound_st st))) 
							      && (not(has_goto(create_compound_st st))) then
                                                                  let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                                                  find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                                                            find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t

                                                              else find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t



              |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->  let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
                                                              let expression_stmt = ((Ast_c.ExprStatement (Some e)),[]) in
                                                              let exe_path = (  if (List.length exe_path) = 0 then
                                                                                    (exe_path@[[expression_stmt]])
                                                                                else
                                                                                (append_exe_path_inner [expression_stmt] exe_path)
                                                                             ) in

                                                              if line_no > start_line && line_no <= end_line then
								let if_exp_list = (if_exp_list@[Some e]) in
                                                                  let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                                                  find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                                                            find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                                                              else if (not(has_return_all(create_compound_st st))) 
							      && (not(has_goto(create_compound_st st))) then
                                                                  let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                                   find_execution_path line_no exe_path if_exp lcol rcol df_eq   ref_list  if_exp_list (create_compound_st st) in
                                                   find_execution_path line_no new_exe_path new_if_exp lcol rcol df_eq   ref_list  if_exp_list t

                                                              else find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t

              |  Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->  let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
                                                                    if line_no > start_line && line_no <= end_line then
                                                                  let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                                                  find_execution_path line_no exe_path  if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                                                            find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                                                              else if (not(has_return_all(create_compound_st st))) 
							      && (not(has_goto(create_compound_st st))) then
                                                                  let (new_exe_path,new_if_exp, lcol, rcol, df_eq, ref_list, if_exp_list   ) =
                                                                  find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                                                            find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t

                                                              else find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t


              |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) ->  let start_line = find_startline_no (create_compound_st st) in
                                                              let end_line = find_endline_no (create_compound_st st) in
                                                                    if line_no > start_line && line_no <= end_line then
                                                                  let (new_exe_path,new_if_exp, lcol , rcol,  df_eq, ref_list, if_exp_list   )=
                                                                  find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list (create_compound_st st) in
                                                                            find_execution_path line_no new_exe_path new_if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                                                              else if (not(has_return_all(create_compound_st st))) 
							      && (not(has_goto(create_compound_st st))) then
                                                                  let (new_exe_path,new_if_exp, lcol, rcol,  df_eq, ref_list, if_exp_list   ) =
                                                                  find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list if_exp_list (create_compound_st st) in
                                                                            find_execution_path line_no new_exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t

                                                              else find_execution_path line_no exe_path if_exp lcol rcol  df_eq  ref_list  if_exp_list t
              |	  _ ->

                  if(List.length exe_path) = 0 then find_execution_path line_no (exe_path@[[h]]) if_exp lcol rcol  df_eq  ref_list  if_exp_list t
                  else
                        find_execution_path line_no (append_exe_path_inner [h] exe_path) if_exp lcol rcol  df_eq  ref_list  if_exp_list t


          )
         else (exe_path,  if_exp, lcol,  rcol,  df_eq, ref_list, if_exp_list  )

let rec create_compound_st_for_switch exp case_stmts stmt_list = function
  []->stmt_list
  | h::t-> match Ast_c.unwrap h with
               Ast_c.Labeled (Ast_c.Case  (e, st)) -> 
		 if(List.length case_stmts =0) then create_compound_st_for_switch e (case_stmts@[st]) stmt_list t
		 else(
		     create_compound_st_for_switch exp []
		     (stmt_list@[(Ast_c.Labeled (Ast_c.Case  (exp, (Ast_c.Compound(stmtEle_add_list (case_stmts)),[]))),[])]) (h::t)
		 )
              | Ast_c.Labeled (Ast_c.Default st) -> 
		  (stmt_list@[(Ast_c.Labeled (Ast_c.Case  (exp, (Ast_c.Compound(stmtEle_add_list (case_stmts)),[]))),[])]@
		   [(Ast_c.Labeled (Ast_c.Default (Ast_c.Compound(stmtEle_add_list ([st]@t)),[])),[])]) 
	      |	_ -> create_compound_st_for_switch exp (case_stmts@[h]) stmt_list t


  





let rec any_arg_found_df_eq df_eq = function
    [] -> false
  |  h::t-> if(is_arg_exists_is_df_eq h df_eq) then true
           else any_arg_found_df_eq df_eq t


let rec gather_goto_code lbl_list stmt_list goto_list  = function
     []->[]
  |  h::t-> match Ast_c.unwrap h with
    |   Ast_c.Jump (Ast_c.Return) -> (stmt_list@[h])
    |   Ast_c.Jump (Ast_c.ReturnExpr e) -> (stmt_list@[h])
    |   Ast_c.Jump (Ast_c.Goto name) ->
                if (st_exists_in_list h goto_list) then []
                else
		  (
                   gather_goto_code lbl_list stmt_list (goto_list@[h]) (refine_code(code_for_goto name lbl_list))

		)
    |  _ -> gather_goto_code lbl_list (stmt_list@[h]) goto_list t


let rec exp_list_defined_in_df_eq new_df_eq = function
   []-> new_df_eq
  | h::t-> let new_df_eq = exp_defined_in_df_eq_st h (Some h) []  new_df_eq in
           exp_list_defined_in_df_eq new_df_eq t

let rec st_exists_in_all_list st = function
  []-> true
  | h::t-> if(st_exists_in_list st h) then 
             st_exists_in_all_list st t
           else false

let rec intersect_df_eq_for_switch  case_list  new_df_eq = function
  []-> new_df_eq
  | h::t-> if (st_exists_in_all_list h case_list) then 
               intersect_df_eq_for_switch  case_list  (new_df_eq@[h]) t
           else intersect_df_eq_for_switch  case_list  new_df_eq t


let rec create_df_eq_for_switch df_eq_list df_eq = function
  []-> df_eq_list
  | h::t-> match Ast_c.unwrap h with
             Ast_c.Labeled (Ast_c.Case  (e, st)) ->
	       let df_eq_case = exp_list_defined_in_df_eq df_eq (create_compound_st st) in
	       create_df_eq_for_switch (df_eq_list@[df_eq_case]) df_eq t
           | Ast_c.Labeled (Ast_c.Default st) -> 
	       let df_eq_case = exp_list_defined_in_df_eq df_eq (create_compound_st st) in
	       create_df_eq_for_switch (df_eq_list@[df_eq_case]) df_eq t
           | _ -> create_df_eq_for_switch df_eq_list df_eq t


let rec add_option = function
  []-> []
  |    h::t-> (Some h)::(add_option t)


let rec find_func_without_arg = function
    []-> false
  | h::t-> (match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
                if(List.length (make_argslist [] es) = 0) then true
                else find_func_without_arg t
    |   _->  find_func_without_arg t )

let rec count_find_func_without_arg count = function
    []-> count
  |  h::t-> (match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
		if(List.length (make_argslist [] es) = 0) then 
		  count_find_func_without_arg (count+1) t
                else count_find_func_without_arg count t
            | _->  count_find_func_without_arg count t )



let rec find_func_without_arg_count count = function
    []-> count
  |  h::t-> (match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
		if(List.length (make_argslist [] es) = 0) then 
		  find_func_without_arg_count (count+1) t
                else find_func_without_arg_count count t
    |    _->  find_func_without_arg_count count t )

let verify_st_for_rmv_frm_flist df_eq arglist exe_path =
  if (any_arg_found_df_eq df_eq arglist) then false
(*  else if ((find_diff_name_same_func (add_option arglist) 0 exe_path) mod 2) != 0 then false*)
  else if (not(same_arg_diff_func_name (refine_arglist df_eq arglist) df_eq exe_path)) && (List.length arglist !=0) then false
  else if (find_func_without_arg exe_path) && (List.length arglist = 0)   then false
  else true
   

let rec generate_flists_inner new_flists df_eq exe_path = function
  []->[]
  | h::t-> match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
               let arglist = remove_option (make_argslist [] es) in
(*       	 print_string("\nClean Statement\n");*)
(*               Printf.printf "\n\n\n%s\n\n" (Dumper.dump (clean_statement h));*)
(*               print_string("\nClean Statement list\n");*)
(*               Printf.printf "\n\n\n%s\n\n" (Dumper.dump (clean_statement_list  exe_path));*)
	       let prev_flist = if (List.length new_flists = 0 ) then [] else (List.hd new_flists) in
	       if((st_exists_in_list h exe_path) || ((verify_st_for_rmv_frm_flist df_eq arglist exe_path) 
		     && (not(st_exists_in_list h prev_flist)))) then 
		 generate_flists_inner new_flists df_eq exe_path t
	       else h::(generate_flists_inner new_flists df_eq exe_path t)
            
            | _-> h::(generate_flists_inner new_flists df_eq exe_path t)

let rec generate_flists df_eq flist new_flists = function
    []->new_flists
  | h::t-> let new_flist = generate_flists_inner new_flists df_eq h flist in 
             generate_flists df_eq flist (new_flists@[new_flist]) t
      

let rec is_flists_equv list = function
  []-> true
  | h::t-> if(compare_stmts_list h list) then 
              is_flists_equv list t
	   else false

let rec union_flists_inner stmt_list1 = function
  []->[]
  | h::t-> if(st_exists_in_list h stmt_list1) then
              h::(union_flists_inner stmt_list1 t)
	   else  
              h::(union_flists_inner stmt_list1 t)
                


let rec union_flists stmt_list = function
    []-> stmt_list
  | h::t-> let stmt_list = union_flists_inner h stmt_list in
           union_flists stmt_list t

let find_single_flist line_no iifunc1 flist = function
  []->[]
  | h::t-> if(is_flists_equv h t) then h
           else( 
(*                 let start_line = line_no  in  *)
(*		 let filename = Ast_c.file_of_info iifunc1 in  *)
(*		 let message = "Flist Problem" in	*)	        
(*                 print_string("* TODO [[view:"); *)
(*                 print_string(filename);  *)
(*                 print_string("::face=ovl-face1::linb=");  *)
(*                 print_string(string_of_int(line_no));  *)
(*                 print_string("::colb=");  *)
(*                 print_string(string_of_int(0));  *)
(*                 print_string("::cole=");  *)
(*                 print_string(string_of_int(0));  *)
(*                 print_string("][");  *)
(*                 print_string(message);  *)
(*                 print_string(" ");  *)
(*                 print_string(filename);  *)
(*                 print_string("::");  *)
(*                 print_string(string_of_int(line_no));  *)
(*                 print_string("]]");  *)
(*                 print_string("\n");   *)

		 flist)




let is_exp_null e  = 
  match e with 
    (((Ast_c.Ident (Ast_c.RegularName("NULL",[]))), typ), ii)-> true
  | _ -> false

let rec is_arg_live_in_path arg = function
   []-> true
  | h::t-> match Ast_c.unwrap h with              
              Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op, e2), typ), ii)) ->
                  if(compare_exp e1 arg) then 
		    if (is_exp_null e2) then false
                    else true
                  else is_arg_live_in_path arg t
            | _ -> is_arg_live_in_path arg t


let rec any_arg_live_in_path exe_path = function
  []-> true
  | h::t-> if(is_arg_live_in_path h (List.rev exe_path)) then any_arg_live_in_path exe_path t
           else false

let rec any_arg_live_in_mul_paths arglist = function
   []-> true
  | h::t-> if(any_arg_live_in_path h arglist) then any_arg_live_in_mul_paths arglist t
           else false


let rec already_have line_no h =  function
  []-> false
  |   (a,b)::t-> if(line_no = a) && (compare_stmts h b) then true
                else already_have line_no h t

let rec any_arg_used_in_if_exp if_exp = function
  []-> false
  |   h::t-> match if_exp with
              None -> false
    |  Some a -> if(is_return_id_in_expr  a h) then true
                        else any_arg_used_in_if_exp if_exp t



let rec find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths prefix_goto exp = function
  []->bug_list
  |  h::t->let ill_st_line = find_startline_no [h] in 
           match Ast_c.unwrap h with
           | Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident (Ast_c.RegularName("kfree",ii3))), typ1), ii1), es)), typ), ii))->
	        find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h]) exp t
           |  Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident (Ast_c.RegularName("platform_device_put",ii3))), 
							   typ1), ii1), es)), typ), ii))->
                find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h]) exp t

           |  Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident (Ast_c.RegularName("put_device",ii3))), 
							   typ1), ii1), es)), typ), ii))->
                find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h]) exp t

           | Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident (Ast_c.RegularName("of_node_put",ii3))), 
							  typ1), ii1), es)), typ), ii))->
                find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h]) exp t
           | Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident (Ast_c.RegularName("kfree_skb",ii3))), typ1), ii1), es)), typ), ii))->
                find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h])  exp t
           |  Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident 
							      (Ast_c.RegularName("nla_nest_cancel",ii3))), typ1), ii1), es)), typ), ii))->
	       find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h])  exp t
           |   Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident
                                                              (Ast_c.RegularName("vfree",ii3))), typ1), ii1), es)), typ), ii))->
               find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h])  exp t

           |   Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident
                                                              (Ast_c.RegularName("rules_ops_put",ii3))), typ1), ii1), es)), typ), ii))->
               find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h])  exp t
           |    Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident
                                                              (Ast_c.RegularName("dst_release",ii3))), typ1), ii1), es)), typ), ii))->
               find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h])  exp t

      
           |     Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident
                                                              (Ast_c.RegularName("rfkill_destroy",ii3))), typ1), ii1), es)), typ), ii))->
               find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h])  exp t

           |      Ast_c.ExprStatement (Some (((Ast_c.FunCall((((Ast_c.Ident
                                                              (Ast_c.RegularName("pci_dev_put",ii3))), typ1), ii1), es)), typ), ii))->
               find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h])  exp t

           | Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))-> 
	       let arglist1 = remove_option (make_argslist [] es) in
               let arglist2 = refine_arglist df_eq arglist1 in
        
               if (List.length arglist2 = 0) then 
		  if (not(any_arg_live_in_mul_paths  arglist1 exe_paths))
		      && (not (has_string h))
		      && (any_arg_used_in_if_exp exp arglist1) (* Have change this condition*)
		     then ( 
                       let filename = Ast_c.file_of_info iifunc1 in
                       let message = "illegale Access" in
                       print_string("* TODO [[view:");
                       print_string(filename);
                       print_string("::face=ovl-face1::linb=");
                       print_string(string_of_int(line_no));
                       print_string("::colb=");
                       print_string(string_of_int(0));
                       print_string("::cole=");
                       print_string(string_of_int(0));
                       print_string("][");
                       print_string(message);
                       print_string(" ");
                       print_string(filename);
                       print_string("::");
                       print_string(string_of_int(ill_st_line));
                       print_string("]]");
                       print_string("\n");

                       print_string("** [[view:");
                       print_string(filename);
                       print_string("::face=ovl-face2::linb=");
                       print_string(string_of_int(ill_st_line));
                       print_string("::colb=");
                       print_string(string_of_int(0));
                       print_string("::cole=");
                       print_string(string_of_int(0));
                       print_string("][");
                       print_string(message);
                       print_string(" ");
                       print_string(filename);
                       print_string("::");
                       print_string(string_of_int(ill_st_line));
                       print_string("]]");
                       print_string("\n");

		       find_illegale_st (bug_list@[(line_no,h)]) line_no iifunc1 lbl_list df_eq exe_paths prefix_goto exp t
		  )
                  else find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h]) exp t
               else find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h]) exp t
          |  Ast_c.Jump (Ast_c.Goto name) ->
                 let goto_code = gather_goto_code lbl_list [] [] [h] in
                 find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h]) exp goto_code
          |  _-> find_illegale_st bug_list line_no iifunc1 lbl_list df_eq exe_paths (prefix_goto@[h]) exp t





let rec exp_exists_list exp  = function
  []-> false
  |   h::t-> if(compare_exp exp h) then true
           else exp_exists_list exp t



let rec any_exp_exists_list arglist2  = function
  []-> false
  |  h::t-> if (exp_exists_list h arglist2 ) then true
           else any_exp_exists_list arglist2 t


let  any_arg_used_in_return st arglist =
  match st with
     None-> false
  |  Some a ->  if( exp_exists_list a arglist) then true
                else false


let rec arg_exists_in_ref_list_inner arg = function
   []-> false
  |  (a,b)::t -> if(exp_exists_in_list arg b) then true
             else arg_exists_in_ref_list_inner arg t

let rec arg_exists_in_ref_list ref_list = function
   []-> false
  |  h::t-> if(arg_exists_in_ref_list_inner  h ref_list) then true
           else arg_exists_in_ref_list ref_list t

let rec refine_ref_list arglist2 = function
  []-> []
  |  (a,b)::t->if(exp_exists_in_list a arglist2) then (a,b)::(refine_ref_list arglist2 t)
               else refine_ref_list arglist2 t

let rec is_link arglist1  ref_list = function
    []-> false
  |  h::t-> ( match Ast_c.unwrap h with
               Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
                 let arglist2 =  remove_option (make_argslist [] es)  in
                 let new_ref_list =  refine_ref_list  arglist2 ref_list in
                 if(arg_exists_in_ref_list new_ref_list arglist1) then true
                 else is_link arglist1  ref_list t
             | _ -> is_link arglist1  ref_list t
            )


let rec find_return_st = function
  []-> None
  |  h::t -> match Ast_c.unwrap h with
            Ast_c.Jump (Ast_c.ReturnExpr e) -> Some e
    |  _ -> find_return_st t



let rec find_close_branch line_no1 st paths lbl_list = function
  []-> paths
| (line_no, if_code, lbl_code, status, exe_paths, df_eq, ref_list, exp)::t->
  let goto_code = gather_goto_code lbl_list [] [] lbl_code  in 
  let goto_code = remove_cast goto_code in
     if(st_exists_in_list st goto_code) then (if(List.length exe_paths>0) then (List.hd exe_paths) else [])
     else find_close_branch line_no1 st paths lbl_list t

let rec find_common_path  common_path exe_path1  exe_path2  =
  match (exe_path1, exe_path2) with
   ([],[])-> common_path
  |   ([] ,_) -> common_path
  |   (_, [])-> common_path
  |   (h::t, h1::t1) -> if (compare_stmts h h1) then find_common_path  (common_path@[h]) t t1
                        else common_path
  
let rec find_subtract_path  common_path single_path_own =
   match (common_path , single_path_own) with
   ([],[])-> []
   |   ([] ,h::t) -> t
   |   (_, [])-> []
   |   (h::t, h1::t1) -> if (compare_stmts h h1) then find_subtract_path   t t1
                         else t1




let diff_name_same_func_in_path arglist if_list exe_path st lbl_list line_no =
      let nearest_branch_paths = find_close_branch line_no st exe_path lbl_list (List.rev if_list) in
      let single_path_nearest =  nearest_branch_paths in
      let single_path_own =   exe_path  in
      let common_path = find_common_path  [] single_path_nearest single_path_own in
      let subtract_path = find_subtract_path common_path single_path_own in
      if (((find_diff_name_same_func (add_option arglist) 0 subtract_path) mod 2) != 0) then true
      else false

let rec diff_name_same_func_in_path_outer arglist if_list st lbl_list line_no = function
  []-> false
  | h::t-> if(diff_name_same_func_in_path arglist if_list h st lbl_list line_no) then true
           else diff_name_same_func_in_path_outer arglist if_list st lbl_list line_no t




let rec insert_into_flist flist lbl_list if_list exe_paths line_no df_eq = function
  []->flist
  |  h::t-> match Ast_c.unwrap h with
       


    |  Ast_c.ExprStatement (Some (((Ast_c.FunCall  (((((Ast_c.Ident (Ast_c.RegularName(id,ii2))), typ1), ii1)), es)), typ), ii))->
	       let arglist = remove_option (make_argslist [] es) in
               let arglist1 = refine_arglist df_eq arglist in

               if (not(has_string h)) && (not(st_exists_in_list h flist)) && id =~ "^[a-z_][a-z_0-9]*$"
               && (List.length arglist !=0) && (List.length arglist1 !=0)then
                 insert_into_flist (flist@[h]) lbl_list if_list exe_paths line_no  df_eq t
               else if (List.length arglist =0) && (not(has_string h)) && id =~ "^[a-z_][a-z_0-9]*$" && (not(st_exists_in_list h flist)) then
                 insert_into_flist (flist@[h]) lbl_list if_list exe_paths line_no df_eq t
               else if (not(has_string h)) && id =~ "^[a-z_][a-z_0-9]*$" && (not(st_exists_in_list h flist))
		          && (List.length arglist !=0) && (List.length arglist1 =0)
               && (diff_name_same_func_in_path_outer arglist if_list h  lbl_list line_no exe_paths ) then
                 insert_into_flist (flist@[h]) lbl_list if_list exe_paths line_no df_eq t
               else insert_into_flist flist lbl_list if_list exe_paths  line_no df_eq t
    |  Ast_c.Jump (Ast_c.Goto name) ->
                 let goto_code = gather_goto_code lbl_list [] [] [h] in
                 let goto_code = remove_cast goto_code in
                 insert_into_flist flist lbl_list if_list exe_paths  line_no df_eq goto_code
    |  _-> insert_into_flist flist lbl_list if_list exe_paths  line_no df_eq t



let rec func_call_in_ass  df_eq st = function
   []-> false
  | h::t-> (match Ast_c.unwrap h with
                 Ast_c.ExprStatement (Some ((Ast_c.Assignment ((((Ast_c.FunCall  (e, es)), typ4), ii5), (Ast_c.SimpleAssign),
                                                       (( Ast_c.Ident (Ast_c.RegularName("NULL",ii4)), typ2), ii3)), typ1), ii2))->
                        let arglist = remove_option (make_argslist [] es)  in
                        if same_arg_diff_func_name arglist df_eq [st] then true
                        else func_call_in_ass  df_eq st t
               |_ -> func_call_in_ass  df_eq st t
            )




let rec find_list_for_df_eq_frm_goto_st st new_st_list  = function
  []-> new_st_list
  | h::t-> if(compare_stmts st h) then new_st_list
           else find_list_for_df_eq_frm_goto_st st (new_st_list@[h]) t


let rec find_rest_args arglist = function
  []-> []
  |  h::t-> if(not(exp_exists_in_list h arglist)) then
                 h::(find_rest_args arglist t)
           else find_rest_args arglist t


let rec arg_exists_in_goto_code arglist arglist1 = function
  []-> false
  | h::t->match Ast_c.unwrap h with
            Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
              if (not(has_string h)) then
                let arglist2 = remove_option (make_argslist [] es)  in
                if any_exp_exists_list arglist2 arglist1 then 
                   let rest_args = find_rest_args arglist2 arglist1 in
                   if args_exists_inner (List.hd arglist) (exp_conv_to_st rest_args)   then true
                   else arg_exists_in_goto_code arglist arglist1 t 
                else arg_exists_in_goto_code arglist arglist1 t
              else arg_exists_in_goto_code arglist arglist1 t
         |_ -> arg_exists_in_goto_code arglist arglist1 t 

let rec arg_exists_in_rest_flist_inner st df_eq arglist1 = function
  []-> false 
  | h::t->match Ast_c.unwrap h with
          Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
           let arglist2 = remove_option (make_argslist [] es) in
           let arglist2 = refine_arglist df_eq arglist2 in
           if any_exp_exists_list arglist2 arglist1 then true
           else arg_exists_in_rest_flist_inner st df_eq arglist1 t
          | _ -> arg_exists_in_rest_flist_inner st df_eq arglist1 t 

let rec arg_exists_in_rest_flist st df_eq arglist1 =function
  []-> false
  |  h::t-> if(compare_stmts h st) then 
              arg_exists_in_rest_flist_inner st df_eq arglist1 t
            else arg_exists_in_rest_flist st df_eq arglist1 t



let rec any_exp_exists_in_rest_flist_loop2 e = function
  []-> false
  | h::t-> if (is_return_id_in_expr e h) then true
           else any_exp_exists_in_rest_flist_loop2 e t


let rec any_exp_exists_in_rest_flist_loop arglist1 = function
  []->false
  | h::t-> if(any_exp_exists_in_rest_flist_loop2 h arglist1) then true
           else any_exp_exists_in_rest_flist_loop arglist1 t

let rec any_exp_exists_in_rest_flist arglist = function
  []->false
  | h::t->match Ast_c.unwrap h with
            Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
              let arglist1 = remove_option (make_argslist [] es) in

              if  any_exp_exists_in_rest_flist_loop arglist arglist1 then true
              else any_exp_exists_in_rest_flist arglist t
          | _-> any_exp_exists_in_rest_flist arglist t 



let rec linked_by_func arglist goto_code = function
  []-> false
  | h::t-> match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
                   let arglist1 = remove_option (make_argslist [] es) in                   


                   if (List.length arglist1 >0) then 
                      if(arg_exists_in_goto_code arglist arglist1  goto_code) then true
                      else linked_by_func arglist goto_code t 
                   else linked_by_func arglist goto_code t
          | _-> linked_by_func arglist goto_code t 


let rec build_arglist arglist = function
  []-> arglist
  | (a,b)::t-> if(exp_exists_in_list a arglist) then
                  build_arglist (arglist@[b]) t
               else build_arglist arglist t

let rec translate_arglist arglist paramset = 
  match (arglist, paramset) with
    ([],[])-> []
  | (h::t,h1::t1)-> (h,h1)::(translate_arglist t t1)

let rec find_function_frm_all_func callin_func = function
  []-> None
  | (name,paramset,stmts)::t->
            ( match Ast_c.unwrap callin_func with
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (id)), typ1), ii1), es)), typ), ii)) ->
	       if (compare_name name id) then
                   let arglist = remove_option (make_argslist [] es) in
		   if(List.length arglist)= (List.length paramset) then 
                      let trans_arglist = translate_arglist arglist paramset in
                      Some (name, paramset, trans_arglist ,stmts)
                   else find_function_frm_all_func callin_func t
               else find_function_frm_all_func callin_func t
            | _ -> find_function_frm_all_func callin_func t
            )  




let rec check_for_calling_func st trans_arglist all_function = function
  []-> false
  | h::t->(
        match Ast_c.unwrap h with  
      |  Ast_c.Labeled (Ast_c.Label (name, st)) -> 
	  if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
	  else check_for_calling_func st trans_arglist  all_function  t
      |	 Ast_c.Labeled (Ast_c.Case  (e, st)) ->  
	  if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
          else check_for_calling_func st trans_arglist  all_function t
      |	 Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) -> 
	  if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
          else check_for_calling_func st trans_arglist  all_function t

      |	 Ast_c.Labeled (Ast_c.Default st) -> 
          if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
          else check_for_calling_func st trans_arglist  all_function t
      
      |	 Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (ident)), typ1), ii1), es)), typ), ii)) -> 
          (match Ast_c.unwrap st with 
               Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (ident1)), typ4), ii4), es1)), typ3), ii3)) ->
		 let arglist1 = remove_option (make_argslist [] es1) in
		 if(compare_stmts st h) then true
	(*	 else if (compare_name ident ident1) && (any_exp_exists_list trans_arglist arglist1) then true*)
                 else ( match (find_function_frm_all_func h all_function) with 
		         None-> check_for_calling_func st trans_arglist  all_function t
                       | Some (name,paramset,args,stmts)->
			   let trans_arglist1 = (*build_arglist trans_arglist args *) trans_arglist in
			       if (check_for_calling_func st trans_arglist1  all_function stmts) then true
			       else check_for_calling_func st trans_arglist  all_function t
                      )
               | _ -> check_for_calling_func st trans_arglist  all_function t  

          )

      |	 Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> 
          if(check_for_calling_func st trans_arglist  all_function (create_compound_st st1)) then true
          else if (check_for_calling_func st trans_arglist  all_function (create_compound_st st2)) then true
          else check_for_calling_func st trans_arglist  all_function t
      |	 Ast_c.Selection  (Ast_c.Switch (e, st)) -> 
          if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
          else check_for_calling_func st trans_arglist  all_function t
      |	 Ast_c.Iteration  (Ast_c.While (e, st)) -> 
          if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
          else check_for_calling_func st trans_arglist  all_function t
      |	 Ast_c.Iteration  (Ast_c.DoWhile (st, e)) -> 
          if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
          else check_for_calling_func st trans_arglist  all_function t
      |	 Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) -> 
          if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
          else check_for_calling_func st trans_arglist  all_function  t
      |	 Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> 
          if(check_for_calling_func st trans_arglist  all_function (create_compound_st st)) then true
          else check_for_calling_func st trans_arglist  all_function t
      |  _ -> check_for_calling_func st trans_arglist  all_function t
    )

let rec find_func_frm_all_function_main st arglist all_function = function
  []-> false
  | h::t-> match Ast_c.unwrap h with
            Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (ident)), typ1), ii1), es)), typ), ii)) ->
                (match (find_function_frm_all_func h all_function) with
                  None-> find_func_frm_all_function_main st arglist all_function t
                | Some (name,paramset,args,stmts)-> 
                    let trans_arglist = (*build_arglist arglist args*) arglist in
                               if (check_for_calling_func st trans_arglist  all_function stmts) then true
                               else find_func_frm_all_function_main st arglist all_function t               
                )
            | _-> find_func_frm_all_function_main st arglist all_function t
           

let rec any_exp_used_list_inner exp = function
  []-> false
  | h::t-> if (is_return_id_in_expr  exp h) then true
           else any_exp_used_list_inner exp t

let rec any_exp_used_list arglist = function
  []-> false
  | h::t-> if any_exp_used_list_inner h arglist  then true
           else any_exp_used_list arglist t


let call_func_return_err_inner exe_path_st e arglist =
  match Ast_c.unwrap exe_path_st  with 
    Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, ((Ast_c.FunCall  (ex, es), typ1), ii1))), typ), ii)) ->
      if(compare_exp e1 e) then
	let arglist1 = remove_option (make_argslist [] es) in
	if(any_exp_used_list arglist1 arglist) || (List.length arglist1 = 0) then true
	else false 
      else false
  | _-> false

let rec call_func_return_err expr arglist exe_path = function
  []-> false
  | h::t->(match expr with
             None-> false
           | Some exp ->
               (match Ast_c.unwrap h with
                 Ast_c.Jump (Ast_c.ReturnExpr e) ->
	           if (is_return_id_in_expr exp e) && (List.length exe_path >1) then

		       if call_func_return_err_inner (List.hd  (List.tl (List.rev exe_path))) e arglist then true
		       else call_func_return_err expr arglist exe_path t 
	           else call_func_return_err expr arglist exe_path t 
                 | _ -> call_func_return_err expr arglist exe_path t )
	  )

let rec remove_ele_from_list ele = function
   []->[]
  | h::t-> if(compare_exp ele h) then remove_ele_from_list ele t
           else h::(remove_ele_from_list ele t)

let func_for_link_count islink h = 
           if(exp_exists_in_list h !nolinklist) = false  && (exp_exists_in_list h !linklist) = false  && islink = false then (
                 nolinklist := h::!nolinklist;
                 nolink := !nolink + 1;
                 true
           )
           else if islink = true  && (exp_exists_in_list h !nolinklist) = false && (exp_exists_in_list h !linklist) = false then (
                 linklist := h::!linklist;
      	         link := !link + 1;
	         true
           )
	   else if islink = true  && (exp_exists_in_list h !nolinklist) = true  && (exp_exists_in_list h !linklist) = false then (
                 linklist := h::!linklist;
                 nolinklist := remove_ele_from_list h !nolinklist;
                 link := !link + 1;
                 nolink := !nolink - 1;
                 true
           )
           else false

let func_for_return_count isret  h =
           if(exp_exists_in_list h !noretlist) = false  && (exp_exists_in_list h !retlist) = false  && isret = false then (
                 noretlist := h::!noretlist;
                 noret := !noret + 1;
                 true
           )
           else if isret = true  && (exp_exists_in_list h !noretlist) = false && (exp_exists_in_list h !retlist) = false then (
                 retlist := h::!retlist;
                 ret := !ret + 1;
                 true
           )
           else if isret = true  && (exp_exists_in_list h !noretlist) = true  && (exp_exists_in_list h !retlist) = false then (
                 retlist := h::!retlist;
                 noretlist := remove_ele_from_list h !noretlist;
                 ret := !ret + 1;
                 noret := !noret - 1;
                 true
           )
           else false


let func_for_call_func_return_err err_count h =
           if(exp_exists_in_list h !noerrlist) = false  && (exp_exists_in_list h !errlist) = false  && err_count = false then (
                 noerrlist := h::!noerrlist;
	              noerr := !noerr + 1;
                 true
           )
           else if err_count = true  && (exp_exists_in_list h !noerrlist) = false && (exp_exists_in_list h !errlist) = false then (
                 errlist := h::!errlist;
                 err := !err + 1;
                 true
           )
           else if err_count = true  && (exp_exists_in_list h !noerrlist) = true  && (exp_exists_in_list h !errlist) = false then (
                 errlist := h::!errlist;
                 noerrlist := remove_ele_from_list h !noerrlist;
                 err := !err + 1;
                 noerr := !noerr - 1;
                 true
           )
           else false



let func_for_interproc_count interpro h =
           if(exp_exists_in_list h !nointerproclist) = false  && (exp_exists_in_list h !interproclist) = false  && interpro = false then (
                 nointerproclist := h::!nointerproclist;
                 nointerproc := !nointerproc + 1;
                 true
           )
           else if interpro = true  && (exp_exists_in_list h !nointerproclist) = false && (exp_exists_in_list h !interproclist) = false then (
                 interproclist := h::!interproclist;
                 interproc := !interproc + 1;
                 true
           )
           else if interpro = true  && (exp_exists_in_list h !nointerproclist) = true  && (exp_exists_in_list h !interproclist) = false then (
                 interproclist := h::!interproclist;
                 nointerproclist := remove_ele_from_list h !nointerproclist;
                 interproc := !interproc + 1;
                 nointerproc := !nointerproc - 1;
                 true
           )
           else false
              
let rec update_global_list arg st new_st new_list mark = function

  []->   if (compare_stmts st new_st) then
            ( if mark = true then (
              (new_list@[(arg,[st])])
              )
              else new_list
            )
         else(

              if mark = true then(

                  (new_list@[(arg,([st]@[new_st]))])
             )
              else new_list
             )

  | (a,[])::t -> if (compare_exp a arg) then 
                        (

                         update_global_list arg st new_st (new_list@[(a,([st]@[new_st]))]) false t
                        )
             else (

                   update_global_list arg st new_st (new_list@[(a,[])]) mark t
                  )
  | (a,b)::t ->  if (not(compare_exp a arg)) then(

                    update_global_list arg st new_st (new_list@[(a,b)]) mark t
                 )
                 else if(compare_exp a arg) && (not(st_exists_in_list st b)) && (not(st_exists_in_list new_st b))
                    then(
		        update_global_list arg st new_st (new_list@[(a,(b@[st]@[new_st]))]) false t
                        )
                 else if (compare_exp a arg) && (st_exists_in_list st b) && (not(st_exists_in_list new_st b))
                    then(
                          update_global_list arg st new_st (new_list@[(a,(b@[new_st]))]) false t
                    )
                 else (
                         update_global_list arg st new_st (new_list@[(a,b)]) false t)


let func_for_same_arg_diff_name diff_face st arglist new_df_eq goto_code arg  =
   if diff_face = true then
        
	  match (fetch_st_same_arg_diff_func_name arglist new_df_eq None goto_code) with 
            None -> !same_arg_diff_name_list
          | Some a-> update_global_list arg st a [] true !same_arg_diff_name_list
    else  update_global_list arg st st [] true !same_arg_diff_name_list


let is_it_bug  all_function goto_code arglist exe_path df_eq ref_list style exp if_list st line_no lbl_list flist = 

    let list_for_df_eq = find_list_for_df_eq_frm_goto_st st [] goto_code in
    let new_df_eq = exp_list_defined_in_df_eq df_eq list_for_df_eq  in

    let arglist1 = refine_arglist new_df_eq arglist in
    let arglist2 = remove_integer arglist1 in

    (*                                     print_string("\narglist:\n"); *)
  (*                                        print_string(string_of_int(List.length arglist));*)
(*                                           Printf.printf "\n\n\n%s\n\n" (Dumper.dump arglist);*)


                                           (*  print_string("\narglist1:\n"); *)
  (*                                         print_string(string_of_int(List.length arglist1)); *)
(*                                            Printf.printf "\n\n\n%s\n\n" (Dumper.dump (clean_exp_list arglist1));*)

      (*                                      print_string("\ndiff name same func:\n"); *)
  (*                                          print_string(string_of_int(List.length arglist1)); *)
(*                                            Printf.printf "\n\n\n%s\n\n" (Dumper.dump (find_diff_name_same_func (add_option arglist) 0 exe_path) ); *)
    let exp_st = match exp with
                 None -> ((Ast_c.ExprStatement None),[])
                | Some a -> ((Ast_c.ExprStatement (Some a)),[]) 
    in
    test_count29:= !test_count29+1;

    if(List.length arglist = 0) &&  (find_func_without_arg exe_path) && (find_func_without_arg goto_code) then (test_count28:= !test_count28+1;false)
    else if(List.length arglist = 0) &&  (count_find_func_without_arg 0 exe_path) >1 then (test_count27:= !test_count27+1;false)
    else if (List.length arglist = 0) &&  (not (find_func_without_arg exe_path)) then (test_count26:= !test_count26+1;false)
    else if (List.length arglist > 1) then (test_count25:= !test_count25+1; test_count29:= !test_count29-1; false)
    else if is_link arglist ref_list (skip_return goto_code) then (
            if (List.length arglist !=0) then (func_for_interproc_count false (List.hd arglist);
            same_arg_diff_name_list:= func_for_same_arg_diff_name false st  arglist new_df_eq goto_code (List.hd arglist);
            func_for_link_count true (List.hd arglist);
            func_for_return_count false (List.hd arglist);
            func_for_call_func_return_err false (List.hd arglist);
            test_count24:= !test_count24+1;false)
            else (test_count23:= !test_count23+1; false))
    else if find_func_frm_all_function_main st arglist1 all_function  goto_code then (
            if (List.length arglist !=0) then (
            same_arg_diff_name_list:= func_for_same_arg_diff_name false st  arglist new_df_eq goto_code (List.hd arglist);
            func_for_return_count false (List.hd arglist);
            func_for_call_func_return_err false (List.hd arglist);
            func_for_interproc_count true (List.hd arglist);
            func_for_link_count false (List.hd arglist);
            test_count22:= !test_count22+1;false)
            else (test_count21:= !test_count21+1;false))


    else if (List.length arglist = 1) && (linked_by_func (add_option arglist) goto_code exe_path) then (
            if (List.length arglist !=0) then (
(*            same_arg_diff_name_list:= func_for_same_arg_diff_name false st  arglist new_df_eq goto_code (List.hd arglist);*)
(*             func_for_return_count false (List.hd arglist);*)
(*             func_for_call_func_return_err false (List.hd arglist);*)
(*             func_for_interproc_count false (List.hd arglist);*)
(*             func_for_link_count false (List.hd arglist); *)
            test_count20:= !test_count20+1;false)
            else (test_count19:= !test_count19+1;false))

    else if  (call_func_return_err exp arglist1 exe_path goto_code) then  (
            if (List.length arglist !=0) then (func_for_interproc_count false (List.hd arglist);
            same_arg_diff_name_list:= func_for_same_arg_diff_name false st  arglist new_df_eq goto_code (List.hd arglist);
            func_for_link_count false (List.hd arglist);
            func_for_return_count false (List.hd arglist);
            func_for_call_func_return_err true (List.hd arglist);
            test_count18:= !test_count18+1;false)
            else (test_count17:= !test_count17+1;false))

(*  && ((not(find_func_without_arg goto_code))&& (not(find_func_without_arg [exp_st]))) then true*)
(*    else if(List.length arglist = 0) &&*)
 (*     (not(find_func_without_arg exe_path)) && (find_func_without_arg goto_code) then false*)
(*    else if(List.length arglist = 0) &&*)
(*      (find_func_without_arg exe_path) && (st_exists_in_list st (goto_code@[exp_st])) then false*)

(*((find_func_without_arg goto_code)||(find_func_without_arg [exp_st])) then false*)
(*    else if(List.length arglist = 0) &&*)
(*      (not(find_func_without_arg exe_path)) && (not(find_func_without_arg goto_code)) then false*)
(*    else if style = Backward && ((any_arg_used_in_if_exp exp arglist)) then false*)
    else if style = Backward && ((any_arg_used_in_if_exp exp arglist) ||
            (same_arg_diff_func_name arglist new_df_eq [((Ast_c.ExprStatement exp),[])])) then  (
            test_count29:= !test_count29-1;false)
            

(*    else if (verify_st_for_rmv_frm_flist df_eq arglist goto_code) then false*)
    else if any_arg_used_in_return (find_return_st goto_code) arglist1 then  (
            if (List.length arglist !=0) then (func_for_interproc_count false (List.hd arglist);
            same_arg_diff_name_list:= func_for_same_arg_diff_name false st  arglist new_df_eq goto_code (List.hd arglist);
            func_for_link_count false (List.hd arglist);
            func_for_return_count true (List.hd arglist);
            func_for_call_func_return_err false (List.hd arglist);
            test_count14:= !test_count14+1;false)
            else (test_count13:= !test_count13+1;false))

(*    else if is_link arglist ref_list (skip_return goto_code) then (  *)
  (*          func_for_link_count true (List.hd arglist);*)
(*            else false)*)
    else if (same_arg_diff_func_name arglist new_df_eq goto_code) && (List.length arglist !=0) then  (
               if (List.length arglist !=0) then (
                       func_for_return_count false (List.hd arglist);
                       same_arg_diff_name_list:= func_for_same_arg_diff_name true st  arglist new_df_eq goto_code (List.hd arglist);                                                                     func_for_interproc_count false (List.hd arglist); 
                       func_for_link_count false (List.hd arglist);
                       func_for_call_func_return_err false (List.hd arglist);
                       test_count12:= !test_count12+1;false)
               
               else (test_count11:= !test_count11+1;false))

(*    else if (same_arg_diff_func_name_simple arglist  df_eq exe_path) && (List.length arglist !=0) then (print_string("XXXXXXXXXXX1");false)*)
(*    else if (not(diff_name_same_func_in_path  arglist if_list exe_path st  lbl_list line_no)) then true*)
    else if (func_call_in_ass new_df_eq st exe_path) then (
            if (List.length arglist !=0) then (
            func_for_return_count false (List.hd arglist);
            same_arg_diff_name_list:= func_for_same_arg_diff_name true st  arglist new_df_eq exe_path (List.hd arglist);
            func_for_interproc_count false (List.hd arglist);
            func_for_link_count false (List.hd arglist);
            func_for_call_func_return_err false (List.hd arglist);
            test_count10:= !test_count10+1;false)
            else (test_count9:= !test_count9+1;false))

    else if (diff_name_same_func_in_path  arglist if_list exe_path st  lbl_list line_no) then (
            if (List.length arglist !=0) then (
            func_for_return_count false (List.hd arglist);
            same_arg_diff_name_list:= func_for_same_arg_diff_name true st  arglist new_df_eq exe_path (List.hd arglist);
            func_for_interproc_count false (List.hd arglist);
            func_for_link_count false (List.hd arglist);
            func_for_call_func_return_err false (List.hd arglist);
            test_count8:= !test_count8+1;false)
            else (test_count7:= !test_count7+1;false))

(*    else if (((find_diff_name_same_func (add_option arglist) 0 exe_path) mod 2) == 0) then (print_string("XXXXXXXXXX9X");false)*)
(*    else if (List.length arglist != 0) && (List.length arglist1 = 0) &&*)
  (*          (((find_diff_name_same_func (add_option arglist) 0 exe_path) mod 2) = 0)*)
(*    then (print_string("XXXXXXXXXXX3");false)*)
    else if (arg_exists_in_rest_flist st df_eq arglist1 flist) || (arg_exists_in_rest_flist_inner st df_eq arglist1 goto_code) 
       then  (
            if (List.length arglist !=0) then (
            test_count6:= !test_count6+1;false)
            else (test_count5:= !test_count5+1; test_count29:= !test_count29-1; false))

(*    else if (List.length arglist1!=0) && (any_exp_exists_in_rest_flist arglist1 goto_code) then false*)
    else if (List.length arglist2 = 0) && (List.length arglist1 != 0) then (test_count4:= !test_count4+1; test_count29:= !test_count29-1; false)
    else if (List.length arglist != 0) && (List.length arglist1 = 0) && 
            (((find_diff_name_same_func (add_option arglist) 0 exe_path) mod 2) = 0) then (test_count3:= !test_count3+1; test_count29:= !test_count29-1; false)
    else (if (List.length arglist !=0) then (same_arg_diff_name_list:= func_for_same_arg_diff_name false st  arglist new_df_eq goto_code (List.hd arglist);
                                             func_for_interproc_count false (List.hd arglist);
                                             func_for_link_count false (List.hd arglist);
                                             func_for_call_func_return_err false (List.hd arglist);
                                             func_for_return_count false (List.hd arglist);
                                             test_count1:= !test_count1+1; true) else (test_count2:= !test_count2+1;true))


let rec is_it_bug_in_paths all_function goto_code arglist  df_eq ref_list style exp if_list st line_no lbl_list flist = function
  []-> false
  |  h::t-> if(is_it_bug  all_function goto_code arglist h df_eq ref_list style exp if_list st line_no lbl_list flist ) then true
           else is_it_bug_in_paths all_function goto_code arglist  df_eq ref_list style exp if_list st line_no lbl_list flist t


let rec already_have line_no h =  function
  []-> false
  |  (a,b)::t-> if(line_no = a) && (compare_stmts h b) then true 
                else already_have line_no h t

let rec find_missing_error_handling_code all_function 
    bug_list exe_paths line_no iifunc1 goto_code df_eq ref_list style exp if_list lbl_list  flist = function
    []-> bug_list
  | h::t-> let st_lineno = find_startline_no [h] in
           match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
                let arglist = remove_option (make_argslist [] es) in
                if(st_exists_in_list h goto_code) || (any_exp_exists_in_rest_flist arglist goto_code) then ( 
                   find_missing_error_handling_code all_function  
		    bug_list exe_paths line_no iifunc1 goto_code df_eq ref_list style exp if_list lbl_list flist t)
	        else if(is_it_bug_in_paths all_function goto_code arglist 
			  df_eq ref_list style exp if_list h line_no lbl_list flist exe_paths) 
		        && (not(already_have line_no h bug_list))then 
		  (
                       let start_line = line_no  in
                       let filename = Ast_c.file_of_info iifunc1 in
                       let message = 
			 match style with
			   Forward-> "Missing BUG:Forward" 
			 | Backward->"Missing BUG:Backward"
		       in
                       print_string("* TODO [[view:");
                       print_string(filename);
                       print_string("::face=ovl-face1::linb=");
                       print_string(string_of_int(line_no));
                       print_string("::colb=");
                       print_string(string_of_int(0));
                       print_string("::cole=");
                       print_string(string_of_int(0));
                       print_string("][");
                       print_string(message);
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
                       print_string(message);
                       print_string(" ");
                       print_string(filename);
                       print_string("::");
                       print_string(string_of_int(st_lineno));
                       print_string("]]");
                       print_string("\n");

	           find_missing_error_handling_code all_function 
			 (bug_list@[(line_no,h)]) exe_paths line_no iifunc1 goto_code df_eq ref_list style exp if_list lbl_list flist t
		  )
	        else 
	           find_missing_error_handling_code all_function 
		    bug_list exe_paths line_no iifunc1 goto_code df_eq ref_list style exp if_list lbl_list flist t
            | _ -> find_missing_error_handling_code all_function 
		  bug_list exe_paths line_no iifunc1 goto_code df_eq ref_list style exp if_list lbl_list flist t 
             


let rec find_illegale_bugs illegale_list iifunc1 lbl_list = function
  []-> illegale_list
  |  (line_no, if_code, lbl_code, status, exe_paths, df_eq, ref_list, exp)::t->
      let goto_code = gather_goto_code lbl_list [] [] lbl_code  in
      let illegale_list_new = find_illegale_st [] line_no iifunc1 lbl_list df_eq exe_paths [] exp goto_code in
          find_illegale_bugs (illegale_list@illegale_list_new) iifunc1 lbl_list t

let rec find_bugs all_function iifunc1 flist lbl_list bug_list style if_list = function
  []-> bug_list
| (line_no, if_code, lbl_code, status, exe_paths, df_eq, ref_list, exp)::t-> 
      let goto_code = remove_cast (gather_goto_code lbl_list [] [] lbl_code)  in
      match exp with 
         None-> find_bugs all_function iifunc1 flist lbl_list bug_list style if_list t
      |	 Some e ->
               if(has_return_bug_new e goto_code) then 
                  let flists = generate_flists df_eq flist [] exe_paths in 
                           (*              print_string("\nFlists:\n"); *)
        (*                                 print_string(string_of_int(List.length flists)); *)
      (*                                   Printf.printf "\n\n\n%s\n\n" (Dumper.dump flists); *)
                  let single_flist = find_single_flist line_no iifunc1 flist flists in
    (*                                       print_string("\nSingle flist:\n"); *)
  (*                                         print_string(string_of_int(List.length single_flist)); *)
(*                                           Printf.printf "\n\n\n%s\n\n" (Dumper.dump single_flist); *)
      
                  let bug_list  = find_missing_error_handling_code all_function bug_list exe_paths line_no iifunc1 
                             goto_code df_eq ref_list style exp if_list lbl_list single_flist single_flist in
                  let lbl_code = remove_cast lbl_code in
                  let flist = insert_into_flist single_flist lbl_list if_list exe_paths  line_no df_eq lbl_code  in
          (*                               print_string("\nlbl_code:\n"); *)
  (*                                       print_string(string_of_int(List.length lbl_code)); *)
(*                                         Printf.printf "\n\n\n%s\n\n" (Dumper.dump lbl_code); *)
                  find_bugs all_function iifunc1 flist lbl_list bug_list style if_list t
               else find_bugs all_function iifunc1 flist lbl_list bug_list style if_list t




let rec generate_execution_paths_new_main exe_paths = function
  []->exe_paths
  | e::es->
         match Ast_c.unwrap e with 
             |  Ast_c.ExprStatement (Some exp) -> 
		 generate_execution_paths_new_main (List.map (function (s ) -> (e::s)) exe_paths) es (*backward*)
             |  Ast_c.ExprStatement (None) ->
		 generate_execution_paths_new_main (List.map (function (s ) -> (e::s)) exe_paths) es
             |  Ast_c.Iteration  (Ast_c.While (exp, st)) ->
		 let (norm_exp,opp_exp) = opp_exp_st ((Ast_c.ExprStatement (Some exp)),[]) in
                 let exe_paths1 = generate_execution_paths_new_main exe_paths (norm_exp::(create_compound_st st)) in
                 let exe_paths2 = generate_execution_paths_new_main exe_paths [opp_exp] in
                 generate_execution_paths_new_main (List.hd (List.map (function (s)-> (s::exe_paths1)) exe_paths2)) es
             |  _-> generate_execution_paths_new_main exe_paths es



        
let rec generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp = function
    []-> (exe_paths, df_eq, ref_list, exp)
  | h::t->
      let start_line = find_startline_no (create_compound_st h)  in
      let end_line   = find_endline_no (create_compound_st h) in 
      if(line_no>=start_line && line_no <=end_line) || line_no >= end_line then (    
          match Ast_c.unwrap h with
             |  Ast_c.Labeled (Ast_c.Label (name, st)) -> 
  	         let st = create_compound_st st  in
                 let start_line = find_startline_no st in
                 let end_line = find_endline_no st in
                 if line_no >= start_line && line_no <= end_line then 
                    let (new_exe_paths, new_df_eq, new_ref_list, new_exp) =
                      generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp st in
                      generate_execution_paths line_no new_exe_paths  new_df_eq new_ref_list lbl_list new_exp t             

                 else if(not(has_return_all st)) && (not(has_goto st)) then
  	            let (new_exe_paths, new_df_eq, new_ref_list, new_exp ) =  
  		      generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp st in
  	              generate_execution_paths line_no new_exe_paths  new_df_eq new_ref_list lbl_list new_exp t 
                 else
  		      generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t
	       
             |  Ast_c.Labeled (Ast_c.Case  (e, st)) ->

                 let st = create_compound_st st  in 
 	         let start_line = find_startline_no st in
                 let end_line = find_endline_no st in
                 if line_no >= start_line && line_no <= end_line then
                    let (new_exe_paths, new_df_eq, new_ref_list, new_exp) =
                      generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp st in
                      generate_execution_paths line_no new_exe_paths  new_df_eq new_ref_list lbl_list new_exp t

 	         else if(not(has_return_all st)) && (not(has_goto st)) then 
 		    let (new_exe_paths, new_df_eq, new_ref_list, new_exp ) = 
 		      generate_execution_paths line_no [] df_eq ref_list lbl_list exp st in	       
	              generate_execution_paths line_no (exe_paths@new_exe_paths) new_df_eq new_ref_list lbl_list new_exp t
                 else generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t

           |  Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->
              let st = create_compound_st st  in
               let start_line = find_startline_no st in
               let end_line = find_endline_no st in
               if line_no >= start_line && line_no <= end_line then
                  let (new_exe_paths, new_df_eq, new_ref_list, new_exp) =
                    generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp st in
                    generate_execution_paths line_no new_exe_paths  new_df_eq new_ref_list lbl_list new_exp t

              else if(not(has_return_all st))  && (not(has_goto st))  then
                  let (new_exe_paths, new_df_eq, new_ref_list, new_exp ) = 
		    generate_execution_paths line_no [] df_eq ref_list lbl_list exp st in
                  generate_execution_paths line_no (exe_paths@new_exe_paths) new_df_eq new_ref_list lbl_list new_exp t
              else generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t

           |  Ast_c.Labeled (Ast_c.Default st) -> 
                let st = create_compound_st st  in
                 let start_line = find_startline_no st in
                 let end_line = find_endline_no st in
                 if line_no >= start_line && line_no <= end_line then
                    let (new_exe_paths, new_df_eq, new_ref_list, new_exp) =
                      generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp st in
                      generate_execution_paths line_no new_exe_paths  new_df_eq new_ref_list lbl_list new_exp t

                else if(not(has_return_all st)) && (not(has_goto st))  then
                    let (new_exe_paths, new_df_eq, new_ref_list, new_exp ) = 
  		    generate_execution_paths line_no [] df_eq ref_list lbl_list exp st in
                    generate_execution_paths line_no (exe_paths@new_exe_paths) new_df_eq new_ref_list lbl_list new_exp t
                else generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t 

           |  Ast_c.Compound statxs -> 
	       let st = stmtEle_remove statxs in
               let start_line = find_startline_no st in
               let end_line = find_endline_no st in
	       if(line_no>= start_line && line_no <= end_line) then 
                 let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
		   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp st in
                   generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
	       else if(not(has_return_all st)) && (not(has_goto st)) then
		 let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
		   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp st in
                   generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
               else generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t

           |    Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.RecordAccess (e, name)), typ1), ii1), op,
                                                                (((Ast_c.Ident (ident)), typ2), ii2) )), typ), ii)) ->
                 let exe_paths = if (List.length exe_paths) = 0 then
                                    (exe_paths@[[h]])
                                 else
                                    (append_exe_path_inner [h] exe_paths)
                                 in
                 let ref_list = insert_exp_into_ref_list e (((Ast_c.RecordAccess (e, name)), typ1), ii1)
		                (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in
            	 generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t

           |   Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.Ident (ident)), typ2), ii2) , op, 
							      (((Ast_c.RecordAccess (e, name)), typ1), ii1))), typ), ii)) ->
                 let exe_paths = if (List.length exe_paths) = 0 then
                                    (exe_paths@[[h]])
                                 else
                                    (append_exe_path_inner [h] exe_paths)
                                 in
                 let ref_list = insert_exp_into_ref_list e (((Ast_c.RecordAccess (e, name)), typ1), ii1)
                                (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in
                 generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t

           |   Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.RecordPtAccess (e, name)), typ1), ii1), op,
                                                               (((Ast_c.Ident (ident)), typ2), ii2) )), typ), ii)) ->
                 let exe_paths = if (List.length exe_paths) = 0 then
                                    (exe_paths@[[h]])
                                 else
                                    (append_exe_path_inner [h] exe_paths)
                                 in
                 let ref_list = insert_exp_into_ref_list e  (((Ast_c.RecordAccess (e, name)), typ1), ii1)
		     (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in
		 generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t
  
           |  Ast_c.ExprStatement (Some (((Ast_c.Assignment ((((Ast_c.Ident (ident)), typ2), ii2), op, 
							     (((Ast_c.RecordAccess (e, name)), typ1), ii1))), typ), ii) ) ->

                 let exe_paths = if (List.length exe_paths) = 0 then
                                    (exe_paths@[[h]])
                                 else
                                    (append_exe_path_inner [h] exe_paths)
                                 in
                 let ref_list = insert_exp_into_ref_list e  (((Ast_c.RecordAccess (e, name)), typ1), ii1)
                     (((Ast_c.Ident (ident)), typ2), ii2) [] ref_list in
                 generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t


           |  Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->      
	       
                 let exe_paths = if (List.length exe_paths) = 0 then
                                    (exe_paths@[[h]])
                                 else
                                    (append_exe_path_inner [h] exe_paths)
                                 in
                 let df_eq = exp_defined_in_df_eq_st h (Some h) [] df_eq  in

		 
                 generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t   
           | Ast_c.ExprStatement (Some (((Ast_c.Cast    (c, (((Ast_c.FunCall  (e, es)), typ1), ii1))), typ), ii))->
                 let exe_paths = if (List.length exe_paths) = 0 then
                                    (exe_paths@[[(Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ1), ii1)),[])]])
                                 else
                                    (append_exe_path_inner 
				       [(Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ1), ii1)),[])] exe_paths)
                                 in
                 generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t
                   
           | Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
                 let exe_paths = if (List.length exe_paths) = 0 then
                                    (exe_paths@[[h]])
                                 else
                                    (append_exe_path_inner [h] exe_paths)
                                 in
                 let arglist = remove_option (make_argslist [] es) in                 
                 let ref_list = if(List.length arglist>1) then
		                   insert_exp_into_ref_list_multi (List.hd arglist) (List.tl arglist) [] ref_list arglist 
		                else ref_list
                                in
                 generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t
             
           | Ast_c.ExprStatement (Some e) ->  
                   let exe_paths = if (List.length exe_paths) = 0 then  
                                      (exe_paths@[[h]])  
                                   else  
                                      (append_exe_path_inner [h] exe_paths)  
                                   in  
                   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t   

           |  Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->  

               let start_line_st1 = find_startline_no (create_compound_st st1) in
               let end_line_st1   = find_endline_no (create_compound_st st1) in
               let expression_stmt = ((Ast_c.ExprStatement (Some e)),[]) in
	       let st1 = create_compound_st st1 in
	       let st2 = if(has_multiple_st (create_compound_st st2)) then create_compound_st st2 else [] in
               let (norm_exp, opp_exp) = opp_exp_st ((Ast_c.ExprStatement (Some e)),[]) in

               let exe_paths1 =   if (List.length exe_paths) = 0 then
                                   (exe_paths@[[norm_exp]])
                                  else
                                   (append_exe_path_inner [norm_exp] exe_paths)
                                  in
               let exe_paths2 =   if (List.length exe_paths) = 0 then
                                   (exe_paths@[[opp_exp]])
                                   else
                                   (append_exe_path_inner [opp_exp] exe_paths)
                                  in
                 let df_eq1 = exp_defined_in_df_eq_st norm_exp (Some norm_exp) [] df_eq  in
                 let df_eq2 = exp_defined_in_df_eq_st opp_exp  (Some opp_exp)  [] df_eq  in

	       if (line_no = start_line_st1) then
         	 let rcol = find_rcol st1 in
 		 let lcol = find_lcol st1 in
                    (exe_paths1, df_eq1, ref_list, (Some e))
(*                   generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list st1*)

	       else if(line_no>= start_line && line_no <= end_line) then
		 let start_line_st1 = find_startline_no st1 in
		 let end_line_st1   = find_endline_no st1 in
		 if(line_no>= start_line_st1  && line_no <= end_line_st1) then
		    let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
		      generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list exp st1 in
		      generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
		 else
		    let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
		      generate_execution_paths line_no exe_paths2 df_eq2 ref_list lbl_list exp st2 in
		      generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
	       else if (not(has_return_all st1)) && (not(has_return_all st2)) && 
		       (not(has_goto st2)) && (not(has_goto st1)) && (List.length st2 >0) then 

		 let (new_exe_paths1, new_df_eq1, ref_list1, new_exp1) = 
		   generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list exp st1 in
		 let (new_exe_paths2, new_df_eq2, ref_list2, new_exp2 ) = 
		   generate_execution_paths line_no exe_paths2 df_eq2 ref_list lbl_list exp st2 in
		   let new_ref_list = intersect_ref_list ref_list1 [] ref_list2 in
		   let new_df_eq = intersect_df_eq new_df_eq1 [] new_df_eq2 in
    		   generate_execution_paths line_no (new_exe_paths1@new_exe_paths2)  new_df_eq new_ref_list lbl_list exp t

               else if (not(has_return_all st1)) && (not(has_return_all st2)) &&
                       (not(has_goto st2)) && (not(has_goto st1)) && (List.length st2 = 0) then

                 let (new_exe_paths1, new_df_eq1, ref_list1, new_exp1) =
                   generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list exp st1 in
                 let (new_exe_paths2, new_df_eq2, ref_list2, new_exp2) =
                   generate_execution_paths line_no exe_paths2 df_eq2 ref_list lbl_list exp st2  in
                 let new_ref_list = intersect_ref_list ref_list1 [] ref_list2 in
(*                 let new_df_eq = intersect_df_eq new_df_eq1 [] new_df_eq2 in*)
                   generate_execution_paths line_no new_exe_paths1  new_df_eq1 new_ref_list lbl_list exp t


	       else if (not(has_return_all st1)) && (not(has_goto st1)) then
                 let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
		   generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list exp st1 in
		    generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
	       else if (not(has_return_all st2)) && (not(has_goto  st2)) then
                 let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
		   generate_execution_paths line_no exe_paths2 df_eq2 ref_list lbl_list exp st2 in
		     generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
	       else generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t   
		   
             |  Ast_c.Selection  (Ast_c.Switch (e, st)) -> 
                let exe_paths =   if (List.length exe_paths) = 0 then 
                                     (exe_paths@[[((Ast_c.ExprStatement (Some e)),[])]])
                                  else
                                     (append_exe_path_inner [((Ast_c.ExprStatement (Some e)),[])] exe_paths)
                                  in
                let st = create_compound_st st  in
                let start_line = find_startline_no st in
                let end_line = find_endline_no st in

 	       if(line_no>= start_line && line_no <= end_line) then
                  let case_stmts = find_case_stmts line_no []  st in
                  let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
 		   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp case_stmts  in
                  generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
                else if (not(has_return_all st)) && (not(has_goto st)) then
 		 let switch_st_compound = create_compound_st_for_switch e [] [] st in
 		 let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
 		   generate_execution_paths line_no [] df_eq ref_list lbl_list exp switch_st_compound in 
                  let switch_exe = if(List.length new_exe_paths =0) then exe_paths 
 		                  else exe_path_for_switch [] new_exe_paths exe_paths in
                  generate_execution_paths line_no switch_exe df_eq ref_list lbl_list new_exp t
 	       else generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t
                 


            |  Ast_c.Iteration  (Ast_c.While (e, st)) ->   
                let (norm_exp,opp_exp) = opp_exp_st ((Ast_c.ExprStatement (Some e)),[]) in
                let exe_paths1 = if (List.length exe_paths) = 0 then
                                    (exe_paths@[[norm_exp]])
                                 else
                                     (append_exe_path_inner [norm_exp] exe_paths)
                                  in
  	       let exe_paths2 = if (List.length exe_paths) = 0 then
                                     (exe_paths@[[opp_exp]])
                                 else
                                     (append_exe_path_inner [opp_exp] exe_paths)
                                 in
                  let st = create_compound_st st  in
                  let start_line = find_startline_no st in
                  let end_line = find_endline_no st in

                  let df_eq1 = exp_defined_in_df_eq_st norm_exp (Some norm_exp) [] df_eq  in
                  let df_eq2 = exp_defined_in_df_eq_st opp_exp  (Some opp_exp)  [] df_eq  in

                  if(line_no>= start_line && line_no <= end_line) then
                    let (new_exe_paths, new_df_eq, new_ref_list,new_exp) = 
   		   generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list exp st in
                      generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t  
  	       else if(not(has_return_all st)) && (not(has_goto st))then
                    let (new_exe_paths1, df_eq1, ref_list1, new_exp1) = 
   		   generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list exp st in

                    let (new_exe_paths2, df_eq2, ref_list2, new_exp2) =
                      generate_execution_paths line_no exe_paths2 df_eq2 ref_list lbl_list exp [] in 
                    let new_df_eq = intersect_df_eq df_eq1 [] df_eq2 in  
                    generate_execution_paths line_no (new_exe_paths1@new_exe_paths2) new_df_eq ref_list lbl_list exp t 
                  else generate_execution_paths line_no exe_paths2 df_eq2 ref_list lbl_list exp t   

           |  Ast_c.Iteration  (Ast_c.DoWhile (st, e)) -> 
               let (norm_exp,opp_exp) = opp_exp_st ((Ast_c.ExprStatement (Some e)),[]) in
               let exe_paths1 = if (List.length exe_paths) = 0 then
                                   (exe_paths@[[norm_exp]])
                                else
                                   (append_exe_path_inner [norm_exp] exe_paths)
                                in
               let exe_paths2 = if (List.length exe_paths) = 0 then
                                   (exe_paths@[[opp_exp]])
                                else
                                   (append_exe_path_inner [opp_exp] exe_paths)
                                in
               let st = create_compound_st st  in
               let start_line = find_startline_no st in
               let end_line = find_endline_no st in

               let df_eq1 = exp_defined_in_df_eq_st norm_exp (Some norm_exp) [] df_eq  in
               let df_eq2 = exp_defined_in_df_eq_st opp_exp  (Some opp_exp)  [] df_eq  in

               if(line_no>= start_line && line_no <= end_line) then
                 let (new_exe_paths, new_df_eq, new_ref_list, new_exp) =
                   generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list exp st in
                   generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
               else if(not(has_return_all st)) && (not(has_goto st)) then
                 let (new_exe_paths1, df_eq1, ref_list1, new_exp1) =
                   generate_execution_paths line_no exe_paths1 df_eq1 ref_list lbl_list exp st in

                 let (new_exe_paths2, df_eq2, ref_list2, new_exp2) =
                   generate_execution_paths line_no exe_paths2 df_eq2 ref_list lbl_list exp [] in
                 let new_df_eq = intersect_df_eq df_eq1 [] df_eq2 in
                 generate_execution_paths line_no (new_exe_paths1@new_exe_paths2) new_df_eq ref_list lbl_list exp t
               else generate_execution_paths line_no exe_paths2 df_eq2 ref_list lbl_list exp t

           |  Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) -> 
               let st = create_compound_st st  in
               if(line_no>= start_line && line_no <= end_line) then
                 let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
		   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp st in
                 generate_execution_paths line_no new_exe_paths new_df_eq new_ref_list lbl_list new_exp t
               else if(not(has_return_all st)) && (not(has_goto st)) then
                 let (new_exe_paths1, df_eq1, ref_list1, new_exp1) =
                   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp st in

                 let (new_exe_paths2, df_eq2, ref_list2, new_exp2) =
                   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp [] in
                 let new_df_eq = intersect_df_eq df_eq1 [] df_eq2 in
                 generate_execution_paths line_no (new_exe_paths1@new_exe_paths2) new_df_eq ref_list lbl_list exp t
               else generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp t

           |  Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> 
               let st = create_compound_st st  in
               if(line_no>= start_line && line_no <= end_line) then
                 let (new_exe_paths, new_df_eq, new_ref_list, new_exp) = 
		   generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp st in
                 generate_execution_paths line_no new_exe_paths  new_df_eq new_ref_list lbl_list new_exp t
               else if(not(has_return_all st)) && (not(has_goto st)) then
                 let (new_exe_paths1, df_eq1, ref_list1, new_exp1) =
                   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp st in

                 let (new_exe_paths2, df_eq2, ref_list2, new_exp2) =
                   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp [] in
                 let new_df_eq = intersect_df_eq df_eq1 [] df_eq2 in
                 generate_execution_paths line_no (new_exe_paths1@new_exe_paths2) new_df_eq ref_list lbl_list exp t

               else generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp t

             |  Ast_c.Jump (Ast_c.Goto name) ->  (exe_paths,df_eq,ref_list, exp)
(*	       let goto_code = gather_goto_code lbl_list [] [] [h] in  *)
(*	       let (new_exe_paths, df_eq, ref_list) = *)
(* 		 generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list goto_code  in*)
(*                   generate_execution_paths line_no new_exe_paths  df_eq ref_list lbl_list t *)
             |  Ast_c.Jump (Ast_c.Return) -> (exe_paths,df_eq,ref_list, exp)
             |  Ast_c.Jump (Ast_c.ReturnExpr e) -> (exe_paths, df_eq, ref_list, exp)
             |  Ast_c.Jump (Ast_c.GotoComputed e) -> (exe_paths,df_eq, ref_list, exp)
             |  Ast_c.Decl decl ->(*Printf.printf "\n\n\n%s\n\n" (Dumper.dump decl);*)
                   let exe_paths = if (List.length exe_paths) = 0 then
                                      (exe_paths@[[h]])
                                   else
                                      (append_exe_path_inner [h] exe_paths)
                                   in
                   generate_execution_paths line_no exe_paths df_eq ref_list lbl_list exp t
		                  
           | _->  generate_execution_paths line_no exe_paths  df_eq ref_list lbl_list exp t
(*                 if(List.length exe_paths) = 0 then *)
(*                   generate_execution_paths line_no (exe_paths@[[h]])  df_eq ref_list lbl_list t*)
(*                 else *)
(*                   generate_execution_paths line_no (append_exe_path_inner [h] exe_paths) df_eq ref_list lbl_list t*)
 ) else (exe_paths, df_eq, ref_list, exp )



let rec remove_st new_flist st =function
  []-> new_flist
  |  h::t-> if(compare_stmts st h) then remove_st new_flist st t
            else remove_st (new_flist@[h]) st t



let rec is_arg_exists_is_df_eq_but_null arg = function
     []-> false
  |    h::t-> ( match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii)) ->
                     if(compare_exp arg e1) then true
                     else is_arg_exists_is_df_eq_but_null arg t
    |   _ ->  is_arg_exists_is_df_eq_but_null arg t
            )




let rec is_same_arg_diff_name df_eq arglist1 = function
  []-> false
  |  h::t -> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> 
		let arglist2 = refine_arglist df_eq (remove_option (make_argslist [] args)) in
	                                               if (clean_exp_list (remove_option arglist1)) =
                                                                    (clean_exp_list arglist2) 
						       && (not(has_string h)) then ((*print_string("\ncccc\n");*)true)
                                                       else if (clean_exp_list (remove_integer (remove_option arglist1))) =
                                                                    (clean_exp_list (remove_integer arglist2)) && 
						       (List.length (remove_integer (remove_option arglist1)) != 0) 
						       && (not(has_string h)) then ((*print_string("\nppp\n");*)true)
                                                       else ((*print_string("\nVVVVVVVVV\n");*)
							 is_same_arg_diff_name df_eq arglist1 t )
    |  _ -> is_same_arg_diff_name df_eq arglist1 t

let rec is_same_arg_diff_name_new df_eq arglist1 = function
  []-> false
  |   h::t -> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> let arglist2 = (remove_option (make_argslist [] args)) in
                                                       if (clean_exp_list (remove_option arglist1)) =
                                                                    (clean_exp_list arglist2)
                                                       && (not(has_string h)) then ((*print_string("\ncccc\n");*)true)
                                                       else if (clean_exp_list (remove_integer (remove_option arglist1))) =
                                                                    (clean_exp_list (remove_integer arglist2)) &&
                                                       (List.length (remove_integer (remove_option arglist1)) != 0)
                                                       && (not(has_string h)) then ((*print_string("\nppp\n");*)true)
                                                       else ((*print_string("\nVVVVVVVVV\n");*)
                                                             is_same_arg_diff_name_new df_eq arglist1 t)
    |   _ -> is_same_arg_diff_name_new df_eq arglist1 t



let rec find_common_path  common_path exe_path1  exe_path2  =
  match (exe_path1, exe_path2) with
   ([],[])-> common_path
  |  ([] ,_) -> common_path
  |  (_, [])-> common_path
  |  (h::t, h1::t1) -> if (compare_stmts h h1) then find_common_path  (common_path@[h]) t t1
                     else common_path

let rec find_farest_brnch_with_st paths st line_no1 st = function
  []->(*print_string("\nVVVVVVVVV\n");*)paths
  |  (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq )::t -> if (line_no > line_no1) then paths
                                                        else if (st_exists_in_list st lbl_code) then  exe_paths
                                                        else find_farest_brnch_with_st paths st line_no1 st t


let rec find_nearest_brnch_with_st_new paths st line_no1 st = function
  []->(*print_string("\nVVVVVVVVV1\n");*)paths
  |   (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list  )::t -> 
                                                        if (line_no > line_no1) then paths
                                                        else if (st_exists_in_list st lbl_code) then  
							  (if(List.length exe_paths>0) then (List.hd exe_paths) else [])
                                                        else find_nearest_brnch_with_st_new paths st line_no1 st t


let rec find_farest_brnch_with_st_new paths st line_no1 st  = function
  []->(*print_string("\nVVVVVVVVV2\n"); *)paths
  |   (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list  )::t ->
                                                         if (line_no < line_no1) && (st_exists_in_list st lbl_code) then 
							   (if(List.length exe_paths>0) then (List.hd exe_paths) else []) 
                                                        else find_farest_brnch_with_st_new paths st line_no1 st t


let rec find_nearest_brnch_with_st paths st line_no1 st  = function
  []-> paths
  |  (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq )::t ->
                                                         if (line_no < line_no1) && (st_exists_in_list st lbl_code) then exe_paths
                                                        else find_nearest_brnch_with_st paths st line_no1 st t



let is_same_arg_diff_name_in_path_new_near df_eq arglist if_list exe_paths st line_no =

(*print_string("\nVVVVVVVVV444444\n");*)
      let nearest_branch_paths = find_nearest_brnch_with_st_new [] st line_no st  if_list in

                                        (*       print_string("\nnearest_branch_paths\n");*)
(*                                               print_string(string_of_int(List.length nearest_branch_paths));*)
(*                                               Printf.printf "\n\n\n%s\n\n" (Dumper.dump nearest_branch_paths );*)

      let single_path_nearest =  nearest_branch_paths in
      let single_path_own =   (if(List.length exe_paths>0) then (List.hd exe_paths) else [])  in
      let common_path = find_common_path  [] single_path_nearest single_path_own in
      let subtract_path = find_subtract_path common_path single_path_own in
      if (is_same_arg_diff_name df_eq arglist subtract_path) then true
      else false



let is_same_arg_diff_name_in_path_new df_eq arglist if_list exe_paths st line_no =
      let farest_branch_paths = find_farest_brnch_with_st_new [] st line_no st  if_list in
      let single_path_farest =  farest_branch_paths in
      let single_path_own =   (if(List.length exe_paths>0) then (List.hd exe_paths) else [])  in
      let common_path = find_common_path  [] single_path_farest single_path_own in
      let subtract_path = find_subtract_path common_path single_path_own in
      if (is_same_arg_diff_name df_eq arglist subtract_path) then true
      else false


let is_same_arg_diff_name_in_path df_eq arglist if_list exe_paths st line_no =
      let nearest_branch_paths = find_nearest_brnch_with_st [] st line_no st  if_list in
(*      let single_path_nearest = select_one_path [] [] nearest_branch_paths in*)
      let single_path_nearest =  nearest_branch_paths in
(*      let single_path_own = select_one_path [] [] exe_paths in*)
      let single_path_own =  exe_paths in
      let common_path = find_common_path  [] single_path_nearest single_path_own in
      let subtract_path = find_subtract_path common_path single_path_own in
      if (is_same_arg_diff_name df_eq arglist subtract_path) then true
      else false




let rec prepare_flist df_eq lcol rcol line_no iifunc1 new_flist exe_path flist line_no if_list  = function
  []->new_flist
| h::t->  ( match Ast_c.unwrap h with
           Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
           let arglist1 =  make_argslist [] es  in
           if(st_exists_in_list h exe_path)
(*|| ((List.length arglist1 !=0 )  && (is_same_arg_diff_name_in_path_new_near df_eq arglist1 if_list [exe_path] h line_no))*) then 
             ( example_mark5:= 1;
	      (* print_string(string_of_int(find_startline_no [h]));*)
(*	       print_string("\n");*)
	       if (is_suffix (List.rev (new_flist@[h])) (List.rev flist) ) then 
                   ( prepare_flist df_eq lcol rcol line_no iifunc1 new_flist exe_path (remove_st [] h flist) line_no if_list t )
               else 
               (    
                let start_line = line_no  in   
                let filename = Ast_c.file_of_info iifunc1 in    
                let message = "Release Unorderly" in    
                
(*                     print_string("* TODO [[view:");  *)
(*                     print_string(filename);   *)
(*                     print_string("::face=ovl-face1::linb="); *)  
 (*                    print_string(string_of_int(line_no));  *)
(*                     print_string("::colb=");   *)
(*                     print_string(string_of_int(lcol));    *)
(*                     print_string("::cole=");   *)
(*                     print_string(string_of_int(rcol));   *)
(*                     print_string("][");    *)
(*                     print_string(message);  *) 
(*                     print_string(" ");    *)
(*                     print_string(filename);  *)  
(*                     print_string("::");   *)
(*                     print_string(string_of_int(line_no));     *)
(*                     print_string("]]");    *)
(*                     print_string("\n");      *)
                    prepare_flist df_eq lcol rcol line_no iifunc1 new_flist exe_path flist line_no if_list t    
                )   
               ) 
         else (prepare_flist df_eq lcol rcol line_no iifunc1 (new_flist@[h]) exe_path flist line_no if_list t)
       | _ -> prepare_flist df_eq lcol rcol line_no iifunc1 (new_flist@[h]) exe_path flist line_no if_list t 
      )




let rec is_flists_equal_inner ele = function
  []-> true
| h::t -> if(compare_stmts_list ele h) then  is_flists_equal_inner ele t
          else false

let rec  is_flists_equal = function
  []-> true
| h::t-> if (is_flists_equal_inner h t) then true
         else false

let rec find_single_flist = function
  []-> []
| h::t -> h



let rec create_idlist idlist = function
  []-> idlist 
| h::t -> (
             match Ast_c.unwrap h with
(*                Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->let arglist = remove_option (make_argslist [] es) in*)
  (*                                                                                 create_idlist idlist t*)

             |    Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ), ii))), typ1), ii1))-> (*let arglist = remove_option (make_argslist [] es) in*)
                                                                                                                               create_idlist (idlist@[e1]) t


             |    Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, ((( Ast_c.Assignment (e2, op2, (((Ast_c.FunCall  (e, es)), typ), ii))), typ1), ii1))), typ2), ii2))->
                                                                                     create_idlist (idlist@[e1]@[e2]) t


            | Ast_c.ExprStatement (Some (((Ast_c.Binary   ((((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ1), ii1))), typ2), ii2), op1, e2)), typ3), ii3)) ->
(*                                                                                              let arglist = remove_option (make_argslist [] es) in*) 
                                                                                                  create_idlist (idlist@[e1]) t
 
             |  _ -> create_idlist idlist t
          )


let rec is_arg_used_with_another_function arglist1 = function
  []-> false
  |  h::t -> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))->
		let arglist2 = remove_option (make_id_argslist [] args) in 
                let arglist1_new = remove_option arglist1 in
                if(any_exp_exists_list arglist2 arglist1_new) then true
                else is_arg_used_with_another_function arglist1 t
          
             | _ -> is_arg_used_with_another_function arglist1 t                                                          


let rec find_same_arg_func_diff_name arglist1 = function
  []-> None
  |  h::t -> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> let arglist2 = make_argslist [] args in
                                                       if (clean_exp_list (remove_option arglist1)) =
                                                                    (clean_exp_list (remove_option arglist2)) then Some h
                                                       else find_same_arg_func_diff_name arglist1 t
    |  _ -> find_same_arg_func_diff_name arglist1 t


let rec find_new_stmts lbl_code new_stmts flist = function
  []-> new_stmts
| h::t ->( (*match Ast_c.unwrap h with*)
(*              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> let arglist = make_argslist [] args in*)

                                                                         if(st_exists_in_list h flist) then find_new_stmts lbl_code new_stmts flist t
                   (*                                                      else if (is_same_arg_diff_name arglist lbl_code) then find_new_stmts lbl_code new_stmts flist t*)
                                                                         else find_new_stmts lbl_code  (new_stmts@[h]) flist t
(*            | _ -> find_new_stmts lbl_code new_stmts flist t*)
          )


let rec select_one_path exe_path idlist = function
  []-> exe_path
| h::t -> let idlist1 = create_idlist [] h in
               if(List.length idlist1) > (List.length idlist) then select_one_path h idlist1 t
               else select_one_path exe_path idlist t 



let rec any_arg_used_for_allocated exe_paths st line_no if_list arglist = 

          let farest_branch_paths = find_farest_brnch_with_st [] st line_no st if_list in
(*          let single_path_farest = select_one_path [] [] farest_branch_paths in*)
(*          let single_path_own = select_one_path [] [] exe_paths in*)
          let single_path_farest =farest_branch_paths in
          let single_path_own = exe_paths in 
          
          let common_path = find_common_path  [] single_path_farest single_path_own in 
          let idlist = create_idlist [] common_path in
         (*  (match st with *)
(*            ((Ast_c.ExprStatement (Some e)),ii) ->  *)
          let rec any_arg_used_for_allocated_inner = function
              []-> if(is_same_arg_diff_name [] (add_option arglist) common_path) then true else false
            | h::t->
                          if (exp_exists_list h idlist) then true
                          else any_arg_used_for_allocated_inner  t
           in any_arg_used_for_allocated_inner arglist
         (*  | _ -> false  *)
         (*  ) *)



let rec all_arg_used_for_allocated exe_paths st line_no if_list arglist =

          let farest_branch_paths = find_farest_brnch_with_st [] st line_no st if_list in
(*          let single_path_farest = select_one_path [] [] farest_branch_paths in*)
(*          let single_path_own = select_one_path [] [] exe_paths in*)
          let single_path_farest =farest_branch_paths in
          let single_path_own = exe_paths in

          let common_path = find_common_path  [] single_path_farest single_path_own in
          let idlist = create_idlist [] common_path in
         (*  (match st with *)
(*            ((Ast_c.ExprStatement (Some e)),ii) ->  *)
          let rec all_arg_used_for_allocated_inner = function
              []->  ((*print_string("\nTrue\n");*)true) 
            | h::t->
                          if (exp_exists_list h idlist) then ((*print_string("\nTrue\n");*)all_arg_used_for_allocated_inner  t)
                          else ((*print_string("\nFalse\n");*)false)
           in all_arg_used_for_allocated_inner arglist








let rec any_arg_used_in_list idlist = function
  []-> false
  | h::t-> if (exp_exists_list h idlist) then true
           else any_arg_used_in_list idlist t


let rec refine_flist usefull_st  = function
    []->[]
  | h::t-> (match Ast_c.unwrap h with
                     Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> let arglist = make_argslist [] args in
                                 if (is_same_arg_diff_name [] arglist usefull_st) then refine_flist usefull_st t
                                 else refine_flist (usefull_st@[h]) t
                  |  _-> refine_flist usefull_st t
           )

let rec refine_allocated_new_stmts flist  = function
    []->[]
  |  h::t-> (match Ast_c.unwrap h with
                     Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> let arglist = make_argslist [] args in
                                 if (is_same_arg_diff_name [] arglist flist) then refine_allocated_new_stmts flist t
                                 else h::(refine_allocated_new_stmts flist t)
                 |   _-> refine_allocated_new_stmts flist t
           )




let  find_allocated_new_stmts line_no exe_paths new_stmts if_list =
    (*let single_path_own = select_one_path [] [] exe_paths in*)
    let single_path_own = exe_paths in
    let idlist = create_idlist [] single_path_own in
    
    let rec find_allocated_new_stmts_inner = function 
      []->[]    
      |	h::t-> (match Ast_c.unwrap h  with    
                  Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> 
                                           let arglist = make_argslist [] args  in    
                                                 if(any_arg_used_for_allocated exe_paths h line_no if_list (remove_option arglist)) then 
							   h::(find_allocated_new_stmts_inner t)    
						 else find_allocated_new_stmts_inner t     
                 |  _ -> find_allocated_new_stmts_inner t    
                )   
      in find_allocated_new_stmts_inner new_stmts    



let rec find_return_st = function
  []-> None
| h::t -> match Ast_c.unwrap h with
            Ast_c.Jump (Ast_c.ReturnExpr e) -> Some e
          | _ -> find_return_st t


let rec find_return_st_new = function
  []-> None
  |  h::t -> match Ast_c.unwrap h with
            Ast_c.Jump (Ast_c.ReturnExpr e) -> Some h
    |  _ -> find_return_st_new t



let rec any_arg_used_in_if_exp_list arglist = function
  []-> false
  | h::t-> if (any_arg_used_in_if_exp h arglist) then true
           else any_arg_used_in_if_exp_list arglist t


let verify_missing_stmt if_exp exe_paths line_no if_list h lbl_code =
  match Ast_c.unwrap h  with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> let arglist = (make_argslist [] args)  in
                                                                           let id_arglist = (make_id_argslist [] args)  in

                   (*                            print_string("\narg_list\n");*)
(*                                               print_string(string_of_int(List.length arglist));*)

  (*                                             print_string("\nid_arg_list\n");*)
(*                                               print_string(string_of_int(List.length id_arglist));*)
  (*                                             Printf.printf "\n\n\n%s\n\n" (Dumper.dump id_arglist );*)

         if (List.length arglist) = 0 then ((*print_string("\nZZZZZKKKK\n");*)true)
         else if (is_same_arg_diff_name [] arglist (skip_return lbl_code) ) then ((*print_string("\nZZZZZ\n");*)false)
         else if (any_arg_used_in_return (find_return_st lbl_code) (remove_option arglist)) then ((*print_string("\nZZZZZGG\n");*)false)
        (* else if (any_arg_used_in_if_exp if_exp (remove_option arglist)) then false *)
         else if (is_same_arg_diff_name_in_path [] arglist if_list exe_paths h line_no) then ((*print_string("\nZZZZZ4\n");*)false)
         else if (is_arg_used_with_another_function id_arglist (skip_return lbl_code) ) then false
         else if (any_arg_used_for_allocated exe_paths h line_no if_list (remove_option arglist)) 
                   && (all_arg_used_for_allocated exe_paths h line_no if_list (remove_option id_arglist)) 
                   then ((*print_string("\nZZZZZ5\n");*)true)
         else if ( not (any_arg_used_for_allocated exe_paths h line_no if_list (remove_option arglist) ))
                   || ( not (all_arg_used_for_allocated exe_paths h line_no if_list (remove_option id_arglist)))
                    then ((*print_string("\nZZZZZ6\n");*)false)
(*         if(is_any_arg_used_for_exp if_exp (make_argslist [] args)) then (print_string("\nIf expression false\n"); false)*)
(*         else if (is_any_arg_used_for_return_exp return_exp_bug (make_argslist [] args)) then false*)
(*         else if (is_diff_name_same_arg (make_argslist [] args) lbl_code ) then false*)
(*         else if (is_any_arg_in_idlist idlist (make_argslist [] args))==true then true*)
(*         else if (is_any_arg_in_idlist idlist (make_argslist [] args))==false then false*)
         else ((*print_string("\nZZZZZ7\n");*)true)

  |_ -> false



let rec find_missing_stmts bug_cause lcol rcol if_exp exe_paths if_list line_no iifunc1 missing_stmts lbl_code = function
     []-> (missing_stmts,bug_cause)
   | h::t -> if(st_exists_in_list h lbl_code) then find_missing_stmts bug_cause lcol rcol if_exp exe_paths if_list line_no iifunc1 missing_stmts lbl_code t
             else (
                      if (verify_missing_stmt if_exp exe_paths line_no if_list h lbl_code) then( 

                let start_line = line_no  in
                let filename = Ast_c.file_of_info iifunc1 in
                let message = "Missing BUG" in
		let bug_cause = 
                (match bug_cause with
                    None -> print_string("* TODO [[view:"); None
                  | Some a -> if (compare_stmts a h) then (print_string("** TODO [["); (Some a))
			      else (print_string("* TODO [[view:"); (Some h))
                ) in
                     print_string(filename);
                     print_string("::face=ovl-face1::linb=");
                     print_string(string_of_int(line_no));
                     print_string("::colb=");
                     print_string(string_of_int(lcol));
                     print_string("::cole=");
                     print_string(string_of_int(rcol));
                     print_string("][");
                     print_string(message);
                     print_string(" ");
                     print_string(filename);
                     print_string("::");
                     print_string(string_of_int(line_no));
                     print_string("]]");
                     print_string("\n");                                       
                                       find_missing_stmts bug_cause lcol rcol  if_exp exe_paths if_list line_no iifunc1 (missing_stmts@[h]) lbl_code t
                                       )
                      else  ((*print_string("\nKKKKPPPPPP\n");*)
			find_missing_stmts bug_cause lcol rcol if_exp exe_paths if_list line_no iifunc1 missing_stmts lbl_code t)

                  )


let rec make_order_branch_inner lbl_code unique_flist allocated_new_stmts = function
  []-> []
  |  h::t-> (
             
                              if(st_exists_in_list h unique_flist) || (st_exists_in_list h allocated_new_stmts)  
                                    then  h::(make_order_branch_inner lbl_code unique_flist allocated_new_stmts t)
                              else make_order_branch_inner lbl_code unique_flist allocated_new_stmts  t
             
            )


let  make_order_branch unique_flist allocated_new_stmts lbl_code =
    let usefull_st = make_order_branch_inner lbl_code unique_flist allocated_new_stmts lbl_code in
    let unique_flist = refine_flist usefull_st unique_flist in
    if(compare_stmts_list usefull_st (allocated_new_stmts@unique_flist)) then true
    else false



let rec any_st_exists_in_list allocated = function
  []-> false
  | h::t-> if(st_exists_in_list h allocated) then true
           else any_st_exists_in_list allocated t

let rec any_exp_exists_in_stmt st = function
  []-> false
  | h::t -> if(is_return_id_used_inner st h) then true
            else any_exp_exists_in_stmt st t

let  rec verify_unordered arglist1 arglist2 = function
    []-> ((*print_string("\nFalse\n");*)false)
  |  h::t-> if(any_exp_exists_in_stmt h arglist1) && (any_exp_exists_in_stmt h arglist2) then ((*print_string("\nOKKKKKK\n");*)
              (*print_string("\nline no\n  ");*)
              (*print_string(string_of_int(find_startline_no [h]));*)
              (*print_string("\n\n");*)
(*            Printf.printf "\n\n\n%s\n\n" (Dumper.dump h ); *)
            true)
            else verify_unordered arglist1 arglist2 t
 


let rec any_from_allocated_with_newflist_inner exe_path arglist = function
    []-> false
  | h::t-> (match Ast_c.unwrap h with 
               (Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))) ->let arglist1 = make_argslist [] args in
                  if (verify_unordered arglist arglist1 exe_path) then true
                  else any_from_allocated_with_newflist_inner exe_path arglist t
               | _ -> any_from_allocated_with_newflist_inner exe_path arglist t 
           )

let rec any_from_allocated_with_newflist exe_path new_flist = function
    []-> false
  | h::t-> (match Ast_c.unwrap h with 
                (Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))) -> let arglist = make_argslist [] args in
		          if(any_from_allocated_with_newflist_inner exe_path arglist new_flist) then true
                          else any_from_allocated_with_newflist exe_path new_flist t
                 | _ -> any_from_allocated_with_newflist exe_path new_flist t
           ) 

let rec is_lbl_code_order exe_path new_flist allocated flist lbl_code  = 
 (*print_string("\nEnter\n");*)
   match (flist, lbl_code) with

     ([],[])->(* print_string("\nEnter1\n");*)true
   | ([],h::t)-> if(any_st_exists_in_list allocated (h::t)) then ( if (any_from_allocated_with_newflist exe_path new_flist (h::t)) 
                                                                       then ((*print_string("\nEnter23\n");*)false) 
                                                                   else ((*print_string("\nEnter24\n");*)true)) 
                 else ( (*print_string("\nEnter3\n");*)true)
   | (h::t, [])->  (*print_string("\nEnter4\n");*)true
   | (h::t, h1::t1)->  (*print_string("\nEnter5\n");*)(match ((Ast_c.unwrap h),(Ast_c.unwrap h1)) with

  ((Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))),
          (Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e1, args1)), typ1), ii1))))->
			        let arglist = make_argslist [] args in
                                let arglist1 = make_argslist [] args1 in
                                    if (compare_stmts h h1) then ((*print_string("\nEnter15\n");*)is_lbl_code_order exe_path (new_flist@[h]) allocated t t1)
                                    else if (st_exists_in_list h1 t)  then (if (verify_unordered arglist arglist1 exe_path) then 
				      ((*print_string("\nEnter16\n");*)false)
					                                    else ((*print_string("\nEnter17\n");*)is_lbl_code_order exe_path (new_flist@[h]) allocated t t1)
					                                   )
                                    else if (st_exists_in_list h1 allocated) then ((*print_string("\nEnter6\n");*)
				                                                   if (List.length new_flist) = 0 then
				                                                    ((*print_string("\nEnter18\n");*)is_lbl_code_order exe_path new_flist allocated (h::t) t1)
					                                           else 
										     (
										     if (verify_unordered arglist arglist1 exe_path) 
										     then ((*print_string("\nEnter19\n");*)false)
										else ((*print_string("\nEnter20\n");*)is_lbl_code_order exe_path new_flist allocated (h::t) t1)

										     )
                                                                                  )
                                    else ((*print_string("\nEnter7\n");*)is_lbl_code_order exe_path new_flist allocated (h::t) t1)
                         | _-> ((*print_string("\nfalse\n");*)false)         
                       )


let rec remove_useless_st new_lbl_code new_allocated useless if_list line_no exe_paths unique_flist = function
  []->(new_lbl_code , new_allocated, useless)
  | h::t-> if(st_exists_in_list h unique_flist) then remove_useless_st (new_lbl_code@[h]) new_allocated useless if_list line_no exe_paths unique_flist t
           else (match Ast_c.unwrap h with 
                        Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, args)), typ), ii))-> 
			let arglist = make_argslist [] args in
                          (match (find_same_arg_func_diff_name arglist unique_flist) with 
			    None -> if (any_arg_used_for_allocated exe_paths h line_no if_list (remove_option arglist) ) then
			              remove_useless_st (new_lbl_code@[h]) (new_allocated@[h]) useless if_list line_no exe_paths unique_flist t
				    else remove_useless_st new_lbl_code new_allocated (useless@[h]) if_list line_no exe_paths unique_flist t
			  | Some a-> 
				 remove_useless_st (new_lbl_code@[a]) new_allocated useless if_list line_no exe_paths unique_flist t
			  )
                      | _ -> remove_useless_st new_lbl_code new_allocated (useless@[h]) if_list line_no exe_paths unique_flist t 
											     
                    ) 




let rec find_pre_stmt pre_st st = function
  []-> None 
  | h::t-> if (compare_stmts h st) then pre_st
           else find_pre_stmt (Some h) st t

let rec find_post_stmt  st = function
  []-> None
  |  h::t-> if (compare_stmts h st) then (if(List.length t)=0 then None
	                                  else (Some (List.hd t)))
            else find_post_stmt  st t


let rec insert_missing_st refine_lbl_code st pre_st = function
  []-> refine_lbl_code
  | h::t-> if(compare_stmts pre_st h) then (refine_lbl_code@[h]@[st]@t)
           else insert_missing_st (refine_lbl_code@[h]) st pre_st t 
 

let rec do_refine_lbl_code  lbl_code unique_flist = function
  []->lbl_code
  | h::t-> let pre_st = find_pre_stmt None h unique_flist in
           let post_st = find_post_stmt h  unique_flist in
                           (match pre_st with
	                         None -> do_refine_lbl_code  (h::lbl_code) unique_flist t
	                       | Some a->do_refine_lbl_code (insert_missing_st [] h a lbl_code) unique_flist t
                                          
                           )
            

let rec find_suffix_of_st st = function
  []-> []
  | h::t-> if(compare_stmts st h) then t
           else find_suffix_of_st st t

let rec find_usefull_st_list sub_usefull_list usefull_list = function
  []-> sub_usefull_list
  |  h::t -> if(st_exists_in_list h usefull_list) then find_usefull_st_list (sub_usefull_list@[h]) usefull_list t
             else find_usefull_st_list sub_usefull_list usefull_list t

let rec insert_useless_st_for_order new_lbl_code usefull_list st = function
  []->new_lbl_code
  |  h::t -> if (st_exists_in_list h usefull_list) then (new_lbl_code@[st]@(h::t))
             else insert_useless_st_for_order (new_lbl_code@[h]) usefull_list st t

let rec insert_useless_st_for_order_outer usefull_list lbl_code = function
  []-> lbl_code
  | h::t-> let suffix_st = find_suffix_of_st h lbl_code in
           let sub_useful_list = find_usefull_st_list [] usefull_list suffix_st in
           let new_lbl_code = insert_useless_st_for_order [] sub_useful_list h usefull_list in
           insert_useless_st_for_order_outer usefull_list new_lbl_code t




let rec prepare_flist_new df_eq lcol rcol line_no iifunc1 flist line_no if_list  = function
    []-> []  
  | h::t-> (prepare_flist df_eq lcol rcol line_no iifunc1 [] h flist line_no if_list flist)::(prepare_flist_new df_eq lcol rcol line_no iifunc1 flist line_no if_list t) 


let rec get_intersect_flist l1 = function
   []->[]
  | h::t-> if(st_exists_in_list h l1) then h::(get_intersect_flist l1 t)
           else get_intersect_flist l1 t


let rec get_intersect_flist_list new_flist = function
  []-> new_flist
  | h::t-> let new_list = get_intersect_flist new_flist h
           in
           get_intersect_flist_list new_list t 

let rec get_single_flist_inner  lcol rcol line_no iifunc1 flist = function
    []-> true
  | h::t-> if (compare_stmts_list h flist) then  get_single_flist_inner lcol rcol line_no iifunc1 flist t
           else (

                       let start_line = line_no  in
                       let filename = Ast_c.file_of_info iifunc1 in
                     (*  let message = "Flist is not same" in *)

(*                             print_string("* TODO [[view:");*)
(*                             print_string(filename);    *)
(*                             print_string("::face=ovl-face1::linb=");  *)
(*                             print_string(string_of_int(line_no));   *)
(*                             print_string("::colb=");    *)
(*                             print_string(string_of_int(lcol));   *)
(*                             print_string("::cole=");   *)
(*                             print_string(string_of_int(rcol));   *)
(*                             print_string("][");    *)
(*                             print_string(message); *)
(*                             print_string(" ");   *)
(*                             print_string(filename); *)
(*                             print_string("::");   *)
(*                             print_string(string_of_int(line_no));    *)
(*                             print_string("]]");   *)
(*                             print_string("\n");         *)
                  false
                )

let rec get_single_flist lcol rcol line_no iifunc1 flist = function
    []-> flist
  | h::t -> if(get_single_flist_inner lcol rcol line_no iifunc1 h t) then h else (get_intersect_flist_list h t)
               

let rec find_prefix_lbl_code flist prefix = function
    []-> prefix 
  | h::t-> if(compare_stmts_list (h::t) flist) then prefix
           else find_prefix_lbl_code flist (prefix@[h]) t


let rec any_arg_found_df_eq_but_null df_eq = function
    [] -> false
  |  h::t-> if(is_arg_exists_is_df_eq_but_null h df_eq) then true
           else any_arg_found_df_eq_but_null df_eq t

let rec does_not_have_func = function
    []-> true
  |  h::t-> (match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
                if(List.length (make_argslist [] es) = 0) then false
                else does_not_have_func t
    |  _->  does_not_have_func t )


let rec find_new_st_for_flist flist exe_paths df_eq new_st_list  iifunc1 = function
    []-> new_st_list
  | h::t-> (match Ast_c.unwrap h with 
                Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) -> 
		  let arglist = remove_option (make_argslist [] es) in
	(*	   if ((List.length arglist = 0 ) &&  (does_not_have_func exe_paths )) *)
(*		    then ( print_string("\n**************************************************@\n");*)
	(*		   print_string(string_of_int(find_startline_no [h]));*)
(*		     find_new_st_for_flist flist exe_paths df_eq new_st_list t) *)
                   if (is_same_arg_diff_name_new df_eq (add_option arglist) exe_paths) && (List.length arglist != 0 )
		       then (  example_mark1 := 1;
		              find_new_st_for_flist flist exe_paths df_eq (new_st_list@[h]) iifunc1 t
		       )
(*                   else if(List.length arglist != 0) && ((not(any_arg_found_df_eq df_eq arglist)) ||  (is_same_arg_diff_name df_eq (add_option arglist) flist))*)
(*		       &&(not (is_same_arg_diff_name_new df_eq (add_option arglist) exe_paths))*)
(*                      then (  print_string("\n**************************************************@\n");*)
(*			    print_string(Ast_c.file_of_info iifunc1);*)
(*			    print_string(string_of_int(find_startline_no [h]));*)
			(*    Printf.printf "\n\n\n%s\n\n" (Dumper.dump arglist) ;*)
(*		             find_new_st_for_flist flist exe_paths df_eq new_st_list  iifunc1 t*)
(*			   )*)

                   else if (List.length arglist = 0 ) &&  (not(does_not_have_func exe_paths ))
		        then ( example_mark2 := 1;
		               find_new_st_for_flist flist exe_paths df_eq (new_st_list@[h])  iifunc1 t
		        )
	           else if(any_arg_found_df_eq df_eq arglist) && (not (is_same_arg_diff_name df_eq (add_option arglist) flist)) 
                      then (   example_mark3 :=  1;
		               find_new_st_for_flist flist exe_paths df_eq (new_st_list@[h])  iifunc1 t    
		       )
                   else ((*print_string("\n**************************************************@\n");*)
		         example_mark4 :=  1;
		         find_new_st_for_flist flist exe_paths df_eq new_st_list  iifunc1 t )
               | _-> find_new_st_for_flist flist exe_paths df_eq new_st_list  iifunc1 t 
                                                                             
            )




let rec find_new_st flist = function
   []-> []
  | h::t-> if(st_exists_in_list h flist) then find_new_st flist t
           else h::(find_new_st flist t)


let rec any_exp_exists_in_stmt_list arglist = function
   []-> false
  | h::t -> if(any_exp_exists_in_stmt h arglist) then true
            else any_exp_exists_in_stmt_list arglist t





let rec expr_exists_in_list exp = function
  []-> false
  | h::t-> if(compare_exp exp h) then true
           else expr_exists_in_list exp t




let rec chk_ass arglist arglist_for_all = function
  []-> false
  |  h::t-> (match Ast_c.unwrap h with
                Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) -> 
		            if(expr_exists_in_list e1 arglist_for_all) && (any_exp_exists_in_stmt h arglist) then true
                            else chk_ass arglist arglist_for_all t
                | _ -> chk_ass arglist arglist_for_all t 

           )


let rec make_union_arglist arglist_for_all = function
  []-> arglist_for_all
  | h::t-> if(expr_exists_in_list h arglist_for_all) then make_union_arglist arglist_for_all t
           else make_union_arglist (arglist_for_all@[h]) t


let rec make_stmt_list_for_arg = function
  []->[]
  | h::t-> (Ast_c.ExprStatement(Some h))::(make_stmt_list_for_arg t)





let rec make_arglist_for_all arglist_for_all df_eq = function
    []-> arglist_for_all
  | h::t-> (match Ast_c.unwrap h with
               Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) -> 
		   let arglist = make_argslist [] es in
                   let arglist = refine_arglist  df_eq (remove_option arglist) in
                   let arglist_for_all = make_union_arglist arglist_for_all arglist in
                    make_arglist_for_all arglist_for_all df_eq t
            | _ -> make_arglist_for_all arglist_for_all df_eq t

           )

let rec chk_func_call arglist arglist_for_all = function
   []-> false
  | h::t-> (match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
                 if (any_exp_exists_in_stmt h arglist) && (any_exp_exists_in_stmt h (add_option arglist_for_all)) then true
                 else chk_func_call arglist arglist_for_all t
              | _-> chk_func_call arglist arglist_for_all t 
           )
let is_depend arglist list df_eq exe_path =
                if(any_exp_exists_in_stmt_list arglist list) then true
                else if (chk_ass arglist (make_arglist_for_all [] df_eq list) df_eq) then true
                else if (chk_func_call arglist (make_arglist_for_all [] df_eq list) exe_path ) then true
                else false


let rec make_order  prefix suffix st arglist df_eq exe_path= function
  []-> (prefix@[st]@suffix)
  | h::t-> if (compare_stmts h st) then make_order prefix suffix st arglist df_eq exe_path t
           else if(is_depend arglist [h] df_eq exe_path ) then make_order  prefix (suffix@[h]) st arglist df_eq exe_path t
           else make_order  (prefix@[h]) suffix st arglist df_eq exe_path t

let rec make_order_main df_eq exe_path lbl_code = function
  []-> lbl_code
  | h::t->( match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
		let arglist = remove_option (make_argslist [] es) in
                let arglist = refine_arglist  df_eq arglist in
                let lbl_code = make_order  [] [] h (add_option arglist) df_eq exe_path lbl_code in
                     make_order_main df_eq exe_path lbl_code t
              | _ -> make_order_main df_eq exe_path lbl_code t
          )


let rec is_unorder_bug exe_path df_eq = function 
  []-> false
  | h::t-> ( match Ast_c.unwrap h with 
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
                let arglist = remove_option (make_argslist [] es) in
                let arglist = refine_arglist  df_eq arglist in
                if(is_depend (add_option arglist) t exe_path df_eq) then true
                else is_unorder_bug exe_path df_eq t
              | _ -> is_unorder_bug exe_path df_eq t 
           )


let rec is_link arglist1  ref_list = function
    []-> false
  | h::t-> (match Ast_c.unwrap h with
               Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) -> 
		 let arglist2 =  remove_option (make_argslist [] es)  in
                 let ref_list =  refine_ref_list  arglist2 ref_list in 
                 if(arg_exists_in_ref_list ref_list arglist1) then true
                 else is_link arglist1  ref_list t
             | _ -> is_link arglist1  ref_list t
           )


let rec find_ref_list_inner arg = function
   []-> []
  |  (a,b)::t -> if(exp_exists_in_list arg b) then a::(find_ref_list_inner arg t)
             else find_ref_list_inner arg t


let rec find_ref_list ref_list new_ref_list = function
    []->[]
  | h::t-> let ref_list_tmp = find_ref_list_inner h ref_list in
           find_ref_list ref_list (new_ref_list@ref_list) t

let any_arg_found_df_eq_but_null_main df_eq  h =
                       match Ast_c.unwrap h with
                         Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
                              let arglist1 =  make_argslist [] es  in
                              let arglist2 = refine_arglist df_eq (remove_option arglist1) in
			      if (any_arg_found_df_eq_but_null df_eq  arglist2) then true
			      else false
                       | _ -> false



let rec any_arg_found_df_eq_but_null_main_st df_eq  = function
  []-> false
  | h::t-> if(any_arg_found_df_eq_but_null_main df_eq  h) then true else any_arg_found_df_eq_but_null_main_st df_eq t

let rec is_missing_bug  mark_bug if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list= function
  []-> (*if(any_arg_found_df_eq_but_null_main_st df_eq  lbl_code) then ( *)
       (*                                        let start_line = line_no  in  *)
       (*                                        let filename = Ast_c.file_of_info iifunc1 in  *)
       (*                                        let message =  "Unorder Access" in  *)
       (*                                        print_string("* TODO [[view:");  *)

(*                                               print_string(filename);  *)
(*                                               print_string("::face=ovl-face1::linb=");  *)
(*                                               print_string(string_of_int(line_no));  *)
(*                                               print_string("::colb=");  *)
(*                                               print_string(string_of_int(lcol));  *)
(*                                               print_string("::cole=");  *)
(*                                               print_string(string_of_int(rcol));  *)
(*                                               print_string("][");*)
(*                                               print_string(message);*)
(*                                               print_string(" "); *)
(*                                               print_string(filename);*)
(*                                               print_string("::");*)
(*                                               print_string(string_of_int(line_no)); *)
(*                                               print_string("]]"); *)
(*                                               print_string("\n");           *)
            (missing_st,bug_cause, lbl_code, bug_list)   
(*       )   *)
(*       else (missing_st,bug_cause, lbl_code)   *)
  | h::t-> if(st_exists_in_list h lbl_code) (*&& (not (any_arg_found_df_eq_but_null_main df_eq  h) )*)
then ( (*print_string("\nXXXXXX9\n");*)is_missing_bug  mark_bug if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t)
           else    ( (*print_string("\nBBBBBBBBBBB3\n");*)
                    match Ast_c.unwrap h with
                         Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->  (*print_string("\nBBBBBBBBBBBXXXX\n");*)
                              let arglist1 =  make_argslist [] es  in
                              let arglist2 = refine_arglist df_eq (remove_option arglist1) in
              
			      
			      let new_lbl_code = (find_return_st lbl_code) in
			      
			      if ((List.length arglist1 =0 ) && 
does_not_have_func (if(List.length exe_paths>0) then (List.hd exe_paths) else []) && mark_bug = 0) 
			      then ( (*print_string("\nXXXXXX1\n");*)
                                         is_missing_bug  mark_bug if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t)
                              else if (any_arg_used_in_return (find_return_st lbl_code) arglist2) 
			           then ((*print_string("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX2\n");*)
(*                          print_string(Ast_c.file_of_info iifunc1);*)
(*                          print_string(string_of_int(line_no));*)

					 
					 is_missing_bug   mark_bug  if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t )
                              else if ((List.length arglist1 !=0 ) && (is_same_arg_diff_name  df_eq arglist1 (skip_return lbl_code))) 
				       (*(not(any_arg_found_df_eq_but_null df_eq  arglist2))*)
			           then ((*print_string("\nXXXXXXCCCCCCCCCCCCCCCCCCCCCCCCCC3\n");*)
(*                            print_string(Ast_c.file_of_info iifunc1);*)
(*                            print_string(string_of_int(line_no));*)
					 is_missing_bug   mark_bug  if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t )
                              else if ((List.length arglist2 !=0 ) && (is_same_arg_diff_name df_eq (add_option arglist2) (skip_return lbl_code))) 
				(*(not(any_arg_found_df_eq_but_null df_eq  arglist2)) *)
                                   then ( (*print_string("\nXXXXXX4\n");*)
                                         is_missing_bug  mark_bug   if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t )
                              else if (is_link (refine_arglist df_eq (remove_option arglist1)) ref_list (skip_return lbl_code)) then 
				((*print_string("\nXXXXXX*****************************************************************************5\n");*)
				         is_missing_bug  mark_bug   if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t)
                              else if (not (any_arg_found_df_eq df_eq  arglist2)) && (List.length arglist1 !=0 ) 
				    (*  (any_arg_found_df_eq_but_null df_eq  arglist2) *)
				  then ((*print_string("\nXXXXXX6\n");*)
			           is_missing_bug  mark_bug  if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t)
				       
      else if (is_same_arg_diff_name_in_path_new df_eq (add_option(remove_integer arglist2)) if_list exe_paths h line_no) && (List.length arglist1 !=0 )   
        then ((*print_string("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX7\n");*)
                            (*print_string(Ast_c.file_of_info iifunc1);*)
                            (*print_string(string_of_int(line_no));*)

	       is_missing_bug  mark_bug  if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t  )
    (*   else if (((any_arg_used_in_if_exp_list  arglist2 if_exp_list) ||  *)
(*                 (any_arg_used_in_if_exp_list  arglist2 (find_ref_list ref_list [] arglist2)))*)
(*&& mark_bug = 0 ) then *)
(*is_missing_bug mark_bug if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t *)
       else if ((any_arg_used_in_if_exp_list  (remove_option arglist1) if_exp_list ) || (List.length (remove_integer arglist2) =0)
     || (any_exp_exists_in_stmt_list (add_option arglist2) lbl_code))     && mark_bug = 1
        
                                   then  (     
  is_missing_bug mark_bug if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t ) 

(*			      else if (arg_link_to_ref_list arglist1 ref_list (skip_return lbl_code)) *)
(*			          then  is_missing_bug exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist t *)
       else if(already_have line_no h bug_list) then 
            is_missing_bug   mark_bug if_exp exe_paths if_list df_eq missing_st lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t  
                              else (
                                        let start_line = line_no  in
                                        let filename = Ast_c.file_of_info iifunc1 in

                                        if(!fixing = 1) then (

                                               let refine_lbl_code =  do_refine_lbl_code  lbl_code flist [h] in 
                                               is_missing_bug   mark_bug if_exp exe_paths if_list df_eq (missing_st@[h]) lcol rcol line_no iifunc1 refine_lbl_code bug_cause flist ref_list bug_list  if_exp_list t
					  ) 
                                        else if ((!fixing = 1) && ((Org.check_org filename start_line) = false)) then     
                                              is_missing_bug   mark_bug if_exp exe_paths if_list df_eq (missing_st@[h]) lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t                   
                                        else (
                                             if(!fixing = 0) then (

                                                 (*  print_string("\Missing function  \n");   *)
(*                                                   Printf.printf "\n\n\n%s\n\n" (Dumper.dump h) ;  *)

(*                                                   print_string("\Arglist  \n"); *)
(*                                                   Printf.printf "\n\n\n%s\n\n" (Dumper.dump arglist2) ;*)

					       let bug_list = (bug_list@[(line_no, h)]) in
				               let start_line = line_no  in
                                               let filename = Ast_c.file_of_info iifunc1 in   
		                               let message = if mark_bug = 0 then "Missing BUG" else "Missing BUG From Bottom" in   
(*                                               let bug_cause =   *)
(*  					         (match bug_cause with   *)
(*  					           None -> print_string("* TODO [[view:");(Some h)   *)
(*  					         | Some a -> if (compare_stmts a h) then (print_string("* TODO [[view:"); (Some a))   *)
(* 					                     else (print_string("* TODO [[view:"); (Some h))   *)
(* 					         ) in *)
(*					       print_string(filename);   *)
(*					       print_string("::face=ovl-face1::linb="); *)
(*					       print_string(string_of_int(line_no)); *)
(*					       print_string("::colb="); *)
(*					       print_string(string_of_int(lcol));   *)
(*					       print_string("::cole=");   *)
(*					       print_string(string_of_int(rcol)); *)
(*					       print_string("][");   *)
(*					       print_string(message);   *)
(*					       print_string(" ");   *)
(*					       print_string(filename); *)
(*					       print_string("::"); *)
(*					       print_string(string_of_int(line_no)); *)
(*					       print_string("]]");  *)
(*					       print_string("\n"); *)
					       mark:=0;
                                     is_missing_bug   mark_bug if_exp exe_paths if_list df_eq (missing_st@[h]) lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t  )
                                             else
					       is_missing_bug   mark_bug if_exp exe_paths if_list df_eq (missing_st@[h]) lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t
                                         )
					 )
                       | _ -> is_missing_bug   mark_bug if_exp exe_paths if_list df_eq (missing_st@[h]) lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list bug_list  if_exp_list t 

                   )

let rec divide_lbl_code new_lbl_code = function
    []-> (new_lbl_code,None)
  | h::t-> (match Ast_c.unwrap h  with
             Ast_c.Jump (Ast_c.Goto name) ->(new_lbl_code, Some h)
          |  _ -> divide_lbl_code (new_lbl_code@[h]) t 
           )


let rec get_code_goto goto_list lbl_list lbl_code = 
   let (new_lbl_code, goto_st) = divide_lbl_code [] lbl_code in
    
(*                                                   print_string("\new_lbl_code  \n");  *)
(*                                                   print_string(string_of_int(List.length new_lbl_code));  *)
  (*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_lbl_code ) ; *)
   match goto_st with
     None -> new_lbl_code
   | Some a -> if (st_exists_in_list a goto_list) then (new_lbl_code@[a])
               else
                  let goto_list = (goto_list@[a]) in
                  let goto_code = (match Ast_c.unwrap a with 
			              Ast_c.Jump (Ast_c.Goto name) -> 
					(new_lbl_code@(refine_code(code_for_goto name lbl_list)))
			             | _  -> []
			          )
		  in
                  get_code_goto goto_list lbl_list goto_code 
                  
      

let get_lbl_code_frm_goto lbl_code lbl_list = 
     if (has_return_all lbl_code) then (lbl_code,0)
     else( goto_mark:=1; (*print_string("\nFor GOTO \n"); *)
           ((get_code_goto [] lbl_list lbl_code),1 )
         )



     let rec check_for_bugs_new mark_bug lbl_list flist iifunc1 bug_cause if_list  = function
         []-> []
       | (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list  )::t ->
     

           
    
           let tmp_flist = prepare_flist_new df_eq lcol rcol line_no iifunc1 flist line_no if_list exe_paths in
                                            
                                            

           let flist = get_single_flist lcol rcol line_no iifunc1 flist tmp_flist  in 



           let tmp_lblcode = lbl_code in 
           let (lbl_code,l_mark) = get_lbl_code_frm_goto lbl_code lbl_list in
           if( l_mark =1 && (not(has_return_all lbl_code))) then ( 
               (line_no, if_code, tmp_lblcode, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list  )::(check_for_bugs_new mark_bug lbl_list flist iifunc1 bug_cause if_list t))
          else(  




           let (missing_st, bug_cause, refine_lbl_code, bug_list) =is_missing_bug mark_bug if_exp  exe_paths if_list df_eq [] lcol rcol line_no iifunc1 lbl_code bug_cause flist ref_list  bug_list if_exp_list flist in
         
            if (is_suffix flist (skip_return lbl_code)) then
                     let prefix_lbl_code = find_prefix_lbl_code flist [] (skip_return lbl_code) in
                     let new_st_for_flist = find_new_st_for_flist flist (if (List.length exe_paths)> 0 then  (List.hd exe_paths)
			                                                 else [] ) df_eq [] iifunc1 prefix_lbl_code in 

		     mark := 0;
		     if(!example_mark1 = 1) && (!example_mark2 = 1) && (!example_mark3 = 1)  && (!example_mark4 = 1) && (!example_mark5 = 1) then (


                              print_string("\n");
                (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug  lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list t))
																
                     else 
         (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug  lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list t)

															           
            else if (List.length missing_st) != 0 then


                     let start_line = line_no  in
                     let filename = Ast_c.file_of_info iifunc1 in
                     let new_st = find_new_st flist (skip_return lbl_code) in
                     let new_st_for_flist = 
		       find_new_st_for_flist flist (if(List.length exe_paths>0) then (List.hd exe_paths) else []) df_eq []  iifunc1 new_st in

                     if(!fixing = 1) then 
                        (
                                                   print_string("\nMissing St \n"); 
                                                   print_string(string_of_int(List.length missing_st));




(*                        let lbl_code = ((make_order_main df_eq (List.hd exe_paths) (skip_return refine_lbl_code)  *)
(*			                     (skip_return (List.rev refine_lbl_code)))@[(List.hd (List.rev refine_lbl_code))]) in *)

(*                                                   print_string("\nlbl_code after \n"); *)
(*                                                   print_string(string_of_int(List.length lbl_code)); *)
(*                                                   Printf.printf "\n\n\n%s\n\n" (Dumper.dump lbl_code ) ; *)
                mark:=0;

                (line_no, if_code, refine_lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list t)                        
                        )
		     else if ((!fixing = 1) && ((Org.check_org filename start_line) = false)) then ( mark:=0;
                       (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list  t)  )
                     else ( mark:=0;
			        if(!example_mark1 = 1) && (!example_mark2 = 1) && (!example_mark3 = 1)  && (!example_mark4 = 1) && (!example_mark5 = 1)   then(
				  

                      (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list  t)) 
																  
                                else
                      (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list  t)

                     )
                    
            else if (is_unorder_bug (if(List.length exe_paths>0) then (List.hd exe_paths) else []) df_eq (skip_return lbl_code)) then 
                     let new_st = find_new_st flist (skip_return lbl_code) in
                     let new_st_for_flist = find_new_st_for_flist flist (if(List.length exe_paths>0) then (List.hd exe_paths) else []) df_eq []  iifunc1 new_st in
                     let start_line = line_no  in
                     let filename = Ast_c.file_of_info iifunc1 in
                     let message = if !mark = 0 then "Unorder BUG" else "GOTO Unorder BUG" in

		     if(!fixing = 1) then
                      ( 
(*		       let lbl_code = make_order_main df_eq (List.hd exe_paths) lbl_code  (List.rev lbl_code) in  *)


                mark:=0;
	       (line_no, if_code, (new_st@flist@[(List.hd (List.rev lbl_code))]), status, exe_paths, 
				   if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list t)
                      )
                     else if ((!fixing = 1) && ((Org.check_org filename start_line) = false)) then ( mark:=0;
                       (line_no, if_code, (new_st@flist@[(List.hd (List.rev lbl_code))]), status, 
			 exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list t) )
                     else (
                           if(!fixing = 0) && (!example_mark1 = 1) && (!example_mark2 = 1) && (!example_mark3 = 1)  && (!example_mark4 = 1) && (!example_mark5 = 1)  then(
                           mark:=0;
(*                           print_string("* TODO [[view:");   *)
(*                            print_string(filename);    *)
(*                            print_string("::face=ovl-face1::linb=");    *)
(*                            print_string(string_of_int(line_no));    *)
(*                            print_string("::colb=");    *)
(*                            print_string(string_of_int(lcol));   *)
(*                            print_string("::cole=");    *)
(*                            print_string(string_of_int(rcol));    *)
(*                            print_string("][");    *)
(*                            print_string(message);    *)
(*                            print_string(" ");   *)
(*                            print_string(filename);   *)
(*                            print_string("::");  *)
(*                            print_string(string_of_int(line_no));   *)
(*                            print_string("]]");      *)
(*                            print_string("\n");      *)

                           (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list t) )
                         else
                           (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list  t)
                         )
            else (
                          let start_line = line_no  in
                          let filename = Ast_c.file_of_info iifunc1 in
                          let new_st = find_new_st flist (skip_return lbl_code) in
                          let new_st_for_flist = find_new_st_for_flist flist (if(List.length exe_paths>0) then (List.hd exe_paths) else []) df_eq []  iifunc1 new_st in
                          mark:=0;
                          if(!fixing = 1 && (Org.check_org filename start_line)) then (
(*                             let lbl_code = make_order_main df_eq (List.hd exe_paths) lbl_code  (List.rev lbl_code) in   *)

                             (line_no, if_code, (new_st@flist@[(List.hd (List.rev lbl_code))]), status, 
			       exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list  t)
                          )
		          else if ((!fixing = 1) && ((Org.check_org filename start_line) = false)) then(


                           (line_no, if_code, (new_st@flist@[(List.hd (List.rev lbl_code))]), status, 
                            exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list t)
                          )
			  else (
                    
                                if(!example_mark1 = 1) && (!example_mark2 = 1) && (!example_mark3 = 1)  && (!example_mark4 = 1) && (!example_mark5 = 1)   then
                                   (

			  (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list  t))
			         else
                          (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_bugs_new mark_bug lbl_list (new_st_for_flist@flist) iifunc1 bug_cause if_list  t)
			  )
                 ))


let  check_for_bugs  flist iifunc1  if_list bug_cause = 
     let rec check_for_bugs_loop flist bug_cause = function
        []->[]
      | (line_no, if_code, lbl_code, status, exe_paths, exe_path_list, if_exp, lcol, rcol)::t ->
                               (*                  print_string("\nExe_path\n");*)
	(*                                         print_string(string_of_int(List.length exe_paths));*)

(*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump exe_paths ); *)
	                                         
  (* let (tmp_flist, mark )= prepare_flist_main lcol rcol line_no iifunc1 [] flist exe_path_list exe_path_list in *)
(*   let tmp_flist = if (mark = true) then tmp_flist else (  *)



  (*              let start_line = line_no  in  *)
   (*                let filename = Ast_c.file_of_info iifunc1 in  *)
      (*             let message = "Problem in flist....." in  *)
                
         (*          print_string("* TODO [[view:");  *)
               (*         print_string(filename);  *)
             (*           print_string("::face=ovl-face1::linb=");  *)
       (*                 print_string(string_of_int(line_no));  *)
         (*               print_string("::colb=");  *)
         (*               print_string(string_of_int(lcol));  *)
         (*               print_string("::cole=");  *)
        (*                print_string(string_of_int(rcol));  *)
         (*               print_string("][");  *)
         (*               print_string(message);  *)
          (*              print_string(" ");  *)
           (*             print_string(filename);  *)
          (*              print_string("::");  *)
            (*            print_string(string_of_int(line_no));  *)
           (*             print_string("]]");    *)
            (*            print_string("\n");  *)



  (*   tmp_flist ) in  *)

                                                (* let flists = prepare_flist lcol rcol line_no iifunc1 [] exe_paths flist flist in *)
	                                           let flists =[] in
						(* let exe_paths = List.hd exe_paths in*)
          (*                                     print_string("\nflist\n");*)
(*                                               print_string(string_of_int(List.length flists));*)
  (*                                             Printf.printf "\n\n\n%s\n\n" (Dumper.dump flists );*)

                                                                      let unique_flist = flists 
									 (*(if (is_flists_equal flists) then find_single_flist flists*)
                                                                                 (* else ( *)
									              (*   print_string(string_of_int(List.length flists));*)
                                                                                      (*   print_string("\nProblem with flist .......\n");flist)*)
                                                                      (*           )  *)
                                                                      in  

           (*                                      print_string("\n unique_flist\n"); *)
           (*                                      Printf.printf "\n\n\n%s\n\n" (Dumper.dump unique_flist ); *)

(*                                               print_string("\nlbl_code length\n");*)
  (*                                             print_string(string_of_int(List.length (skip_return lbl_code)));*)
						   let (new_lbl_code,new_allocated,useless) = 
						     remove_useless_st [] [] [] if_list line_no exe_paths unique_flist (skip_return lbl_code) in 
                                                   
(*                                                   let new_stmts = find_new_stmts lbl_code [] unique_flist (skip_return (lbl_code))  in *)
(*                                            let allocated_new_stmts = refine_allocated_new_stmts  unique_flist (find_allocated_new_stmts line_no exe_paths new_stmts if_list) in*)
  (*                                               print_string("\n new_lbl_code\n"); *)
(*                                                  Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_lbl_code ); *)

  (*                                               print_string("\n allocated_new_stmts\n"); *)
(*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_allocated ); *)

  (*                                               print_string("\n Useless\n");*)
(*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump useless );*)



 if (is_suffix unique_flist new_lbl_code) then (
  (*                                                            print_string("\nSuffix\n");*)
                          (line_no, if_code, lbl_code, status, exe_paths, exe_path_list, if_exp, lcol, rcol, "Nothing", 0, None)::(check_for_bugs_loop (new_allocated@unique_flist) bug_cause t)
                                               )
 else (							       
       let (missing_stmts,bug_cause) = 
	  find_missing_stmts bug_cause lcol rcol if_exp exe_paths if_list line_no iifunc1 [] new_lbl_code unique_flist in							        
		if (List.length missing_stmts) = 0 then(
(*                                                 print_string("\nflist\n");*)
(*                                                 print_string(string_of_int(List.length unique_flist)); *)

  (*                                               print_string("\nlbl_code\n");*)
    (*                                             print_string(string_of_int(List.length (skip_return (lbl_code))));*)
							 if ( (List.length new_lbl_code) = 0
								                         || (List.length unique_flist) = 0 
											 || (is_lbl_code_order exe_paths [] new_allocated  unique_flist new_lbl_code )) then
									     ( (* print_string("\nSuffix\n");*)
							(line_no, if_code, lbl_code, status, exe_paths, exe_path_list,  if_exp, lcol, rcol, "Donot Know", 0, None)::(check_for_bugs_loop (new_allocated@unique_flist) bug_cause t)
                                                                             )
						         else (
							   let start_line = line_no  in
							   let filename = Ast_c.file_of_info iifunc1 in
							   let message = "Unorder" in
							   print_string("* TODO [[view:");
							   print_string(filename);
							   print_string("::face=ovl-face1::linb=");
							   print_string(string_of_int(line_no));
							   print_string("::colb=");
							   print_string(string_of_int(lcol));
							   print_string("::cole=");
							   print_string(string_of_int(rcol));
							   print_string("][");
							   print_string(message);
							   print_string(" ");
							   print_string(filename);
							   print_string("::");
							   print_string(string_of_int(line_no));
							   print_string("]]");
							   print_string("\n");
                   
                   let refined_lbl_code = insert_useless_st_for_order_outer (new_allocated@unique_flist) lbl_code useless in

                                                 (*print_string("\n refined_lbl_code for order\n");*)
(*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump refined_lbl_code );*)

		  (line_no, if_code, lbl_code, status, exe_paths, exe_path_list,  if_exp, lcol, rcol, "Unorder", 0, None)::(check_for_bugs_loop (new_allocated@unique_flist) bug_cause t)))

               else(
                let refined_lbl_code =do_refine_lbl_code  lbl_code unique_flist missing_stmts  in
  (*                                               print_string("\n refined_lbl_code for missing\n");*)
(*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump refined_lbl_code );*)

               (line_no, if_code, lbl_code, status, exe_paths,exe_path_list, if_exp, lcol, rcol, "Missing", 0, None)::(check_for_bugs_loop (new_allocated@unique_flist) bug_cause t)
                   ))
                                                                                                                                   
								  
     in check_for_bugs_loop flist bug_cause if_list




let rec find_execution_path_main prog = function
  []->[]
  | (line_no, if_code, lbl_code, status)::t -> 
                                           let (new_exe_paths,if_exp, lcol, rcol,  df_eq , ref_list, if_exp_list  ) = find_execution_path line_no [] None None None [] [] [] prog in
               (*                                 print_string("\nExe Paths\n");*)
(*                                                print_string(string_of_int(List.length new_exe_paths));*)

                                               let exe_path_list = new_exe_paths in
					       let new_exe_paths =(
					       if (List.length new_exe_paths) > 0 then 
						    List.hd new_exe_paths 
                                               else []) in
					       
                                               let lcol =
						 (match lcol with 
						      None-> 0
						    | Some a -> a ) in 

					        let rcol =
                                                 (match rcol with
                                                    None-> 0
						 |  Some a -> a ) in

                                                (* print_string("\nExe Paths\n");*)
(*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_exe_paths );*)
(*                                                 print_string("\nIf exp\n");*)
(*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump if_exp );*)

                                                 (line_no, if_code, lbl_code, status, new_exe_paths, exe_path_list , if_exp, lcol, rcol)::(find_execution_path_main prog t)




let rec refine_df_eq = function
   []-> []
  | h::t-> (match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ), ii))), typ1), ii1)) -> h::(refine_df_eq t)
(*     |  Ast_c.ExprStatement (Some (((Ast_c.Assignment   *)
(*(e1, op, (((Ast_c.Ident (Ast_c.RegularName("NULL",ii3))), typ1), ii1))), typ), ii)) ->h::(refine_df_eq t) *)
             | _ -> refine_df_eq t
           ) 

let rec find_execution_path_outer prog = function
  []->[]
  |  (line_no, if_code, lbl_code, status)::t ->

                                      let (new_exe_paths,if_exp, lcol, rcol,  df_eq , ref_list, if_exp_list ) = 
					find_execution_path  line_no [] None None None [] [] [] prog in

(*                                                   print_string("\nraw Data flow equation before\n"); *)
(*                                                   Printf.printf "\n\n\n%s\n\n" (Dumper.dump df_eq ); *)
                                           let df_eq = refine_df_eq df_eq in
                                               (*     print_string("\nExe Paths\n"); *)
(*                                                    print_string(string_of_int(List.length new_exe_paths));  *)

                                               
                                               let lcol =
                                                 (match lcol with
                                                      None-> 0
                                                 |  Some a -> a ) in

                                                let rcol =
                                                 (match rcol with
                                                    None-> 0
                                                 |   Some a -> a ) in

                                                  (* print_string("\nExe Paths\n"); *)
(*                                                   Printf.printf "\n\n\n%s\n\n" (Dumper.dump new_exe_paths ); *)
(*						   print_string("\nData flow equation before\n");   *)
(*						   Printf.printf "\n\n\n%s\n\n" (Dumper.dump df_eq ); *) 
(*                                                 print_string("\nIf exp\n");*)
(*                                                 Printf.printf "\n\n\n%s\n\n" (Dumper.dump if_exp );*)

           (line_no, if_code, lbl_code, status, new_exe_paths , if_exp, lcol, rcol, df_eq,ref_list, [], if_exp_list )
						::(find_execution_path_outer prog t)




let rec generate_env_vars prog  lbl_list = function
  []-> []
  | (line_no, if_code, lbl_code, status)::t->
      let goto_code = gather_goto_code lbl_list [] [] lbl_code in 
      if(List.length goto_code = 0) then generate_env_vars prog lbl_list t
      else
           let (exe_paths, df_eq, ref_list, exp) = generate_execution_paths line_no [] [] [] lbl_list None prog in


(*	   print_string("\nref_list\n"); *)
  (*        print_string(string_of_int(List.length ref_list));*)
    (*       Printf.printf "\n\n\n%s\n\n" (Dumper.dump ref_list);*)

(*           print_string("\nData Flow\n");*)
(*           print_string(string_of_int(List.length df_eq));*)
(*           Printf.printf "\n\n\n%s\n\n" (Dumper.dump df_eq);*)

         (line_no, if_code, lbl_code, status, exe_paths, df_eq, ref_list, exp)::(generate_env_vars prog  lbl_list t)     




let rec create_basic_blks basic_blk_list stmt_list = function
    []->if (List.length stmt_list) = 0 then (basic_blk_list)
	else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no, stmt_list)]))
             
  | h::t-> (  match Ast_c.unwrap h with

              |  Ast_c.Labeled (Ast_c.Case  (e, st)) ->
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let new_basic_blk_list =
                      create_basic_blks tmp_basic_blk_list []  (create_compound_st st)
                  in
                      create_basic_blks new_basic_blk_list []  t


	      |	 Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) -> 
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let new_basic_blk_list =
                      create_basic_blks tmp_basic_blk_list []  (create_compound_st st)
                  in
                      create_basic_blks new_basic_blk_list []  t


	      |	 Ast_c.Labeled (Ast_c.Default st) -> 
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let new_basic_blk_list  =
                      create_basic_blks tmp_basic_blk_list []  (create_compound_st st)
                  in
                      create_basic_blks new_basic_blk_list []  t


	      |	 Ast_c.Compound statxs -> 
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let  new_basic_blk_list  =
                      create_basic_blks tmp_basic_blk_list []  (stmtEle_remove statxs)
                  in
                      create_basic_blks new_basic_blk_list []  t

              |	  Ast_c.Selection  (Ast_c.Switch (e, st)) ->
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let new_basic_blk_list =
                      create_basic_blks tmp_basic_blk_list []  (create_compound_st st)
                  in
                      create_basic_blks new_basic_blk_list []  t


	      |	 Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
		      if  (has_multiple_st (create_compound_st st2)) then 
                   
                         let tmp_basic_blk_list =
                            if (List.length stmt_list) = 0 then basic_blk_list
                            else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                         let new_basic_blk_list1 =
                            create_basic_blks tmp_basic_blk_list []  (create_compound_st st1)
                         in
                         let new_basic_blk_list2 =
                            create_basic_blks new_basic_blk_list1 []  (create_compound_st st1)
                         in

                            create_basic_blks new_basic_blk_list2 []  t


		       else
                         let tmp_basic_blk_list =                
                            if (List.length stmt_list) = 0 then basic_blk_list
                            else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                         let new_basic_blk_list  =
                            create_basic_blks tmp_basic_blk_list []  (create_compound_st st1)
                         in
                            create_basic_blks new_basic_blk_list []  t

	      |	 Ast_c.Iteration  (Ast_c.While (e, st)) ->
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let new_basic_blk_list =
                      create_basic_blks tmp_basic_blk_list []  (create_compound_st st)
                  in
                      create_basic_blks new_basic_blk_list []  t



	      |	 Ast_c.Iteration  (Ast_c.DoWhile (st, e)) -> 
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let new_basic_blk_list =
                      create_basic_blks tmp_basic_blk_list []  (create_compound_st st)
                  in
                      create_basic_blks new_basic_blk_list []  t



	      |	 Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)) -> 
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let new_basic_blk_list  =
                      create_basic_blks tmp_basic_blk_list []  (create_compound_st st)
                  in
                      create_basic_blks new_basic_blk_list []  t




	      |	 Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) -> 
                  let tmp_basic_blk_list =
                  if (List.length stmt_list) = 0 then basic_blk_list
                  else ( blk_no:=!blk_no+1; (basic_blk_list@[(!blk_no,  stmt_list)])) in
                  let new_basic_blk_list =
                      create_basic_blks tmp_basic_blk_list []  (create_compound_st st)
                  in
                      create_basic_blks new_basic_blk_list []  t


                  
	      |	 Ast_c.Jump (Ast_c.Goto name) ->
		            blk_no:=!blk_no+1;
                            create_basic_blks (basic_blk_list@[(( !blk_no, stmt_list@[h]))]) []  t
	      |	 Ast_c.Jump ((Ast_c.Continue|Ast_c.Break)) -> create_basic_blks basic_blk_list (stmt_list@[h])   t
	      |	 Ast_c.Jump (Ast_c.Return) -> 
                            blk_no:=!blk_no+1;
                            create_basic_blks (basic_blk_list@[(( !blk_no, stmt_list@[h]))]) []  t
	      |	 Ast_c.Jump (Ast_c.ReturnExpr e) ->  
                            blk_no:=!blk_no+1;
                            create_basic_blks (basic_blk_list@[(( !blk_no, stmt_list@[h]))]) []  t
	      |	 Ast_c.Jump (Ast_c.GotoComputed e) ->
                            blk_no:=!blk_no+1;
                            create_basic_blks (basic_blk_list@[(( !blk_no, stmt_list@[h]))]) []  t
	      |	 _ -> create_basic_blks basic_blk_list (stmt_list@[h]) t

           )



let rec create_cfg basic_blk_list stmt_list count pre post  = function
    []->if (List.length stmt_list) = 0 then (basic_blk_list)
        else (basic_blk_list@[((count+1), pre, post ,stmt_list)])

  |  h::t-> (  match Ast_c.unwrap h with

                |   Ast_c.Iteration  (Ast_c.While (e, st)) ->
                        let tmp_basic_blk_list =
                           if (List.length stmt_list) = 0 then basic_blk_list
                           else (basic_blk_list@[( (count+1), pre ,(count+2), stmt_list)]) in
                        let new_basic_blk_list =
                            create_cfg  tmp_basic_blk_list []  (count+1) count post (create_compound_st st)
                        in
                            create_cfg new_basic_blk_list []  count pre post t


                 |   Ast_c.Jump (Ast_c.Goto name) ->
                            create_cfg  (basic_blk_list@[(((count+1), pre, 0 ,stmt_list@[h]))]) []  (count+1) pre post t
                 |   Ast_c.Jump ((Ast_c.Continue|Ast_c.Break)) -> create_cfg basic_blk_list (stmt_list@[h])  count pre post  t
                 |   Ast_c.Jump (Ast_c.Return) ->
                            create_cfg (basic_blk_list@[( ((count+1), pre, 0 ,stmt_list@[h]))]) [] (count+1) pre post t
                 |   Ast_c.Jump (Ast_c.ReturnExpr e) ->
                            create_cfg (basic_blk_list@[(((count+1), pre, 0 ,stmt_list@[h]))]) []  (count+1) pre post t
                 |   Ast_c.Jump (Ast_c.GotoComputed e) ->
                            create_cfg (basic_blk_list@[(((count+1), pre, 0 ,stmt_list@[h]))]) [] (count+1) pre post t
                 |   _ -> create_cfg basic_blk_list (stmt_list@[h]) count pre post t

           )
 
let rec convert_if_bug_list_to_if_list = function
  []-> []
  | (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list,  if_exp_list )::t->
             (line_no, if_code, lbl_code, status)::(convert_if_bug_list_to_if_list t) 

let rec is_arg_defined_in_exe_path ex = function
  []-> false
  | h::t-> match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ1), ii1)), typ), ii)) ->
                  if(compare_exp e1 ex) then true
                  else is_arg_defined_in_exe_path ex t
           | _ -> is_arg_defined_in_exe_path ex t 


let rec any_arg_defined_in_exe_path exe_path = function
    []-> false
  | h::t-> if(is_arg_defined_in_exe_path h exe_path) then true
           else any_arg_defined_in_exe_path exe_path t



let rec is_illegal_access iifunc1 lcol rcol df_eq exe_paths line_no if_exp ref_list bug_list illegal_list = function
   []->illegal_list 
  | h::t-> match Ast_c.unwrap h with
            Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->  (*print_string("\nBBBBBBBBBBBXXXX\n");*)


                                              (* print_string("\nexe_path\n");*)
                                              (* print_string(string_of_int(List.length exe_paths));*)
                                              (* Printf.printf "\n\n\n%s\n\n" (Dumper.dump exe_paths );*)


                                              (* print_string("\ndf_eq\n");*)
                                              (* print_string(string_of_int(List.length df_eq));*)
                                               (*Printf.printf "\n\n\n%s\n\n" (Dumper.dump df_eq ); *)


                let arglist1 =  make_argslist [] es  in

                let arglist2 = refine_arglist df_eq (remove_option arglist1) in
                let exe_path = if (List.length exe_paths > 0) then (List.hd exe_paths) else [] in
                if(any_arg_defined_in_exe_path exe_path (remove_option arglist1)) && (not(already_have line_no h bug_list)) &&
		  (List.length arglist2 =0)
		   then (

                                               let start_line = line_no  in
                                               let filename = Ast_c.file_of_info iifunc1 in
                                               let message = "Illegal Access" in
              (*                                 print_string("* TODO [[view:");*)
(*                                               print_string(filename);*)
(*                                               print_string("::face=ovl-face1::linb=");*)
(*                                               print_string(string_of_int(line_no));*)
(*                                               print_string("::colb=");*)
(*                                               print_string(string_of_int(lcol));*)
(*                                               print_string("::cole=");*)
(*                                               print_string(string_of_int(rcol));*)
(*                                               print_string("][");*)
(*                                               print_string(message);*)
(*                                               print_string(" ");*)
(*                                               print_string(filename);*)
(*                                               print_string("::");*)
(*                                               print_string(string_of_int(line_no));*)
(*                                               print_string("]]");*)
(*                                               print_string("\n");*)


                     is_illegal_access iifunc1 lcol  rcol df_eq exe_paths line_no if_exp ref_list bug_list (illegal_list@[h]) t
                        )
                else is_illegal_access iifunc1 lcol rcol df_eq exe_paths line_no if_exp ref_list bug_list illegal_list t
          | _ -> is_illegal_access iifunc1 lcol  rcol df_eq exe_paths line_no if_exp ref_list bug_list illegal_list t 



let rec check_for_illegal_access lbl_list iifunc1 = function
    []->[]
  | (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::t->
     
           let (lbl_code,l_mark) = get_lbl_code_frm_goto lbl_code lbl_list in
               if( l_mark =1 && (not(has_return_all lbl_code))) then 
 (line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_illegal_access 
  lbl_list iifunc1 t)
               else(
                       let illegal_access = 
                             is_illegal_access iifunc1 lcol rcol df_eq exe_paths line_no if_exp ref_list bug_list [] lbl_code in
       
(line_no, if_code, lbl_code, status, exe_paths, if_exp, lcol, rcol, df_eq, ref_list, bug_list, if_exp_list )::(check_for_illegal_access 
  lbl_list iifunc1 t)
               )

let has_function st = 
  match Ast_c.unwrap st with
     Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op,((( Ast_c.FunCall  (e, es)), typ1), ii1) ), typ), ii)) -> false
  |  _ -> true

let rec find_example1_inner e11  e22 = function
  []-> false
  | h::t-> match Ast_c.unwrap h with
         Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
              if (e1_exists_in_st e11 h) && (has_function h)then true
              else find_example1_inner e11  e22 t
         | _-> find_example1_inner e11 e22 t
 

let rec find_example1 iifunc1 l_list = function
  []-> l_list
  | h::t-> match Ast_c.unwrap h with
             Ast_c.ExprStatement (Some ((Ast_c.Assignment (e1, op,((( Ast_c.FunCall  (e, es)), typ1), ii1) ), typ), ii)) ->
                  if (find_example1_inner e1 ((( Ast_c.FunCall  (e, es)), typ1), ii1) t) then 
		    ( 


                                               let filename = Ast_c.file_of_info iifunc1 in

                                               print_string("* TODO [[view:");
                                               print_string(filename);
                                               print_string("::face=ovl-face1::linb=");
                                               print_string(string_of_int(find_startline_no [h]));
                                               print_string("::colb=");

                                               print_string("::cole=");

                                               print_string("][");

                                               print_string(" ");
                                               print_string(filename);
                                               print_string("::");
                                         
                                               print_string("]]");
                                               print_string("\n");



		       l_list
                    )
                  else find_example1 iifunc1 l_list t
            | _ -> find_example1 iifunc1 l_list t 


let rec count_same_arg_diff_name = function
  []->[]
  | (a,b)::t-> let len = List.length b in
               if len = 0 then count_same_arg_diff_name t
               else if len = 1 then (count_sadn1:= !count_sadn1 + 1; count_same_arg_diff_name t)
               else if len = 2 then (count_sadn2:= !count_sadn2 + 1; count_same_arg_diff_name t)
               else if len = 3 then (count_sadn3:= !count_sadn3 + 1; count_same_arg_diff_name t)
               else if len = 4 then (count_sadn4:= !count_sadn4 + 1; count_same_arg_diff_name t)
               else if len = 5 then (count_sadn5:= !count_sadn5 + 1; count_same_arg_diff_name t)
               else if len = 6 then (count_sadn6:= !count_sadn6 + 1; count_same_arg_diff_name t)
               else if len = 7 then (count_sadn7:= !count_sadn7 + 1; count_same_arg_diff_name t)
               else if len = 8 then (count_sadn8:= !count_sadn8 + 1; count_same_arg_diff_name t)
               else if len = 9 then (count_sadn9:= !count_sadn9 + 1; count_same_arg_diff_name t)
               else if len = 10 then (count_sadn10:= !count_sadn10 + 1; count_same_arg_diff_name t)
               else (count_sadn11:= !count_sadn11 + 1; count_same_arg_diff_name t)



let rec find_if_code start_line = function
  []->[]
  | (line_no, if_code, lbl_code,status)::t-> 
                                             if line_no = start_line then if_code
                                             else find_if_code start_line t


let rec update_lbl_list st lbl = function
  []->[]
  | h::t-> if (compare_stmts h st) then
               (Ast_c.Labeled (Ast_c.Label (lbl, h)),[])::t
           else h::(update_lbl_list st lbl t)

let rec find_lbl_prog name = function
  []-> []
  | (a,b)::t-> if(compare_name a name) then b else find_lbl_prog name t

let rec update_new_lbl_prog name ele ele_lbl = function
 []-> []
|(a,b)::t-> if(compare_name a name) then ele::(update_new_lbl_prog name ele ele_lbl t) 
            else if (compare_name a ele_lbl) then update_new_lbl_prog name ele ele_lbl t
            else (a,b)::(update_new_lbl_prog name ele ele_lbl t)

let rec merge_hard_inner ele_lbl ele_st lbl_prog  = function
    []->lbl_prog
  | (a,b)::t-> if (is_suffix ele_st b) then
                  let new_st_list = update_lbl_list (List.hd ele_st) ele_lbl (find_lbl_prog a lbl_prog) in
                   (update_new_lbl_prog a  (a,new_st_list) ele_lbl lbl_prog )
               else   merge_hard_inner ele_lbl ele_st lbl_prog t


let rec merge_hard new_lbl_prog = function
    []->new_lbl_prog
  | (a,b)::t-> let new_lbl_prog1 = merge_hard_inner a b new_lbl_prog  (List.rev t) in
(*               Printf.printf "%s'\n " (Dumper.dump new_lbl_prog1);*)
               merge_hard new_lbl_prog1  t



let rec lbl_list_prog lbl_list = function
  []-> lbl_list
  | (a,b)::t-> if (List.length b)>0 then 
                 lbl_list_prog (lbl_list@((Ast_c.Labeled (Ast_c.Label (a, (List.hd b))),[])::(List.tl b))) t

             else lbl_list_prog lbl_list t


let rec refine_lbl_list = function
  []-> []
 |h::t->
        if (List.length t)>0 then

          match Ast_c.unwrap (List.hd t)  with
            Ast_c.Labeled (Ast_c.Label (lbl, st))->
                    if (compare_stmts st h) then refine_lbl_list t
                    else h::(refine_lbl_list t)
          | _-> 
                    if (compare_stmts (List.hd t) h) then refine_lbl_list t
                    else h::(refine_lbl_list t)
       else h::(refine_lbl_list t)


let transform all_function names iifunc1 prog =


same_arg_diff_name_list:= [];
perfect_function:= 0;
unorder_count:= 0;
harder_list:= [];
simple_function:= 0;
hard_function:= 0;
harder_function:= 0;
hardest_function:= 0;
x:= 0;
blk_no:= 0;
example_mark1:=0;
example_mark2:=0;
example_mark3:=0;
example_mark4:=0;
example_mark5:=0;
if_branch_applied:= 0;

let lbl_list = create_lbl_list [] [] prog in
(*print_string("LBL_LIST:");*)
(*print_string(string_of_int(List.length lbl_list));*)




let if_list = (*if (has_switch prog) then [] else *) create_if_list  lbl_list prog in 
(*print_string("\nIf branch List Length \n");*)
(*print_string(string_of_int(List.length if_list));*)
(*print_string("\n\n");  *)

(*if_branch_count := !if_branch_count+(List.length (if_list));*)





(*****Begin *****)

(* if(List.length if_list)> 0 then *)
(*   if_branch_count:= !if_branch_count + 1; *)


(***** End  *****)








(* let col_info = Ast_c.col_of_info iifunc1 in *)
(* let col_info = Ast_c.line_of_info iifunc1 in *)
(* let line = Ast_c.file_of_info iifunc1 in *)

(*      let  ver_names = *)
(*                 try *)
(*                   Str.search_forward reg_ver line 0; *)
(*                   Str.matched_string line; *)
(*                 with Not_found -> "s" *)
(*      in *)

(*      let  dir_names = *)
(*                 try *)
(*                   Str.search_forward reg_dir line 0; *)
(*                   Str.matched_string line *)
(*                  with Not_found -> "s" *)
(*      in  *)



let if_list_goto = create_if_list_goto prog prog in

(*     ver_name:= ver_names; *)
(*     dir_name:= dir_names; *)


(* if(List.length if_list)> 0 then *)
(* (  s:= !s+1; *)
(*   path_list:=!path_list@[(Ast_c.file_of_info iifunc1),(Ast_c.str_of_name names)];  *)
(*  )  *)
(* else s:= !s; *)



if(List.length if_list)> 0 && (List.length if_list_goto)=0 then
          only_return:= !only_return+1 ;

if(List.length if_list)=0 && (List.length if_list_goto)>0 then
         only_goto:= !only_goto+1 ;

if(List.length if_list)> 0 && (List.length if_list_goto)> 0 then
         goto_return:= !goto_return+1;

if (List.length if_list)> 0 || (List.length if_list_goto)> 0 then
         ehc_count:= !ehc_count+1;


let return_id = find_return_id ([prog@(marge_if_list if_list)]) in

let mark = (
 match return_id with
     None -> 0
 |  Some a ->1
 ) in


let if_list = do_partition_if_list (update_if_list_cng_return return_id if_list) in
 
      if(List.length if_list)> 0 then
         return_after_partition:= !return_after_partition+1 else return_after_partition:= !return_after_partition;


(*print_string("\nIf branch List Length after partition \n");*)
(*print_string(string_of_int(List.length if_list));*)
(*print_string("\n\n");*)



let (prog_list,init_lbl_list)= create_init_lbl_prog_list [] [] prog in



let single_if_mark =
if(List.length if_list)> 0 && (List.length if_list)<2 then true else false
  in



let return_id_name =
(match return_id with
   None -> "rett"
|  Some (((Ast_c.Ident (Ast_c.RegularName(s,ii2))), typ), ii) -> s
)in


let if_list = filter_shared  return_id_name lbl_list init_lbl_list if_list in


let no_ass = count_assingment if_list in

if !perfect_function = 0 && (List.length if_list)>0 then 
   total_perfect_function:= !total_perfect_function + 1;

if !unorder_count = 1 && (List.length if_list) = 0 then
   total_unorder_count:= !total_unorder_count + 1;


if(List.length if_list)> 0 then
   (return_after_filter:= !return_after_filter+1;
  )
else 
           if(single_if_mark) then
             single_if:= !single_if+1;
      
    

(*let return_cng =   (match return_id with*)
(*                       None -> count_return_cng (create_id_name "ret") if_list *)
(*                     | Some (((Ast_c.Ident (Ast_c.RegularName(s,ii2))), typ), ii) -> count_return_cng (Ast_c.RegularName(s,ii2)) if_list) in*)

(*if(List.length if_list)> 0 then*)
(*(let example = find_example prog prog in*)
(*if example !=0 then*)
(*(print_string(string_of_int example ); ););*)




let function_return_id = find_function_return_id prog in   
let lbl_prog = create_lbl_prog prog in


(*let lbl_no = find_lbl_out_no prog in*)
let lbl_no = 0 in
let (if_list,lbl_prog,lbl_no,lbl_list) = set_simple_hard_end_code init_lbl_list lbl_list lbl_no [] if_list in
let (if_list,lbl_prog,lbl_no,lbl_list)   = set_harder function_return_id  lbl_prog lbl_list lbl_no [] if_list in 
let (if_list,lbl_prog,lbl_no,lbl_list) =  set_hardest function_return_id lbl_prog lbl_list lbl_no [] if_list in


let lbl_prog = create_lbl_prog prog in



let (if_list,lbl_prog,lbl_no,lbl_list) = set_hard lbl_prog lbl_list lbl_no [] if_list in

let (if_list,lbl_list) = set_simple lbl_list  [] if_list in



if (List.length if_list)>0 && !harder_function = 0 && !hardest_function = 0  then
      no_harder_hardest:= !no_harder_hardest + 1; 

if (List.length if_list)>0 &&  !hardest_function = 0   && !harder_function = 1  then
      single_harder:= !single_harder + 1;

if (List.length if_list)>0  && !hardest_function = 0   && !harder_function > 1  then
      multi_harder:= !multi_harder + 1;

if (List.length if_list)>0 &&  !hardest_function >=1 then
      hardest_function_total:= !hardest_function_total + 1;

if (List.length if_list)>0 &&  !if_branch_applied>= 1 then
      total_if_branch_applied:= !total_if_branch_applied + 1;

(*print_string(string_of_int(List.length lbl_list));*)
(*Printf.printf "%s'\n " (Dumper.dump lbl_list);*)
let lbl_p_list = merge_hard lbl_list lbl_list in 
(*Printf.printf "%s'\n " (Dumper.dump lbl_p_list);*)
let lbl_p_list = lbl_list_prog [] lbl_p_list in 

let lbl_p_list = refine_lbl_list lbl_p_list in

(*print_string(string_of_int(List.length lbl_p_list));*)
(*Printf.printf "%s'\n " (Dumper.dump lbl_p_list);*)



(* tmp *)
let lbl_p_list = [] in
lbl_p_list 
(*
   let rec transform_loop = function
        []->   lbl_p_list
      | h::t-> ( match h with
                  | (Ast_c.Labeled (Ast_c.Label (name, st)),ii) -> lbl_p_list  
                  | (Ast_c.Selection  (Ast_c.If (e, st1, st2)),ii) ->
                              (match ((has_inner_block st1),(has_inner_block st2)) with
                                 (true, true)->  let inner_prog1 = transform_loop (create_compound_st st1) in
                                                 let inner_prog2 = transform_loop (create_compound_st st2) in
                                                 let new_st2 = (Ast_c.Compound(stmtEle_add_list (inner_prog2)),[]) in
                                                 let outer_prog = transform_loop t in
                                                     (create_outer_stmt inner_prog1 new_st2  h)::outer_prog
                               | (true, false)-> let inner_prog1 = transform_loop (create_compound_st st1) in
                                                 let outer_prog = transform_loop t in
                                                     (create_outer_stmt inner_prog1 st2 h)::outer_prog
                               | (false, _ )-> let inner_prog2 =  
                                                   (match (has_inner_block st2) with 
                                                      true -> (Ast_c.Compound(stmtEle_add_list (transform_loop (create_compound_st st2))),[])
                                                    | false-> st2 
                                                   ) in
                                               let outer_prog = (transform_loop t) in
                                                 let start_line = (find_startline_no [st1])+1 in

                                                 let if_code = find_if_code start_line if_list in
                                                     if (List.length if_code)>0 then 
                                                         (create_if_st e inner_prog2 (create_id_name "out") if_code)::outer_prog
						     else (let outer_prog = (transform_loop t) in h::outer_prog)
                              )
                  | (Ast_c.Selection  (Ast_c.Switch (e, st)),ii) -> 
                              if(has_inner_block st) then
                                  let inner_prog = transform_loop (create_compound_st st) in
                                  let outer_prog = transform_loop t in
                                       (create_outer_stmt inner_prog st h)::outer_prog
                              else (let outer_prog = (transform_loop t) in h::outer_prog)

                  | (Ast_c.Iteration  (Ast_c.While (e, st)),ii) ->
                              if(has_inner_block st) then
                                  let inner_prog = transform_loop (create_compound_st st) in
                                  let outer_prog = transform_loop t in
                                       (create_outer_stmt inner_prog st h)::outer_prog
                              else (let outer_prog = (transform_loop t) in h::outer_prog)

                  | (Ast_c.Iteration  (Ast_c.DoWhile (st, e)),ii) ->
                              if(has_inner_block st) then
                                  let inner_prog = transform_loop (create_compound_st st) in
                                  let outer_prog = transform_loop t in
                                       (create_outer_stmt inner_prog st h)::outer_prog
                              else (let outer_prog = (transform_loop t) in h::outer_prog)

                  | (Ast_c.Iteration  (Ast_c.For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)),ii) -> 
                              if(has_inner_block st) then
                                  let inner_prog = transform_loop (create_compound_st st) in
                                  let outer_prog = transform_loop t in
                                       (create_outer_stmt inner_prog st h)::outer_prog
                              else (let outer_prog = (transform_loop t) in h::outer_prog)

                  | (Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)),ii) -> 
                              if(has_inner_block st) then 
                                  let inner_prog = transform_loop (create_compound_st st) in
                                  let outer_prog = transform_loop t in
                                       (create_outer_stmt inner_prog st h)::outer_prog
                              else (let outer_prog = (transform_loop t) in h::outer_prog)
                  | _ -> let outer_prog = (transform_loop t) in h::outer_prog
               )  
   in transform_loop prog
*)


let  analyze_def def =
    let defbis, ii = def in
    match ii with
    |   iifunc1::iifunc2::i1::i2::ifakestart::isto ->
        let {Ast_c.f_name = name;
             Ast_c.f_type = (returnt, (paramst, (b, iib)));
             Ast_c.f_storage = sto;
             Ast_c.f_body = statxs;
             Ast_c.f_attr = attrs;
            }   = defbis
            in
        all_def:= (!all_def@[(name,paramst,(stmtEle_remove statxs))]);
        (* the following call processes the statements *)

(*        Printf.printf "%s'\n " (Dumper.dump paramst);*)
(*print_string("\nParamset :  ");*)
(*print_string(string_of_int(List.length paramst));*)
        ( { defbis with Ast_c.f_body = List.map (function s -> Ast_c.StmtElem s)  
	     (transform !all_def name iifunc1 (stmtEle_remove statxs))
          }, ii )
    |   _ -> failwith "Impossible1"


let analyze_toplevel x  =
    match x with
    Ast_c.Declaration decl -> x
    |  Ast_c.Definition def -> Ast_c.Definition (analyze_def def)
    |  Ast_c.CppTop directive ->  x
    |  Ast_c.MacroTop (s, es,   [i1;i2;i3;i4]) -> x
    |  Ast_c.EmptyDef ii ->  x
    |  Ast_c.NotParsedCorrectly ii -> x
    |  Ast_c.FinalDef info -> x
    |  Ast_c.IfdefTop ifdefdir -> x
    |  Ast_c.MacroTop _ -> x

