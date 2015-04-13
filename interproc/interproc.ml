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

(* Whether goto statement exists in the statement list *)


let rec goto_exists_in_list = function
    []-> false
  |  h::t ->  match Ast_c.unwrap h with
                Ast_c.Jump (Ast_c.Goto name) -> true
    |   _ -> goto_exists_in_list t


(* Whether return statement exists in the statement list *)

let rec return_exists_in_list = function
    []-> false
  |  h::t ->  match Ast_c.unwrap h with
               Ast_c.Jump (Ast_c.ReturnExpr e) -> true
    |  Ast_c.Jump (Ast_c.Return ) -> true
    |  _ -> return_exists_in_list t



let rec refine_args_list list  = function
  []->list
  |  h::t-> match h with
             (((Ast_c.Ident ( Ast_c.RegularName (s, ii2))), typ), ii)-> (refine_args_list (list@[s]) t)
    |  ((( Ast_c.RecordAccess   ((((Ast_c.Ident (Ast_c.RegularName (s1, ii3))), typ1), ii1), (Ast_c.RegularName (s2, ii2)))), typ), ii) ->
               (refine_args_list (list@[s1]@[s2]) t)
    |   ((( Ast_c.RecordPtAccess   ((((Ast_c.Ident (Ast_c.RegularName (s1, ii3))), typ1), ii1), (Ast_c.RegularName (s2, ii2)))), typ), ii) ->
               (refine_args_list (list@[s1]@[s2]) t)
    | _->  refine_args_list list t


let rec find_func_frm_all_func callin_func = function
  []-> None
  |    (name,paramset,stmts)::t->
            ( match Ast_c.unwrap callin_func with
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (id)), typ1), ii1), es)), typ), ii)) ->
               if (Def.compare_names name id) then
                         Some (name, paramset ,stmts)
               else find_func_frm_all_func callin_func t
            | Ast_c.ExprStatement (Some (((Ast_c.Ident (ident)), typ), ii))->
		if (Def.compare_names name ident) then
		  Some (name, paramset, stmts)
                else find_func_frm_all_func callin_func t
            |    _ -> find_func_frm_all_func callin_func t
            )



let rec check_for_calling_func stmt trans_arglist all_function = function
     []-> false
  |   h::t->(
        match Ast_c.unwrap h with
        |    Ast_c.Labeled (Ast_c.Label (name, st)) ->
              if(check_for_calling_func stmt trans_arglist  all_function (Def.create_stmtlist st)) then true
              else check_for_calling_func stmt trans_arglist  all_function  t
        |    Ast_c.Labeled (Ast_c.Case  (e, st)) ->
               check_for_calling_func stmt trans_arglist  all_function t
        |    Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->
               check_for_calling_func stmt trans_arglist  all_function t

        |    Ast_c.Labeled (Ast_c.Default st) ->
              check_for_calling_func stmt trans_arglist  all_function t

        |    Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (ident)), typ1), ii1), es)), typ), ii)) ->



              (match Ast_c.unwrap stmt with
                  Ast_c.ExprStatement (Some (((Ast_c.FunCall  ((((Ast_c.Ident (ident1)), typ4), ii4), es1)), typ3), ii3)) ->

                     let args_list1 = Def.remove_optionlist (Def.create_argslist [] es1) in
		     let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
                     let args_list2 = List.map (function arg -> (Ast_c.ExprStatement (Some arg),[])) args_list in
                     if(Def.compare_stmts stmt h) then (true)
                     else if (refine_args_list [] args_list1) =  (refine_args_list [] args_list) then (true)
                     else if (
                      let rec loop = function
                         []-> false
			| argH::argT-> match (find_func_frm_all_func argH all_function) with
			               None -> loop argT
                                      | Some (name,paramset,stmts)->let trans_arglist1 =  trans_arglist in
					if (check_for_calling_func stmt trans_arglist1  all_function stmts) then true
                                        else loop argT
 
                      in loop args_list2
                     ) then (true)
                     else (
		       match (find_func_frm_all_func h all_function) with
                              None-> check_for_calling_func stmt trans_arglist  all_function t
                            | Some (name,paramset,stmts)->
                               let trans_arglist1 =  trans_arglist in
                                    if (check_for_calling_func stmt trans_arglist1  all_function stmts) then ((*print_string("\nAAAAAAAAAAAAA2\n");*)true)
                                    else check_for_calling_func stmt trans_arglist  all_function t
                      )
              |	 _ -> check_for_calling_func stmt trans_arglist  all_function t
              )


        |    Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
              check_for_calling_func stmt trans_arglist  all_function t
        |    Ast_c.Selection  (Ast_c.Switch (e, st1)) ->
              check_for_calling_func stmt trans_arglist  all_function t
        |    Ast_c.Iteration  (Ast_c.While (e, st1)) ->
              check_for_calling_func stmt trans_arglist  all_function t
        |    Ast_c.Iteration  (Ast_c.DoWhile (st1, e)) ->
              
              check_for_calling_func stmt trans_arglist  all_function t
        | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
	    failwith "for loop with declaration in first argument not supported"
	|    Ast_c.Iteration  (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st1)) ->
              check_for_calling_func stmt trans_arglist  all_function  t
        |    Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st1)) ->
              check_for_calling_func stmt trans_arglist  all_function t
        |    _ -> check_for_calling_func stmt trans_arglist  all_function t
    )


let rec find_func_frm_all_func_main st args_list all_function = function
     []-> false
   | (a,h)::t-> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some e) ->
                (match (find_func_frm_all_func h all_function) with
                   None-> find_func_frm_all_func_main st args_list all_function t
                |  Some (name,paramset,stmts)->
                         let trans_arglist = args_list in
                               if (check_for_calling_func st trans_arglist  all_function stmts) then (true)
                               else (find_func_frm_all_func_main st args_list all_function t)
                )
             | _-> find_func_frm_all_func_main st args_list all_function t




 let rec func_contains_rr miss_st args_list  = function  
  []-> false  
  | h::t-> 
        begin
           match Ast_c.unwrap h with           
             Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
	       let args_list1 = Def.create_argslist [] es in	            
               if (refine_args_list [] (Def.remove_optionlist args_list)) =  (refine_args_list [] (Def.remove_optionlist args_list1)) then  (true)
	       else ( func_contains_rr miss_st args_list t)
           | _->func_contains_rr miss_st args_list t 
         end

let rec find_all_interproc_func func_name function_list args_list miss_st  = function
  []->function_list
  | (name,paramset,stmts)::t-> if (not(Def.compare_names name func_name)) & ((List.length stmts) <12 )then 
                                 if(func_contains_rr miss_st args_list stmts) then find_all_interproc_func func_name ((name,paramset,stmts)::function_list) args_list miss_st t 
	                         else find_all_interproc_func func_name function_list args_list miss_st t  
                                else find_all_interproc_func func_name function_list args_list miss_st t  
 


let rec find_app_model_blk miss_st diff model model_line miss_blk = function
     []->(model_line, model)
  |   (brnch_strtlineno,test_case,typ,blk_strtlineno,blk_endlineno,stmtlist)::t->
      if(Def.stmt_exists_in_list miss_st stmtlist) then
          if (miss_blk - blk_strtlineno) < 0 && diff = 0 then (blk_strtlineno, stmtlist)
          else if (miss_blk - blk_strtlineno) > diff then find_app_model_blk miss_st (miss_blk - blk_strtlineno) stmtlist blk_strtlineno miss_blk t
          else find_app_model_blk miss_st diff model model_line miss_blk t
      else find_app_model_blk miss_st diff model model_line miss_blk t


 let rec name_exists_in_exepaths name exists = function 
  []-> exists
  | h::t-> match Ast_c.unwrap h with
            Ast_c.ExprStatement (Some (((Ast_c.Ident (ident)), typ), ii)) ->

              
	      if Def.compare_names ident name then  name_exists_in_exepaths name true t
              else name_exists_in_exepaths name exists t 
           | Ast_c.ExprStatement (Some (((Ast_c.FunCall (((((Ast_c.Ident (ident)), typ1), ii1)), es)), typ), ii)) ->
                     let args_list = Def.create_argslist [] es in
                     let args_list = List.map (function arg -> (Ast_c.ExprStatement (Some arg),[])) (Def.remove_optionlist args_list) in

	             if Def.compare_names ident name then name_exists_in_exepaths name true t
                     else if name_exists_in_exepaths name exists args_list then name_exists_in_exepaths name true t
		     else name_exists_in_exepaths name exists t
	   |  Ast_c.ExprStatement (Some ((((Ast_c.CondExpr (e1, e2, e3))), typ), ii)) ->
   	         if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
                 else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement e2,[])] then name_exists_in_exepaths name true t
                 else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e3),[])] then name_exists_in_exepaths name true t
                 else name_exists_in_exepaths name exists t

	   |  Ast_c.ExprStatement (Some ((((Ast_c.Sequence (e1, e2))), typ), ii)) ->
 	       if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
	       else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e2),[])] then name_exists_in_exepaths name true t
	       else name_exists_in_exepaths name exists t  

	   |  Ast_c.ExprStatement (Some ((((Ast_c.Assignment (e1, op, e2))), typ), ii)) ->
               if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
               else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e2),[])] then name_exists_in_exepaths name true t
               else name_exists_in_exepaths name exists t  

	   |  Ast_c.ExprStatement (Some ((((Ast_c.Postfix  (e, op)), typ), ii)))  -> 
               if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
               else name_exists_in_exepaths name exists t 

 	   |  Ast_c.ExprStatement (Some ((((Ast_c.Infix    (e, op)), typ), ii)))   ->  
                 if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
                 else name_exists_in_exepaths name exists t 
	   
           |  Ast_c.ExprStatement (Some (((Ast_c.Unary    (e, op)), typ), ii)) -> 
               if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
               else name_exists_in_exepaths name exists t 

	   |  Ast_c.ExprStatement (Some (((Ast_c.Binary   (e1, op, e2), typ), ii)))   -> 
                 if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
                 else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e2),[])] then name_exists_in_exepaths name true t
                 else name_exists_in_exepaths name exists t

	   |  Ast_c.ExprStatement (Some (((Ast_c.ArrayAccess    (e1, e2)), typ), ii)) ->
                 if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e1),[])] then name_exists_in_exepaths name true t
                 else if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e2),[])] then name_exists_in_exepaths name true t
                 else name_exists_in_exepaths name exists t

           |   Ast_c.ExprStatement (Some (((Ast_c.RecordAccess (e, name1)), typ), ii)) ->
                 if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
                 else name_exists_in_exepaths name exists t

	   |  Ast_c.ExprStatement (Some (((Ast_c.RecordPtAccess (e, name2)), typ), ii)) ->
                 if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
                 else name_exists_in_exepaths name exists t  

	   |  Ast_c.ExprStatement (Some (((Ast_c.SizeOfExpr  (e) ), typ), ii)) ->
               if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
               else name_exists_in_exepaths name exists t


  	   |  Ast_c.ExprStatement (Some (((Ast_c.ParenExpr (e)), typ), ii)) ->          
                 if name_exists_in_exepaths name exists [(Ast_c.ExprStatement (Some e),[])] then name_exists_in_exepaths name true t
                 else name_exists_in_exepaths name exists t 
           | _-> name_exists_in_exepaths name exists t

 let rec name_exists_in_exepaths_main name = function
  []-> false 
  | h::t-> if name_exists_in_exepaths name false h then true
           else name_exists_in_exepaths_main name t

let rec locally_defined ident = function
  []-> false
  |  (name,paramset,stmts)::rest -> if (Def.compare_names name ident) then true
                                    else locally_defined ident rest

let rec interproc_new func_name all_function miss_lineno errblks prog lbl_list miss_rr_ops_list = function
  | []-> miss_rr_ops_list
  | (alloc,args,h)::t->  
            match Ast_c.unwrap h with  
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  ( (((Ast_c.Ident (ident)), typ1), ii1), es)), typ), ii)) ->  
                let args_list = Def.create_argslist [] es in 
		if (List.length args_list) = 0 && (locally_defined ident all_function) then 
		  interproc_new func_name all_function miss_lineno errblks prog lbl_list miss_rr_ops_list t
                else if (List.length args_list) = 0 && (
                   match Ast_c.unwrap alloc with
                     Ast_c.ExprStatement (Some ((( Ast_c.FunCall  ( (((Ast_c.Ident (ident10)), typ11), ii11), es10)), typ10), ii10)) ->
		       let args_list10 = Def.create_argslist [] es10 in
		       if (List.length args_list10) = 0 then(
			 if (locally_defined ident10 all_function) then (true)
                         else false)
		       else false
                   | _-> false
                 ) then interproc_new func_name all_function miss_lineno errblks prog lbl_list miss_rr_ops_list t
                else(
                let interproc_funcs = find_all_interproc_func func_name [] args_list h all_function in
                if (List.length interproc_funcs) > 0 then( 

                   let (model_lineno, nearest_model) = find_app_model_blk h 0 [] 0 miss_lineno errblks in 
        	   let model_exe = Errorhandling.generate_exe_paths_simple model_lineno [] lbl_list prog in
                   let candidate_exe = Errorhandling.generate_exe_paths_simple miss_lineno [] lbl_list prog in

  
                      let rec loop  = function
                          []-> interproc_new func_name  all_function miss_lineno errblks prog lbl_list ((alloc,args,h)::miss_rr_ops_list) t
			| (name,paramset,stmts)::rest -> 
			    if (name_exists_in_exepaths_main name candidate_exe) && (not(name_exists_in_exepaths_main name model_exe))  then (
			      interproc_new func_name all_function miss_lineno errblks prog lbl_list miss_rr_ops_list t)
                            else loop rest
                      in loop interproc_funcs)
                  
                    
                else interproc_new func_name  all_function miss_lineno errblks prog lbl_list ((alloc,args,h)::miss_rr_ops_list) t
               )
              |_-> interproc_new func_name all_function miss_lineno errblks prog lbl_list miss_rr_ops_list t




let rec gather_all_callin_func line_no lbl_list = function
    []->[]
  |  h::t-> let start_lineno = Def.find_startline_no (Def.create_stmtlist h) in
            let end_lineno = Def.find_endline_no (Def.create_stmtlist h) in
            if(line_no>=start_lineno && line_no <= end_lineno) || line_no >= start_lineno then
              match Ast_c.unwrap h with
              |	  Ast_c.Labeled (Ast_c.Label (name, st)) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st)
                        else if line_no > end_lineno && (not(goto_exists_in_list (Def.create_stmtlist st))) && (not(return_exists_in_list (Def.create_stmtlist st))) then
                             gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st)
                        else if line_no < end_lineno then gather_all_callin_func line_no  lbl_list []
                        else gather_all_callin_func line_no  lbl_list t

              |	  Ast_c.Labeled (Ast_c.Case  (e, st)) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st)
                        else if line_no > end_lineno && (not(goto_exists_in_list (Def.create_stmtlist st))) && (not(return_exists_in_list (Def.create_stmtlist st))) then
                               gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st)
                        else if line_no < end_lineno then gather_all_callin_func line_no  lbl_list []
                        else gather_all_callin_func line_no  lbl_list t

              |	  Ast_c.Labeled (Ast_c.CaseRange  (e, e2, st)) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st)
                        else if line_no > end_lineno && (not(goto_exists_in_list (Def.create_stmtlist st))) && (not(return_exists_in_list (Def.create_stmtlist st))) then
                               gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st)
                        else if line_no < end_lineno then gather_all_callin_func line_no  lbl_list []
                        else gather_all_callin_func line_no  lbl_list t

              |	  Ast_c.Labeled (Ast_c.Default st) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st)
                        else if line_no > end_lineno && (not(goto_exists_in_list (Def.create_stmtlist st))) && (not(return_exists_in_list (Def.create_stmtlist st))) then
                               gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st)
                        else if line_no < end_lineno then gather_all_callin_func line_no  lbl_list []
                        else gather_all_callin_func line_no  lbl_list t
              |	  Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, (((Ast_c.FunCall  (e, es)), typ), ii))), typ1), ii1)) ->
                      ((Def.find_startline_no (Def.create_stmtlist h)),h)::(gather_all_callin_func line_no  lbl_list t)
              |   Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
	              ((Def.find_startline_no (Def.create_stmtlist h)),h)::(gather_all_callin_func line_no  lbl_list t)
              |	  Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii))->
                      ((Def.find_startline_no (Def.create_stmtlist h)),
                      (Ast_c.ExprStatement (Some e2),[]))::(gather_all_callin_func line_no  lbl_list t)
              |	  Ast_c.Selection  (Ast_c.If (e, st1, st2)) ->
                             let start_lineno_st1 = Def.find_startline_no (Def.create_stmtlist st1) in
                             let end_lineno_st1 = Def.find_endline_no (Def.create_stmtlist st1) in
                             let start_lineno_st2 = Def.find_startline_no (Def.create_stmtlist st2) in
                             let end_lineno_st2 = Def.find_endline_no (Def.create_stmtlist st2) in
                                if (line_no >= start_lineno_st1 && line_no <= end_lineno_st1) then
                                    gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st1)
                                else if (line_no >= start_lineno_st2 && line_no <= end_lineno_st2) then
                                      gather_all_callin_func line_no  lbl_list (Def.create_stmtlist st2)
                                else gather_all_callin_func line_no  lbl_list t

              |   Ast_c.Selection  (Ast_c.Switch (e, st)) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func  line_no lbl_list (Def.create_stmtlist st)
                        else gather_all_callin_func line_no  lbl_list t

              |   Ast_c.Iteration  (Ast_c.While (e, st)) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func  line_no lbl_list (Def.create_stmtlist st)
                        else gather_all_callin_func line_no  lbl_list t

              |   Ast_c.Iteration  (Ast_c.DoWhile (st, e)) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func line_no lbl_list (Def.create_stmtlist st)
                        else gather_all_callin_func line_no  lbl_list t

              | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
		  failwith "for loop with declaration in first argument not supported"
              |   Ast_c.Iteration  (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st)) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func line_no lbl_list (Def.create_stmtlist st)
                        else gather_all_callin_func line_no  lbl_list t

              |   Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st)) ->
                        if (line_no >= start_lineno && line_no <= end_lineno) then gather_all_callin_func line_no lbl_list (Def.create_stmtlist st)
                        else gather_all_callin_func line_no  lbl_list t
              |	   Ast_c.Jump (Ast_c.Goto name) -> gather_all_callin_func line_no  lbl_list t

              |	   Ast_c.Jump (Ast_c.Return) ->  gather_all_callin_func line_no  lbl_list []
	      |	   Ast_c.Jump (Ast_c.ReturnExpr e) -> gather_all_callin_func line_no  lbl_list []

              |  _-> gather_all_callin_func line_no  lbl_list t
           else gather_all_callin_func line_no  lbl_list t


	 








let rec find_interproc_calls branch all_function miss_lineno lbl_list prog miss_rr_ops_list = function
  []-> miss_rr_ops_list
  |  (alloc,args,h)::t-> match Ast_c.unwrap h with
              Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) -> 
                  let args_list = Def.create_argslist [] es in
		  if(List.length args_list) = 0 then find_interproc_calls branch all_function miss_lineno lbl_list prog ((alloc,args,h)::miss_rr_ops_list) t
                  else ( 
                          let all_callin_func = ((gather_all_callin_func (Def.find_endline_no branch)  lbl_list branch)
						   @(gather_all_callin_func miss_lineno lbl_list prog)) in
                          if(find_func_frm_all_func_main h args_list all_function all_callin_func) then
                             find_interproc_calls branch all_function miss_lineno lbl_list prog miss_rr_ops_list t
                          else find_interproc_calls branch all_function miss_lineno lbl_list prog ((alloc,args,h)::miss_rr_ops_list) t
		  )
              | _-> find_interproc_calls branch all_function miss_lineno lbl_list prog  miss_rr_ops_list t




