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

let all_def = ref []


let rec create_rr_ops_list func_name iifunc1 lbl_list rr_ops_list init_lbl_list errblks_list = function
  []-> rr_ops_list 
      
  | (brnch_strtlineno,test_case,typ,blk_strtlineno,blk_endlineno,stmtlist)::t->
      let new_rr_ops_list= Rr_op_finder.stack_rr_op_new func_name iifunc1 lbl_list rr_ops_list [] stmtlist stmtlist in
      create_rr_ops_list func_name iifunc1 lbl_list new_rr_ops_list init_lbl_list errblks_list t



let rec recheck_blks func_name iifunc1  lbl_list prog = function
       []-> []
     | (brnch_strtlineno,test_case,goto,st,typ,blk_strtlineno,blk_endlineno,stmtlist)::t->
           if (Errorhandling.remove_not_errorblks func_name iifunc1 test_case lbl_list prog stmtlist typ blk_strtlineno) 
	      then((recheck_blks func_name iifunc1 lbl_list  prog t))
           else (brnch_strtlineno,test_case,goto,st,typ,blk_strtlineno,blk_endlineno,stmtlist)::(recheck_blks func_name iifunc1 lbl_list  prog t)

let rec refine_rr_ops_inner args_list = function
  []-> false
  | (alloc, args, rrl)::t-> 
         if (Def.compare_explists args args_list) then true
         else refine_rr_ops_inner args_list t


let rec refine_rr_ops = function
  []->[]
  | (alloc, args, rrl)::t->
    if (refine_rr_ops_inner args t) then refine_rr_ops t
    else (alloc, args, rrl)::(refine_rr_ops t)


let analyze_each_blk_new func lbl_list rr_ops_list prog iifunc1 all_def func_name init_lbl_list errblks_list =
  let rec analyze_each_blk_loop_new  = function
    []->[]
    |  (brnch_strtlineno,test_case,typ,blk_strtlineno,blk_endlineno,stmtlist)::t-> 
          let miss_rr_ops_list = Rr_op_finder.find_missing_rr_ops_new lbl_list stmtlist rr_ops_list in
          let update_miss_rr_ops_list = refine_rr_ops miss_rr_ops_list in
                 if(List.length update_miss_rr_ops_list)> 0 then(
                      let miss_rr_ops_list_new  = Rm_true_positives.is_resource_having_same_def_new func_name iifunc1 []
                          blk_strtlineno lbl_list prog errblks_list init_lbl_list update_miss_rr_ops_list  in
                            if(List.length miss_rr_ops_list_new)> 0 then(
                              let miss_rr_ops_list1 = Rm_true_positives.is_rrwa_alloc errblks_list stmtlist 
                                                                 blk_strtlineno [] lbl_list prog miss_rr_ops_list_new  in
                                if(List.length miss_rr_ops_list1)> 0 then(
                                  let miss_rr_ops_list_new2  = Rm_true_positives.return_resource_new lbl_list stmtlist []  miss_rr_ops_list1 in
                                    if(List.length miss_rr_ops_list_new2)> 0 then(
                                      let miss_rr_ops_list_new3 = 
                                           Rm_true_positives.rls_in_exe_paths blk_strtlineno lbl_list prog errblks_list test_case 
					       stmtlist init_lbl_list [] miss_rr_ops_list_new2 in
                                         if(List.length miss_rr_ops_list_new3)> 0 then(
                                           let miss_rr_ops_list_new4 =  
                                              Interproc.find_interproc_calls stmtlist all_def  blk_strtlineno lbl_list prog [] miss_rr_ops_list_new3 in  
  					   let miss_rr_ops_list_new5 =  
                                            Interproc.interproc_new func all_def blk_strtlineno errblks_list prog lbl_list  [] miss_rr_ops_list_new4 in  
                                           let miss_rr_ops_list_new6 =  
					    Rm_true_positives.resource_is_not_allocated_yet errblks_list blk_strtlineno prog lbl_list [] miss_rr_ops_list_new5 in  
                                              if(List.length miss_rr_ops_list_new6)> 0 then(
                                                Report.generate_report_new func_name blk_strtlineno  iifunc1 miss_rr_ops_list_new6;

                                               analyze_each_blk_loop_new t
                                          )
                                          else analyze_each_blk_loop_new t
                                    )
                                    else analyze_each_blk_loop_new t
                                 )
                                 else analyze_each_blk_loop_new t
                          )
                          else analyze_each_blk_loop_new t
                       )
                       else analyze_each_blk_loop_new t
                 )
                 else analyze_each_blk_loop_new t
   in analyze_each_blk_loop_new errblks_list




         





let rec create_init_lbl_prog_list prog_list init_lbl_list  = function
    []-> (prog_list,init_lbl_list)
  |  h::t-> (match Ast_c.unwrap h with
              Ast_c.Labeled (Ast_c.Label (name, st)) ->if (List.length init_lbl_list)>0 then
                                                          (match Ast_c.unwrap st with
                                                          |      Ast_c.ExprStatement (Some e) ->
                                                                 create_init_lbl_prog_list prog_list (init_lbl_list@[st]) t
                                                          |      Ast_c.Jump (Ast_c.Goto name) -> (prog_list, (init_lbl_list@[st]))
                                                          |      Ast_c.Jump (Ast_c.Return) -> (prog_list, (init_lbl_list@[st]))
                                                          |      Ast_c.Jump (Ast_c.ReturnExpr e) -> (prog_list, (init_lbl_list@[st]))
                                                          |      Ast_c.Decl decl -> create_init_lbl_prog_list prog_list (init_lbl_list@[st]) t
                                                          |      _ -> (prog_list,[])
                                                          )
                                                       else (prog_list,init_lbl_list)

    |  Ast_c.ExprStatement (Some e) -> create_init_lbl_prog_list prog_list (init_lbl_list@[h]) t
    |  Ast_c.Jump (Ast_c.Goto name) -> (prog_list, (init_lbl_list@[h]))
    |  Ast_c.Jump (Ast_c.Return) -> (prog_list, (init_lbl_list@[h]))
    |  Ast_c.Jump (Ast_c.ReturnExpr e) -> (prog_list, (init_lbl_list@[h]))
    |  Ast_c.Decl decl -> create_init_lbl_prog_list prog_list (init_lbl_list@[h]) t
    |  _ -> create_init_lbl_prog_list (prog_list@init_lbl_list@[h]) [] t
  )




 
let  analyze_def full_file all_fn def =
    let defbis, ii = def in
    match ii with
    |    iifunc1::iifunc2::i1::i2::ifakestart::isto ->
        let {Ast_c.f_name = name;
             Ast_c.f_type = (returnt, (paramst, (b, iib)));
             Ast_c.f_storage = sto;
             Ast_c.f_body = statxs;
             Ast_c.f_attr = attrs;
            }    = defbis
            in
            let lbl_list = Def.create_lbl_list [] [] (Def.remove_stmtElelist statxs) in

         let (s,inf) = Ast_c.get_s_and_ii_of_name name in
         Var_dec.func_name:=s;
         Var_dec.file_info:=Ast_c.file_of_info iifunc1;

         let (prog_list,init_lbl_list)= create_init_lbl_prog_list [] [] (Def.remove_stmtElelist statxs)  in
         let prog = (Def.remove_stmtElelist statxs) in
        
         
         let new_errblks_list = Errorhandling.find_errorhandling lbl_list prog prog  in 
        
         let updated_blk_list = recheck_blks s iifunc1 lbl_list prog new_errblks_list in
         let updated_blk_list = Rm_true_positives.remove_blks_that_returns_resource prog updated_blk_list updated_blk_list in


         let (updated_blk_list, f_basic, f_goto) =
           let rec loop f_basic f_goto list = function
             []-> (list, f_basic, f_goto)
             |  (brnch_strtlineno,test_case,goto,st_normal,typ,blk_strtlineno,blk_endlineno,stmtlist)::t->
                      match goto with
                      |	 Some a -> loop f_basic true ((brnch_strtlineno,test_case,(* mender :  st_normal, *)typ,blk_strtlineno,blk_endlineno,stmtlist)::list) t
                      |	 _-> loop true f_goto ((brnch_strtlineno,test_case,(* mender :  st_normal, *) typ,blk_strtlineno,blk_endlineno,stmtlist)::list) t
           in loop false false [] updated_blk_list
         in


      
         let rr_ops_list_new = if (List.length updated_blk_list)>1 then (  
	                         create_rr_ops_list s iifunc1 lbl_list [] init_lbl_list updated_blk_list updated_blk_list ) 
	                       else [] in   


        let tmplist = 
           if (List.length updated_blk_list)>  1  && (List.length rr_ops_list_new)>0 then  (
             analyze_each_blk_new name lbl_list rr_ops_list_new prog iifunc1 !all_def s init_lbl_list updated_blk_list )
  	   else []  in     


        ( { defbis with Ast_c.f_body = List.map (function s -> Ast_c.StmtElem s)
            []
          } , ii )

    |    _ -> failwith "Impossible1"


let analyze_toplevel file  all_fn x  =
    match x with
        Ast_c.Declaration decl ->  x
    |   Ast_c.Definition def -> Ast_c.Definition (analyze_def file all_fn def)
    |   Ast_c.CppTop directive ->  x
    |   Ast_c.MacroTop (s, es,   [i1;i2;i3;i4]) -> x
    |   Ast_c.EmptyDef ii ->  x
    |   Ast_c.NotParsedCorrectly ii -> x
    |   Ast_c.FinalDef info -> x
    |   Ast_c.IfdefTop ifdefdir -> x
    |   Ast_c.MacroTop _ -> x
