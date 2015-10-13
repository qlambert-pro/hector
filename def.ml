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


type block_part  =
     Then
  |  Else

type rank  =
      Hr
  |   Mr
  |   Lr
  |   No


(**************************************************************************  Clean Parsing parameter from objects *******************************************************)

(* Clean expression means remove extra parsing parameters from expression *)
let clean_exp  exp =  Lib_parsing_c.al_expr exp


(* Clean statement means remove extra parsing parameters from statement *)
let clean_stmt st =  Lib_parsing_c.real_al_statement st


(* Clean all expressions in the list means remove extra parsing parameters from the expression list*)
let rec clean_explist = function
    []-> []
  | h::t-> (clean_exp h)::(clean_explist t)

(* Clean all statements in the list means remove extra parsing parameters from the statement list*)
let rec clean_stmtlist  = function
    []-> []
  | h::t-> (clean_stmt h)::(clean_stmtlist t)



(**************************************************************************  Remove element from the statement  *******************************************************)

(* remove options from all elements in the list *)
let rec remove_optionlist = function
    []-> []
  | None::t -> failwith "unexpected None in remove_optionlist"
  | (Some h)::t -> h::(remove_optionlist t)


(* remove statement elements from all statements in the list *)
let rec remove_stmtElelist l =
  match l with
    []   -> []
  | h::t ->
    match h with
      Ast_c.StmtElem st -> st::(remove_stmtElelist t)
    | _ -> remove_stmtElelist t


(**************************************************************************  Add element into the statement  *******************************************************)


(* Add  Ast_c.StmtElem into all statements in the  list *)
let add_stmtElelist l=
    List.map (function st -> Ast_c.StmtElem st ) l


(* add options from all elements in the list *)
let rec add_optionlist = function
    []-> []
  | h::t-> (Some h)::(add_optionlist t)


(**************************************************************************  Compare two objects  *******************************************************)


(* Compare two names *)
let compare_names name1 name2 =
   Lib_parsing_c.al_name name1 = Lib_parsing_c.al_name name2

(* Compare two statements *)
let compare_stmts st1 st2 =
   if (clean_stmt st1) = (clean_stmt st2) then true
   else false

(* Compare two expressions *)
let compare_exps exp1 exp2 =
  (clean_exp exp1) = (clean_exp exp2)

(* Compare two  expression lists  *)
let compare_explists st1 st2 =
   if (clean_explist st1) = (clean_explist st2) then true
   else false




(**************************************************************************  Create Objects  ***********************************************************************)


(* Create statement list from compound statement *)
let create_stmtlist st =
  match Ast_c.unwrap st with
      Ast_c.Compound  st -> (remove_stmtElelist st)
  |    _ -> [st]


(* Whether return statement exists in the statement list *)
let rec return_exists_in_list = function
    []-> None
  |  h::t ->  match Ast_c.unwrap h with
               Ast_c.Jump (Ast_c.ReturnExpr e) -> Some h
            |  Ast_c.Jump (Ast_c.Return) -> Some h
            |  _ -> return_exists_in_list t



let rec return_exists_in_list_bool = function
    []-> false
  | h::t ->  match Ast_c.unwrap h with
               Ast_c.Jump (Ast_c.ReturnExpr e) -> true
           |   Ast_c.Jump (Ast_c.Return) -> true
           |   _ -> return_exists_in_list_bool t


(* Create lbl_list from existing label and out0 label *)
let rec create_lbl_list_loop current finished  st =
  match Ast_c.unwrap st  with
  |   Ast_c.Labeled (Ast_c.Label (name, st1)) -> (match Ast_c.unwrap st1 with
                                                   | Ast_c.Labeled (Ast_c.Label (name, st2)) ->
						       (((name,  [st2] )::(name,  [st2] )::((List.map (function (label,code ) -> (label, code@[st2])) current))), finished)
						   | Ast_c.Selection  (Ast_c.If (e, st11, st22)) -> 
						        if(return_exists_in_list_bool (create_stmtlist st11)) then(
							  (((name,  [st1] )::((List.map (function (label,code ) -> (label, code@[st1])) current))), finished))
							else (
						          (((name,  (create_stmtlist st11) )::((List.map (function (label,code ) -> 
							    (label, code@(create_stmtlist st11))) current))), finished))
                                                   | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
      failwith "for loop with declaration in first argument not supported"
                                                   | Ast_c.Iteration  (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st11)) ->
						       if(return_exists_in_list_bool (create_stmtlist st11)) then
							 (((name,  [st1] )::((List.map (function (label,code ) -> (label, code@[st1])) current))), finished)
						       else 
						         (((name,  (create_stmtlist st11) )::((List.map (function (label,code ) -> 
							   (label, code@(create_stmtlist st11))) current))), finished)
						   | _-> (((name,  [st1] )::((List.map (function (label,code ) -> (label, code@[st1])) current))), finished)
	                                         )
  |   Ast_c.Jump (Ast_c.Goto name) -> ([], (List.map (function (label,code)-> (label, code@[st])) current )@finished)
  |   Ast_c.Jump (Ast_c.ReturnExpr e) -> ([], (List.map (function (label,code)-> (label, code@[st])) current )@finished)
  |   Ast_c.Selection  (Ast_c.If (e, st11, st22)) -> if(return_exists_in_list_bool (create_stmtlist st11)) then(
                                                     ((List.map (function (label,code ) -> (label, code@[st])) current), finished))
	                                           else(
                                                     ((List.map (function (label,code ) -> (label, code@(create_stmtlist st11))) current), finished))
  | Ast_c.Iteration (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st1)) ->
      failwith "for loop with declaration in first argument not supported"
  | Ast_c.Iteration
      (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st1)) -> 

                                                   if(return_exists_in_list_bool (create_stmtlist st1)) then
						     ((List.map (function (label,code ) -> (label, code@[st])) current), finished)
						   else
                                                     ((List.map (function (label,code ) -> (label, code@(create_stmtlist st1))) current), finished)
  |   _ -> ((List.map (function (label,code ) -> (label, code@[st])) current), finished)




let rec create_lbl_list current finished = function
     [] ->  current@finished
  |  e::es -> let (current, finished) = create_lbl_list_loop current finished e
in create_lbl_list current finished es


(* Create an argument list *)
let rec create_argslist args_list statements =
  let create_argslist_aux statement acc =
    match Ast_c.unwrap statement with
      Common.Left es ->
      (match es with
         (((Ast_c.FunCall  (_, es1)), _), _) ->
         let new_args_list = create_argslist acc es1 in
         new_args_list
       | (((Ast_c.Cast (_, ((exp, typ), ii) )), _), _)
       | ((exp, typ), ii) ->
         acc@[Some ((exp, typ), ii)])
    | _ -> acc
  in
  List.fold_right create_argslist_aux statements []





(**************************************************************************  Boolean value ***********************************************************************)


(* Whether an expression is exists in an expression list *)
let rec exp_exists_in_list exp = function
    []   -> false
  | h::t ->
    if compare_exps exp h
    then true
    else exp_exists_in_list exp t


(* Whether a statement is exists in an statement list *)
let rec stmt_exists_in_list st = function
    [] -> false
  | h::t ->
    if compare_stmts st h
    then true
    else stmt_exists_in_list st t


let rec stmt_exists_in_all_list st = function
     []-> true
  |  h::t -> if(stmt_exists_in_list st h) then
               stmt_exists_in_all_list st t
             else false

(* Returns an expression present in both exps and the last argument *)
let which_exp_exists_in_list_new exps =
  let which_exp_exists_in_list_aux acc e =
    if exp_exists_in_list e exps
    then Some e
    else acc
  in
  List.fold_left which_exp_exists_in_list_aux None


let rec which_exp_exists_in_list_outer (arg, list) = function
    []           -> None
  | (arg1, b)::t ->
    if compare_exps arg arg1
    then
      match which_exp_exists_in_list_new list b with
        (Some (((Ast_c.FunCall (e, es)), typ), ii))
      | (Some (((Ast_c.Cast (_, (((Ast_c.FunCall  (e, es)), typ), ii))), _),
              _)) ->
        (Some (((Ast_c.FunCall  (e, es)), typ), ii))
      | _ -> None
    else which_exp_exists_in_list_outer (arg, list) t


let unique_id_values list1 list2 =
  let unique_id_values_aux acc id =
    let expression = which_exp_exists_in_list_outer id list2 in
    match expression with
      None   -> acc
    | Some e -> e::acc
  in
  List.fold_left unique_id_values_aux [] list1


let rec refine_id_list = function
    []-> []
  | (arg, b)::t ->
    if (List.length b) > 0
    then (arg, b)::(refine_id_list t)
    else refine_id_list t


(* Whether any statement is exists in an statement list *)

let rec any_stmt_exists_in_list stmts = function
    []-> false
  | h::t-> if(stmt_exists_in_list h stmts) then true
           else any_stmt_exists_in_list stmts t


(* Whether goto statement exists in the statement list *)

let rec goto_exists_in_list = function
    []-> None
  | h::t ->  match Ast_c.unwrap h with
                Ast_c.Jump (Ast_c.Goto name) -> Some h
             | _ -> goto_exists_in_list t









(* Whether expression exists in the statement *)

let exp_exists_in_stmt exp st =
  let flag = ref false in
  (Visitor_c.vk_statement
     { Visitor_c.default_visitor_c with
       Visitor_c.kexpr =
         (function (k,bigf) -> function e -> k e;
            match exp with
              None -> ()
            | Some a -> if (compare_exps e a) then flag := true else ())
     }
     st);
  !flag

(* Whether string exists in the statement *)
let is_string exp =
   match exp with
   | (((Ast_c.Constant (Ast_c.MultiString _)), typ), ii)->true
   | (((Ast_c.Constant (Ast_c.String _)), typ), ii)->true
   |_ -> false

let string_exists_in_stmt st =
  let flag = ref false in
  (Visitor_c.vk_statement
     {Visitor_c.default_visitor_c with
      Visitor_c.kexpr =
        (function (k, bigf) -> function exp -> k exp;
           if (is_string exp) then
             flag:=true
           else ()
        )}
     st);
  !flag

let rec string_exists_in_explist = function
     []-> false
   | h::t-> if (string_exists_in_stmt (Ast_c.ExprStatement (Some h),[])) then true else string_exists_in_explist t

let inner_block_in_compound_stmt st =
  let l = (create_stmtlist st) in
  let rec inner_block_in_compound_stmt_loop  = function
      []->  false
    | h::t ->  match Ast_c.unwrap h with
      | Ast_c.Labeled    (Ast_c.Label (name, st1)) -> true
      | Ast_c.Labeled    (Ast_c.Case  (e, st1)) -> true
      | Ast_c.Labeled    (Ast_c.CaseRange  (e, e2, st1)) -> true
      | Ast_c.Labeled    (Ast_c.Default st1) -> true
      | Ast_c.Selection  (Ast_c.If (e, st1, st2)) -> true
      | Ast_c.Selection  (Ast_c.Switch (e, st1 )) ->true
      | Ast_c.Iteration  (Ast_c.While (e, st1)) -> true
      | Ast_c.Iteration  (Ast_c.DoWhile (st1, e)) -> true
      | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _,(e2opt,il2),(e3opt, il3),st11)) ->
        failwith "for loop with declaration in first argument not supported"
      | Ast_c.Iteration  (Ast_c.For (Ast_c.ForExp(e1opt,il1),(e2opt,il2),(e3opt, il3),st1)) -> if (List.length (create_stmtlist st1))>1 then true
        else inner_block_in_compound_stmt_loop t
      | Ast_c.Iteration  (Ast_c.MacroIteration (s,es,st1)) -> true
      | _ -> inner_block_in_compound_stmt_loop t
  in inner_block_in_compound_stmt_loop l


let rec inner_blk = function
    []-> false
  | h::t ->  match Ast_c.unwrap h with
    | Ast_c.Labeled    (Ast_c.Case  _) -> true
    | Ast_c.Selection  (Ast_c.Switch _) -> true
    | Ast_c.Iteration  (Ast_c.For (Ast_c.ForDecl _, _, _, _)) ->
      failwith "for loop with declaration in first argument not supported"
    | Ast_c.Iteration  _ -> true
    | _ -> inner_blk t




type ptr = IsPtr | IsntPtr | UnknownType

let is_pointer ((exp, info), ii) =
   match !info with
     (Some((_,ty),_), _) ->
        (match Ast_c.unwrap ty with
            Ast_c.Pointer _ -> IsPtr
          | _ -> IsntPtr)
 |   (None,_)  -> UnknownType





let rec find_ptr_args_list = function
    []->[]
  | h::t->match  h with
      (((Ast_c.Cast    (cast, e)), typ), ii)->

      (match (is_pointer e) with
         IsPtr->
         e::find_ptr_args_list t
       |  UnknownType->
         e::find_ptr_args_list t
       |  _-> find_ptr_args_list t
      )
    | _-> (match (is_pointer h) with
          IsPtr->h::find_ptr_args_list t
        |	 UnknownType->h::find_ptr_args_list t
        |	 _-> find_ptr_args_list t
      )





(**************************************************************************  Find Value  ***********************************************************************)

(* Find start line number of the statement *)
let find_startline_no st =
    match st with
       []-> 0
    |  h::t-> let info = Lib_parsing_c.ii_of_stmt h in
              if (List.length info)>0 then(
              let (file,current_element,(start_line,_),(end_line,_))= Lib_parsing_c.lin_col_by_pos info in
		  start_line
              )
              else 0

(* Find end line number of the statement *)
let find_endline_no st =
    match List.rev st with
       []-> 0
    |  h::t-> let info = Lib_parsing_c.ii_of_stmt h in
              if (List.length info)>0 then(
              let (file,current_element,(start_line,_),(end_line,_))= Lib_parsing_c.lin_col_by_pos info in
                end_line
              )
              else 0

(* Temporary factoring *)
let code_for_goto name labels =
  try
    let (_, code) = List.find (fun (a, _) -> compare_names a name) labels in
    if (inner_blk code) then [] else code
  with
    Not_found -> []


let filter_empty_list_out xs =
  let is_empty l = l = []  in
  List.filter (fun l -> not (is_empty l)) xs

let boolean_of_option = function
    None -> false
  | Some _ -> true
