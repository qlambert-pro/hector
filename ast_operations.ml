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

open Ast_c

module StringPair =
struct
  type t = string * string
  let compare = compare
end

module StringSet     = Set.Make(String)
module StringPairSet = Set.Make(StringPair)

let error_constants     = ref StringSet.empty
let testing_functions   = ref StringSet.empty
let assigning_functions = ref StringSet.empty
let contained_fields    = ref StringPairSet.empty

let set_error_constants     s = error_constants     := s
let set_testing_functions   s = testing_functions   := s
let set_assigning_functions s = assigning_functions := s
let set_contained_fields    s = contained_fields    := s

let string_of_expression = Pretty_print_c.string_of_expression

let is_simple_assignment op =
  match unwrap op with
    SimpleAssign -> true
  | _ -> false

let is_pointer_type = function
    (_, (Pointer _, _)) -> true
  | _ -> false
let rec is_pointer exp =
  let ((expression, info), _) = exp in
  match expression with
    ParenExpr e -> is_pointer e
  | Unary (_, GetRef)
  | Cast ((_, (Pointer _, _)), _) -> true
  | Constant _ -> false
  | _ ->
    match !info with
      (None, _) -> true
    | (Some (ftype, _), _) -> is_pointer_type ftype

let expressions_of_arguments arguments =
  let expressions_of_arguments_aux acc argument =
    match unwrap argument with
      Common.Left e -> e::acc
    | _             -> acc
  in
  List.fold_left expressions_of_arguments_aux [] arguments

type error=
    Clear
  | Ambiguous

type value =
    NonError
  | Error of error

type assignment =
    Value of value
  | Variable of Ast_c.expression

let rec get_assignment_type expression' =
  let ((expression, _), _) = expression' in
  match expression with
    Cast (_, e)
  | ParenExpr e -> get_assignment_type e
  | Unary (_, UnMinus)
  | Unary ((((Constant (Int ("0", _))), _), _), Tilde) -> Value (Error Clear)
  | Ident (RegularName(s, _))
    when StringSet.exists ((=) s) !error_constants ->
    Value (Error Clear)
  | Ident _ -> Variable expression'
  | FunCall _ -> Value (Error Ambiguous)
  | _ -> Value NonError


let rec identifier_name_of_expression exp =
  let ((expression, _), _) = exp in
  match expression with
    Cast (_, e)
  | ArrayAccess (e, _)
  | Unary (e, DeRef)
  | ParenExpr e -> identifier_name_of_expression e
  | RecordAccess (_, RegularName (name, [_]))
  | RecordPtAccess (_, RegularName (name, [_]))
  | Ident (RegularName (name, [_])) ->
    Some name
  | _ -> None

let rec function_name_of_expression exp  =
  let ((expression, _), _) = exp in
  match expression with
    Cast (_, e)
  | RecordAccess (e ,_)
  | RecordPtAccess (e ,_)
  | ArrayAccess (e, _)
  | ParenExpr e -> function_name_of_expression e
  | Assignment (_, op, e) when is_simple_assignment op ->
    function_name_of_expression e
  | FunCall (name, _) ->
    identifier_name_of_expression name
  | _ -> None


let test_error_value alias_f f e =
  let error_type =
    match get_assignment_type e with
      Value e    -> e
    | Variable v -> alias_f v
  in
  f error_type

let is_error_return_code alias_f e =
  let test e =
    match e with
      Error Clear -> true
    | _           -> false
  in
  test_error_value alias_f test e

let is_error_right_value alias_f e =
  let test e =
    match e with
      Error _ -> true
    | _       -> false
  in
  test_error_value alias_f test e


let expression_equal expression1 expression2 =
  Lib_parsing_c.real_al_expr expression1 = Lib_parsing_c.real_al_expr expression2

let expression_compare expression1 expression2 =
  String.compare
    (string_of_expression expression1)
    (string_of_expression expression2)

let statement_equal st1 st2 =
  Lib_parsing_c.real_al_statement st1 = Lib_parsing_c.real_al_statement st2

module OrderedExpression =
struct
  type t = expression
  let compare e1 e2 = compare (Lib_parsing_c.real_al_expr e1)
      (Lib_parsing_c.real_al_expr e2)
end

module ExpressionSet = Set.Make(OrderedExpression)


let rec get_arguments expression =
  match unwrap (unwrap expression) with
    Cast (_, e)
  | ParenExpr e -> get_arguments e
  | Assignment (_, op, e) when is_simple_assignment op -> get_arguments e
  | FunCall (e, arguments)
    when not (StringSet.exists
                (fun x -> (=) (identifier_name_of_expression e) (Some x))
                !testing_functions) ->
    Some (expressions_of_arguments arguments)
  | _ -> None

let rename_name n s =
  match n with
    RegularName (_, info_old') ->
    let info_old = List.map (rewrap_str s) info_old' in
    RegularName (s, info_old)
  | _ -> n

let rename_ident expression s =
  let ((exp, info1), info2) = expression in
  match exp with
    Ident n -> ((Ident (rename_name n s), info1), info2)
  | _ -> expression

let unify_array_access expression =
  let ((exp, info1), info2) = expression in
  match exp with
    ArrayAccess (e, s) ->
    ((ArrayAccess (e, rename_ident s "token_index"), info1), info2)
  | _ -> expression

let resources_of_arguments = function
    Some xs ->
    let resources = List.find_all is_pointer xs in
    List.map unify_array_access resources
  | None    -> []

let is_string e =
  match unwrap (unwrap e) with
    StringConstant _
  | Constant (String _)
  | Constant (MultiString _) -> true
  | _ -> false

let apply_on_assignment f (expression, _) =
  match unwrap expression with
    Assignment (e1, op, e2) -> f e1 op e2
  | e -> ()

let apply_on_funcall_side_effect f (expression, _) =
  match unwrap expression with
  | FunCall (_, arguments') ->
    let arguments = expressions_of_arguments arguments' in
    let pointers = List.find_all is_pointer arguments in
    (match pointers with
       [p] -> f p
     | _   -> ())
  | e -> ()

let apply_on_initialisation f declaration =
  match declaration.v_namei with
    Some (n, ValInit (_, (InitExpr e, _))) ->
    f (mk_e_bis (Ident n)
         (ref (Some (declaration.v_type, NotLocalVar), NotTest)) [])
      (SimpleAssign, []) e
  | _ -> ()

type branch_side =
    Then
  | Else
  | Neither

let rec which_is_the_error_branch alias_f f e =
  let (expression, _) = e in
  let not_f = function
      Then -> f Else
    | Else -> f Then
    | x    -> f x
  in
  let which_branch e =
    if is_pointer e
    then f Else
    else f Then
  in
  match unwrap expression with
    ParenExpr e
  | Cast (_, e) -> which_is_the_error_branch alias_f f e
  | Unary (e, Not) ->
    which_is_the_error_branch alias_f not_f e
  | Binary (e1, (Logical OrLog, _), e2) -> which_is_the_error_branch alias_f f e1
  | Binary (e1, (Logical Eq   , _), e2)
    when is_error_return_code alias_f e1 || is_error_return_code alias_f e2 ->
    f Then
  | Binary (e1, (Logical NotEq, _), e2)
    when is_error_return_code alias_f e1 || is_error_return_code alias_f e2 -> f Else
  | Binary (e1, (Logical Eq   , _), e2)
    when not (is_error_return_code alias_f e1 || is_error_return_code alias_f e2) ->
    f Else
  | Binary (e1, (Logical NotEq, _),e2)
    when not (is_error_return_code alias_f e1 || is_error_return_code alias_f e2) ->
    f Then
  | Binary (e1, (Logical Inf  , _), ((Constant (Int ("0", _)), _), _)) ->
    f Then
  | FunCall _ -> f Then
  | Assignment (_, op, e)
    when is_simple_assignment op && is_error_right_value alias_f e ->
    f Then
  | RecordAccess _
  | RecordPtAccess _
  | ArrayAccess _
  | Unary (_, DeRef)
  | Ident _ ->
    which_branch e
  | _ -> f Neither

let rec is_testing_identifier identifier expression' =
  let (expression, _) = expression' in
  match unwrap expression with
    ParenExpr e        -> is_testing_identifier identifier e
  | Unary (e, _)       -> expression_equal identifier e ||
                          is_testing_identifier identifier e
  | Binary (e1, _, e2) -> expression_equal identifier e1 ||
                          expression_equal identifier e2 ||
                          is_testing_identifier identifier e1 ||
                          is_testing_identifier identifier e2
  | Assignment (_, op, e) when is_simple_assignment op ->
    expression_equal identifier e
  | FunCall (e, args) ->
    let arguments = expressions_of_arguments args in
    let n = identifier_name_of_expression e in
    StringSet.exists (fun e -> (=) n (Some e)) !testing_functions &&
    List.exists (is_testing_identifier identifier) arguments
  | _ -> expression_equal identifier expression'

let string_of_name = function
    RegularName (n, _) -> n
  | CppConcatenatedName _
  | CppVariadicName _
  | CppIdentBuilder _ -> "error: cpp stuff"

let get_definition_name d = string_of_name d.Ast_c.f_name

type 'a computation =
    ToplevelAndInfo of (Ast_c.toplevel -> Ast_c.info -> 'a)
  | Defbis of (Ast_c.definitionbis -> 'a)

let apply_if_function_definition f x default =
  match x with
    Ast_c.Definition (defbis, infos::_) ->
    (match f with
       ToplevelAndInfo f -> f x infos
     | Defbis f -> f defbis)
  | Ast_c.Definition _
  | Ast_c.Declaration _
  | Ast_c.CppTop _
  | Ast_c.Namespace _
  | Ast_c.EmptyDef _
  | Ast_c.NotParsedCorrectly _
  | Ast_c.FinalDef _
  | Ast_c.IfdefTop _
  | Ast_c.MacroTop _ -> default

let get_name d =
  apply_if_function_definition (Defbis get_definition_name) d "unknown name"

let expression_of_parameter (p, info) =
  let exp_info =
    ref (Some (p.Ast_c.p_type, Ast_c.NotLocalVar), Ast_c.NotTest)
  in
  match p.Ast_c.p_namei with
    Some n -> Some ((Ast_c.Ident n, exp_info), info)
  | _ -> None

