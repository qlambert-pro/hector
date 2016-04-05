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

let built_in_constants = [
  "NULL";
  "FAILED";
  "FALSE";
  "HTTP_INTERNAL_SERVER_ERROR";
  "DECLINED";
  "HTTP_GATEWAY_TIME_OUT";
  "HTTP_FORBIDDEN";
  "HTTP_UNAUTHORIZED";
  "HTTP_BAD_REQUEST";
  "HTTP_REQUEST_ENTITY_TOO_LARGE";
  "HTTP_METHOD_NOT_ALLOWED";
  "HTTP_NOT_FOUND";
  "HTTP_NOT_IMPLEMENTED";
  "HTTP_TEMPORARY_REDIRECT";
  "HTTP_MOVED_TEMPORARILY";
  "HTTP_PRECONDITION_FAILED";
  "HTTP_NOT_MODIFIED";
  "HTTP_NO_CONTENT";
  "HTTP_BAD_GATEWAY";
  "MODSSL_ERROR_HTTP_ON_HTTPS";
  "HTTP_UPGRADE_REQUIRED";
  "HTTP_REQUEST_TIME_OUT";
  "HTTP_SERVICE_UNAVAILABLE";
  "HTTP_MOVED_PERMANENTLY";
  "HTTP_EXPECTATION_FAILED";
  "HTTP_MULTIPLE_CHOICES";
  "HTTP_NOT_ACCEPTABLE";
  "HTTP_VARIANT_ALSO_VARIES";
  "AUTHZ_DENIED_NO_USER";
  "AUTHZ_DENIED";
  "AUTHZ_GENERAL_ERROR";
  "AUTHZ_USER_NOT_FOUND";
  "AUTH_DENIED_NO_USER";
  "AUTH_DENIED";
  "AUTH_GENERAL_ERROR";
  "AUTH_USER_NOT_FOUND";
  "APR_EABOVEROOT";
  "APR_EABSOLUTE";
  "APR_EACCES";
  "APR_EAGAIN";
  "APR_EBADDATE";
  "APR_EBADF";
  "APR_EBADIP";
  "APR_EBADMASK";
  "APR_EBADPATH";
  "APR_EBUSY";
  "APR_ECONNABORTED";
  "APR_ECONNREFUSED";
  "APR_ECONNRESET";
  "APR_EDSOOPEN";
  "APR_EEXIST";
  "APR_EFTYPE";
  "APR_EGENERAL";
  "APR_EHOSTUNREACH";
  "APR_EINCOMPLETE";
  "APR_EINIT";
  "APR_EINPROGRESS";
  "APR_EINTR";
  "APR_EINVAL";
  "APR_EINVALSOCK";
  "APR_EMFILE";
  "APR_EMISMATCH";
  "APR_ENAMETOOLONG";
  "APR_END";
  "APR_ENETUNREACH";
  "APR_ENFILE";
  "APR_ENODIR";
  "APR_ENOENT";
  "APR_ENOLOCK";
  "APR_ENOMEM";
  "APR_ENOPOLL";
  "APR_ENOPOOL";
  "APR_ENOPROC";
  "APR_ENOSHMAVAIL";
  "APR_ENOSOCKET";
  "APR_ENOSPC";
  "APR_ENOSTAT";
  "APR_ENOTDIR";
  "APR_ENOTEMPTY";
  "APR_ENOTHDKEY";
  "APR_ENOTHREAD";
  "APR_ENOTIME";
  "APR_ENOTIMPL";
  "APR_ENOTSOCK";
  "APR_EOF";
  "APR_EPATHWILD";
  "APR_EPIPE";
  "APR_EPROC_UNKNOWN";
  "APR_ERELATIVE";
  "APR_ESPIPE";
  "APR_ESYMNOTFOUND";
  "APR_ETIMEDOUT";
  "APR_EXDEV";
  "TCL_ERROR";
  "DB_NOTFOUND";
  "DB_TIMEOUT";
  "WRC_Prune";
  "DB_TIMEOUT";
  "errno";
  "EABOVEROOT";
  "EABSOLUTE";
  "EACCES";
  "EAGAIN";
  "EBADDATE";
  "EBADF";
  "EBADIP";
  "EBADMASK";
  "EBADPATH";
  "EBUSY";
  "ERANGE";
  "EPERM";
  "ECONNABORTED";
  "ECONNREFUSED";
  "ECONNRESET";
  "EDSOOPEN";
  "EEXIST";
  "EFTYPE";
  "EGENERAL";
  "EHOSTUNREACH";
  "EINCOMPLETE";
  "EINIT";
  "EINPROGRESS";
  "EINTR";
  "EINVAL";
  "EINVALSOCK";
  "EMFILE";
  "EMISMATCH";
  "ENAMETOOLONG";
  "END";
  "ENETUNREACH";
  "ENFILE";
  "ENODIR";
  "ENOENT";
  "ENOLOCK";
  "ENOMEM";
  "ENOPOLL";
  "ENOPOOL";
  "ENOPROC";
  "ENOSHMAVAIL";
  "ENOSOCKET";
  "ENOSPC";
  "ENOSTAT";
  "ENOTDIR";
  "ENOTEMPTY";
  "ENOTHDKEY";
  "ENOTHREAD";
  "ENOTIME";
  "ENOTIMPL";
  "ENOTSOCK";
  "EOF";
  "EPATHWILD";
  "EPIPE";
  "EPROC_UNKNOWN";
  "ERELATIVE";
  "ESPIPE";
  "ESYMNOTFOUND";
  "ETIMEDOUT";
  "EXDEV";
  "SQLITE_IOERR";
  "SQLITE_CANTOPEN";
  "SQLITE_FULL";
  "SQLITE_ERROR";
  "SQLITE_IOERR_NOMEM";
  "SQLITE_MISUSE";
  "SQLITE_NOMEM";
  "SQLITE_LOCKED";
  "SQLITE_MISUSE_BKPT";
  "SQLITE_BUSY";
  "SQLITE_IOERR_LOCK";
  "SQLITE_IOERR_FSTAT";
  "SQLITE_IOERR_WRITE";
  "SQLITE_NOLFS";
  "SQLITE_MISUSE_BKPT";
  "SQLITE_CORRUPT_BKPT";
  "SQLITE_NOTFOUND";
  "SQLITE_TOOBIG";
  "SQLITE_CONSTRAINT";
  "SQLITE_IOERR_ACCESS";
  "SQLITE_CORRUPT";
  "SQLITE_DENY";
  "SQL_INVALID_HANDLE";
  "SQL_NO_DATA";
  "SQL_ERROR";
  "FAILURE";
  "False_";
  "False";
  "false";
  "EXIT_FAILURE";
  "EOF";
  "SIG_ERR";
  "HASH_KEY_IS_LONG";
  "HASH_KEY_IS_STRING";
  "HASH_KEY_NON_EXISTANT";
  "ZEND_HASH_APPLY_STOP";
  "ZEND_HASH_APPLY_KEEP";
  "ZEND_HASH_APPLY_REMOVE";
  "NAMESPACE_ERR";
  "PHP_ICONV_ERR_UNKNOWN";
  "PHP_ICONV_ERR_ALLOC";
  "PHP_ICONV_ERR_WRONG_CHARSET";
  "PHP_CONV_ERR_NOT_FOUND";
  "PHP_CONV_ERR_ALLOC";
  "PHP_CONV_ERR_UNEXPECTED_EOS";
  "PHP_CONV_ERR_TOO_BIG";
  "PSFS_ERR_FATAL";
  "PHP_FTP_MOREDATA";
  "PHP_FTP_FAILED";
  "SSB_FAIL";
  "EXIT_FAILURE";
  "STATUS_ERROR";
  "ECPG_INFORMIX_OUT_OF_MEMORY";
  "ECPG_INFORMIX_NUM_OVERFLOW";
  "ECPG_INFORMIX_NUM_UNDERFLOW";
  "ECPG_ARRAY_ERROR";
  "PXE_PGP_CORRUPT_DATA";
  "PXE_PGP_NOT_TEXT";
  "PXE_BUG";
  "PXE_PGP_UNSUPPORTED_COMPR";
  "PXE_PGP_COMPRESSION_ERROR";
  "PXE_PGP_UNSUPPORTED_CIPHER";
  "PXE_PGP_KEYPKT_CORRUPT";
  "PXE_PGP_WRONG_KEY";
  "PXE_PGP_CORRUPT_ARMOR";
  "DTERR_FIELD_OVERFLOW";
  "DTERR_BAD_FORMAT";
  "DTERR_MD_FIELD_OVERFLOW";
  "DTERR_TZDISP_OVERFLOW";
  "PGRES_POLLING_FAILED";
  "InvalidOid";
  "ERR_PTR";
  "PTR_ERR"]

let is_simple_assignment op =
  match unwrap op with
    SimpleAssign -> true
  | _ -> false

let rec is_pointer ((expression, info), _) =
  match expression with
    Cast (_, e)
  | ParenExpr e -> is_pointer e
  | _ ->
    match !info with
      (Some ((_, (Pointer _, _)), _), _) -> true
    | _ -> false

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

let get_assignment_type =
  let rec aux expression' =
    let ((expression, _), _) = expression' in
    match expression with
      Cast (_, e)
    | ParenExpr e -> aux e
    | Unary (_, UnMinus)
    | Unary ((((Constant (Int ("0", _))), _), _), Tilde) -> Value (Error Clear)
    | Ident (RegularName(s, _)) when List.exists ((=) s) built_in_constants ->
      Value (Error Clear)
    | Ident _ -> Variable expression'
    | FunCall _ -> Value (Error Ambiguous)
    | _ -> Value NonError
  in
  aux

let is_error_return_code alias_f e =
  match alias_f e with
    Error Clear -> true
  | _           -> false

let is_error_right_value alias_f e =
  match alias_f e with
    Error _ -> true
  | _       -> false


let expression_equal expression1 expression2 =
  Lib_parsing_c.real_al_expr expression1 = Lib_parsing_c.real_al_expr expression2

let apply_on_assignment f (expression, _) =
  match unwrap expression with
    Assignment (e1, op, e2)
    when is_simple_assignment op ->
    f e1 (Some e2)
  | FunCall (_, arguments') ->
    let arguments = expressions_of_arguments arguments' in
    let pointers = List.find_all is_pointer arguments in
    (match pointers with
       [p] -> f p None
     | _   -> ())
  | e -> ()

let apply_on_initialisation f declaration =
  match declaration.v_namei with
    Some (n, ValInit (_, (InitExpr e, _))) ->
    (*TODO create the correct type and infos*)
    f (mk_e (Ident n) []) (Some e)
  | _ -> ()

(*TODO None ..*)
type branch_side =
    Then
  | Else
  | None

let rec which_is_the_error_branch alias_f f (expression, _) =
  let not_f = function
      Then -> f Else
    | Else -> f Then
    | x    -> f x
  in
  match unwrap expression with
    ParenExpr e
  | Cast (_, e) -> which_is_the_error_branch alias_f f e
  | Unary (e, Not) -> which_is_the_error_branch alias_f not_f e
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
    when is_simple_assignment op && is_error_right_value alias_f e -> f Then
  | Ident _ -> f Then
  | _ -> f None

(*TODO does not handle complex if case with || or &&*)
let rec is_testing_identifier identifier expression' =
  let (expression, _) = expression' in
  match unwrap expression with
    ParenExpr e        -> is_testing_identifier identifier e
  | Unary (e, _)       -> expression_equal identifier e
  | Binary (e1, _, e2) -> expression_equal identifier e1 ||
                          expression_equal identifier e2
  | Assignment (_, op, e) when is_simple_assignment op ->
    expression_equal identifier e
  | _ -> expression_equal identifier expression'
