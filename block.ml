open Common

(* TODO These should be reimplemented with records and losing the constructors *)
type block = Block of int * Ast_c.expression *
                      Ast_c.statement option * Ast_c.statement list *
                      Def.block_part * int * int * Ast_c.statement list
           | BlockSimple of int * Ast_c.statement list

let mk_block block_start test_case goto st_normal part branch_start branch_end
    statements =
  Block (block_start, test_case, goto, st_normal, part, branch_start,
         branch_end, statements)

let mk_block_simple branch_start statements = BlockSimple (branch_start, statements)

let extract (Block (block_start, test_case, goto, st_normal, part, branch_start,
                    branch_end, statements)) = (block_start, test_case,
                                                part, branch_start,
                                                branch_end, statements)

let extract_statements (Block (_, _, _, _, _, _, _, statements)) =
  statements

let extract_part (Block (_, _, _, _, part, _, _, _)) =
  part

let extract_branch_start = function
    Block (_, _, _, _, _, branch_start, _, _)
  | BlockSimple (branch_start, _) ->
    branch_start

let extract_branch_end (Block (_, _, _, _, _, _, branch_end, _)) =
  branch_end

let extract_test_case (Block (_, test_case, _, _, _, _, _, _)) =
  test_case

let compare_branch_start block1 block2 =
  let (branch_start1, branch_start2) =
    match (block1, block2) with
      ((Block (_, _, _, _, _, branch_start1, _, _)),
       (Block (_, _, _, _, _, branch_start2, _, _)))
    | ((Block (_, _, _, _, _, branch_start1, _, _)),
       (BlockSimple (branch_start2, _)))
    | ((BlockSimple (branch_start1, _)),
       (Block (_, _, _, _, _, branch_start2, _, _)))
    | ((BlockSimple (branch_start1, _)),
       (BlockSimple (branch_start2, _))) ->
      (branch_start1, branch_start2)
  in
  branch_start1 - branch_start2

let compare_branch_end_with_start block1 block2 =
  let (branch_end, branch_start) =
    match (block1, block2) with
      ((Block (_, _, _, _, _, _, branch_end1, _)),
       (Block (_, _, _, _, _, branch_start2, _, _)))
    | ((Block (_, _, _, _, _, branch_end1, _, _)),
       (BlockSimple (branch_start2, _)))
    | ((BlockSimple (branch_end1, _)),
       (Block (_, _, _, _, _, branch_start2, _, _)))
    | ((BlockSimple (branch_end1, _)),
       (BlockSimple (branch_start2, _))) ->
      (branch_end1, branch_start2)
  in
  branch_end - branch_start


let has_goto (Block (_, _, goto, _, _, _, _, _)) =
  match goto with
    Some _ -> true
  | None   -> false


let rec release_by_address arg = function
    []-> false
  | h::t->
    match h with
    | (((Ast_c.Unary (e, Ast_c.GetRef)), typ), ii)->
      if (Def.compare_exps arg e) then true
      else
        (
          match e with
            ((( Ast_c.RecordAccess   (e, name)), typ1), ii1)->
            if(Def.compare_exps arg e) then true
            else release_by_address arg t
          | ((( Ast_c.RecordPtAccess   (e, name)), typ1), ii1)->
            if(Def.compare_exps arg e) then true
            else release_by_address arg t
          | _->release_by_address arg t
        )
    | _-> release_by_address arg t

let contains_expression (Block (_, _, _, _, _, _, _, statements)) arg =
  let contains_expression_aux h =
    match Ast_c.unwrap h with
      Ast_c.ExprStatement (Some ((( Ast_c.FunCall  (e, es)), typ), ii)) ->
      if not (Def.string_exists_in_stmt h)
      then
        let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
        Def.exp_exists_in_list arg args_list ||
        release_by_address arg args_list
      else false
    | Ast_c.ExprStatement (Some (((Ast_c.Assignment (e1, op, e2)), typ), ii)) ->
      Def.compare_exps arg e1
    | _ -> false
  in
  List.exists contains_expression_aux statements


let does_block_contains_expression (Block (_, _, _, _, _, _, _, statements))
    return_val =
  let rec does_block_contains_expression_aux = function
      []-> false
    | h::t->
      match Ast_c.unwrap h with
        Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
        let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
        if (Def.exp_exists_in_list return_val args_list) then true
        else does_block_contains_expression_aux t
      | _ -> does_block_contains_expression_aux t
  in
  does_block_contains_expression_aux statements


let expression_used_as_argument (Block (_, _, _, _, _, _, _, statements))
    return_val =
  let rec expression_used_as_argument_aux = function
      []-> false
    | h::t->
      match Ast_c.unwrap h with
        Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii))->
        let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
        if (Def.exp_exists_in_list return_val args_list) then true
        else expression_used_as_argument_aux t
      | _ -> expression_used_as_argument_aux t
  in
  expression_used_as_argument_aux statements


let does_block_contains_statement (Block (_, _, _, _, _, _, _, statements))
    statement =
  Def.stmt_exists_in_list statement statements

let return_built_in_constant id =
  if id =~ "NULL" then false
  else if id =~ "FAILED" then false
  else if id =~ "FALSE" then false
  else if id =~ "HTTP_INTERNAL_SERVER_ERROR" then false
  else if id =~ "DECLINED" then true
  else if id =~ "HTTP_GATEWAY_TIME_OUT" then false
  else if id =~ "HTTP_FORBIDDEN" then false
  else if id =~ "HTTP_UNAUTHORIZED" then false
  else if id =~ "HTTP_BAD_REQUEST" then false
  else if id =~ "HTTP_REQUEST_ENTITY_TOO_LARGE" then false
  else if id =~ "HTTP_METHOD_NOT_ALLOWED" then false
  else if id =~ "HTTP_NOT_FOUND" then false
  else if id =~ "HTTP_NOT_IMPLEMENTED" then false
  else if id =~ "HTTP_TEMPORARY_REDIRECT" then false
  else if id =~ "HTTP_MOVED_TEMPORARILY" then false
  else if id =~ "HTTP_PRECONDITION_FAILED" then false
  else if id =~ "HTTP_NOT_MODIFIED" then false
  else if id =~ "HTTP_NO_CONTENT" then false
  else if id =~ "HTTP_BAD_GATEWAY" then false
  else if id =~ "MODSSL_ERROR_HTTP_ON_HTTPS" then false
  else if id =~ "HTTP_UPGRADE_REQUIRED" then false
  else if id =~ "HTTP_REQUEST_TIME_OUT" then false
  else if id =~ "HTTP_SERVICE_UNAVAILABLE" then true
  else if id =~ "HTTP_MOVED_PERMANENTLY" then false
  else if id =~ "HTTP_EXPECTATION_FAILED" then false
  else if id =~ "HTTP_MULTIPLE_CHOICES" then false
  else if id =~ "HTTP_NOT_ACCEPTABLE" then false
  else if id =~ "HTTP_VARIANT_ALSO_VARIES" then false
  else if id =~ "AUTHZ_DENIED_NO_USER" then false
  else if id =~ "AUTHZ_DENIED" then false
  else if id =~ "AUTHZ_GENERAL_ERROR" then false
  else if id =~ "AUTHZ_USER_NOT_FOUND" then false
  else if id =~ "AUTH_DENIED_NO_USER" then false
  else if id =~ "AUTH_DENIED" then false
  else if id =~ "AUTH_GENERAL_ERROR" then false
  else if id =~ "AUTH_USER_NOT_FOUND" then false
  else if id =~ "APR_EABOVEROOT" then false
  else if id =~ "APR_EABSOLUTE" then false
  else if id =~ "APR_EACCES" then false
  else if id =~ "APR_EAGAIN" then false
  else if id =~ "APR_EBADDATE" then false
  else if id =~ "APR_EBADF" then false
  else if id =~ "APR_EBADIP" then false
  else if id =~ "APR_EBADMASK" then false
  else if id =~ "APR_EBADPATH" then false
  else if id =~ "APR_EBUSY" then false
  else if id =~ "APR_ECONNABORTED" then false
  else if id =~ "APR_ECONNREFUSED" then false
  else if id =~ "APR_ECONNRESET" then false
  else if id =~ "APR_EDSOOPEN" then false
  else if id =~ "APR_EEXIST" then false
  else if id =~ "APR_EFTYPE" then false
  else if id =~ "APR_EGENERAL" then false
  else if id =~ "APR_EHOSTUNREACH" then false
  else if id =~ "APR_EINCOMPLETE" then false
  else if id =~ "APR_EINIT" then false
  else if id =~ "APR_EINPROGRESS" then false
  else if id =~ "APR_EINTR" then false
  else if id =~ "APR_EINVAL" then false
  else if id =~ "APR_EINVALSOCK" then false
  else if id =~ "APR_EMFILE" then false
  else if id =~ "APR_EMISMATCH" then false
  else if id =~ "APR_ENAMETOOLONG" then false
  else if id =~ "APR_END" then false
  else if id =~ "APR_ENETUNREACH" then false
  else if id =~ "APR_ENFILE" then false
  else if id =~ "APR_ENODIR" then false
  else if id =~ "APR_ENOENT" then false
  else if id =~ "APR_ENOLOCK" then false
  else if id =~ "APR_ENOMEM" then false
  else if id =~ "APR_ENOPOLL" then false
  else if id =~ "APR_ENOPOOL" then false
  else if id =~ "APR_ENOPROC" then false
  else if id =~ "APR_ENOSHMAVAIL" then false
  else if id =~ "APR_ENOSOCKET" then false
  else if id =~ "APR_ENOSPC" then false
  else if id =~ "APR_ENOSTAT" then false
  else if id =~ "APR_ENOTDIR" then false
  else if id =~ "APR_ENOTEMPTY" then false
  else if id =~ "APR_ENOTHDKEY" then false
  else if id =~ "APR_ENOTHREAD" then false
  else if id =~ "APR_ENOTIME" then false
  else if id =~ "APR_ENOTIMPL" then false
  else if id =~ "APR_ENOTSOCK" then false
  else if id =~ "APR_EOF" then false
  else if id =~ "APR_EPATHWILD" then false
  else if id =~ "APR_EPIPE" then false
  else if id =~ "APR_EPROC_UNKNOWN" then false
  else if id =~ "APR_ERELATIVE" then false
  else if id =~ "APR_ESPIPE" then false
  else if id =~ "APR_ESYMNOTFOUND" then false
  else if id =~ "APR_ETIMEDOUT" then false
  else if id =~ "APR_EXDEV" then false
  else if id =~ "TCL_ERROR" then false
  else if id =~ "DB_NOTFOUND" then false
  else if id =~ "DB_TIMEOUT" then false
  else if id =~ "WRC_Prune" then false
  else if id =~ "DB_TIMEOUT" then false
  else if id =~ "errno" then false
  else if id =~ "EABOVEROOT" then false
  else if id =~ "EABSOLUTE" then false
  else if id =~ "EACCES" then false
  else if id =~ "EAGAIN" then false
  else if id =~ "EBADDATE" then false
  else if id =~ "EBADF" then false
  else if id =~ "EBADIP" then false
  else if id =~ "EBADMASK" then false
  else if id =~ "EBADPATH" then false
  else if id =~ "EBUSY" then false
  else if id =~ "ERANGE" then false
  else if id =~ "EPERM" then false
  else if id =~ "ECONNABORTED" then false
  else if id =~ "ECONNREFUSED" then false
  else if id =~ "ECONNRESET" then false
  else if id =~ "EDSOOPEN" then false
  else if id =~ "EEXIST" then false
  else if id =~ "EFTYPE" then false
  else if id =~ "EGENERAL" then false
  else if id =~ "EHOSTUNREACH" then false
  else if id =~ "EINCOMPLETE" then false
  else if id =~ "EINIT" then false
  else if id =~ "EINPROGRESS" then false
  else if id =~ "EINTR" then false
  else if id =~ "EINVAL" then false
  else if id =~ "EINVALSOCK" then false
  else if id =~ "EMFILE" then false
  else if id =~ "EMISMATCH" then false
  else if id =~ "ENAMETOOLONG" then false
  else if id =~ "END" then false
  else if id =~ "ENETUNREACH" then false
  else if id =~ "ENFILE" then false
  else if id =~ "ENODIR" then false
  else if id =~ "ENOENT" then false
  else if id =~ "ENOLOCK" then false
  else if id =~ "ENOMEM" then false
  else if id =~ "ENOPOLL" then false
  else if id =~ "ENOPOOL" then false
  else if id =~ "ENOPROC" then false
  else if id =~ "ENOSHMAVAIL" then false
  else if id =~ "ENOSOCKET" then false
  else if id =~ "ENOSPC" then false
  else if id =~ "ENOSTAT" then false
  else if id =~ "ENOTDIR" then false
  else if id =~ "ENOTEMPTY" then false
  else if id =~ "ENOTHDKEY" then false
  else if id =~ "ENOTHREAD" then false
  else if id =~ "ENOTIME" then false
  else if id =~ "ENOTIMPL" then false
  else if id =~ "ENOTSOCK" then false
  else if id =~ "EOF" then false
  else if id =~ "EPATHWILD" then false
  else if id =~ "EPIPE" then false
  else if id =~ "EPROC_UNKNOWN" then false
  else if id =~ "ERELATIVE" then false
  else if id =~ "ESPIPE" then false
  else if id =~ "ESYMNOTFOUND" then false
  else if id =~ "ETIMEDOUT" then false
  else if id =~ "EXDEV" then false
  else if id =~ "SQLITE_IOERR" then false
  else if id =~ "SQLITE_CANTOPEN" then false
  else if id =~ "SQLITE_FULL" then false
  else if id =~ "SQLITE_ERROR" then false
  else if id =~ "SQLITE_IOERR_NOMEM" then false
  else if id =~ "SQLITE_MISUSE" then false
  else if id =~ "SQLITE_NOMEM" then false
  else if id =~ "SQLITE_LOCKED" then false
  else if id =~ "SQLITE_MISUSE_BKPT" then false
  else if id =~ "SQLITE_BUSY" then false
  else if id =~ "SQLITE_IOERR_LOCK" then false
  else if id =~ "SQLITE_IOERR_FSTAT" then false
  else if id =~ "SQLITE_IOERR_WRITE" then false
  else if id =~ "SQLITE_NOLFS" then false
  else if id =~ "SQLITE_MISUSE_BKPT" then false
  else if id =~ "SQLITE_CORRUPT_BKPT" then false
  else if id =~ "SQLITE_NOTFOUND" then false
  else if id =~ "SQLITE_TOOBIG" then false
  else if id =~ "SQLITE_CONSTRAINT" then false
  else if id =~ "SQLITE_IOERR_ACCESS" then false
  else if id =~ "SQLITE_CORRUPT" then false
  else if id =~ "SQLITE_DENY" then false
  else if id =~ "SQL_INVALID_HANDLE" then false
  else if id =~ "SQL_NO_DATA" then false
  else if id =~ "SQL_ERROR" then false
  else if id =~ "FAILURE" then false
  else if id =~ "False_" then false
  else if id =~ "False" then false
  else if id =~ "false" then false
  else if id =~ "EXIT_FAILURE" then false
  else if id =~ "EOF" then false
  else if id =~ "SIG_ERR" then false
  else if id =~ "HASH_KEY_IS_LONG" then false
  else if id =~ "HASH_KEY_IS_STRING" then false
  else if id =~ "HASH_KEY_NON_EXISTANT" then false
  else if id =~ "ZEND_HASH_APPLY_STOP" then false
  else if id =~ "ZEND_HASH_APPLY_KEEP" then false
  else if id =~ "ZEND_HASH_APPLY_REMOVE" then false
  else if id =~ "NAMESPACE_ERR" then false
  else if id =~ "PHP_ICONV_ERR_UNKNOWN" then false
  else if id =~ "PHP_ICONV_ERR_ALLOC" then false
  else if id =~ "PHP_ICONV_ERR_WRONG_CHARSET" then false
  else if id =~ "PHP_CONV_ERR_NOT_FOUND" then false
  else if id =~ "PHP_CONV_ERR_ALLOC" then false
  else if id =~ "PHP_CONV_ERR_UNEXPECTED_EOS" then false
  else if id =~ "PHP_CONV_ERR_TOO_BIG" then false
  else if id =~ "PSFS_ERR_FATAL" then false
  else if id =~ "PHP_FTP_MOREDATA" then false
  else if id =~ "PHP_FTP_FAILED" then false
  else if id =~ "SSB_FAIL" then false
  else if id =~ "EXIT_FAILURE" then false
  else if id =~ "STATUS_ERROR" then false
  else if id =~ "ECPG_INFORMIX_OUT_OF_MEMORY" then false
  else if id =~ "ECPG_INFORMIX_NUM_OVERFLOW" then false
  else if id =~ "ECPG_INFORMIX_NUM_UNDERFLOW" then false
  else if id =~ "ECPG_ARRAY_ERROR" then false
  else if id =~ "PXE_PGP_CORRUPT_DATA" then false
  else if id =~ "PXE_PGP_NOT_TEXT" then false
  else if id =~ "PXE_BUG" then false
  else if id =~ "PXE_PGP_UNSUPPORTED_COMPR" then false
  else if id =~ "PXE_PGP_COMPRESSION_ERROR" then false
  else if id =~ "PXE_PGP_UNSUPPORTED_CIPHER" then false
  else if id =~ "PXE_PGP_KEYPKT_CORRUPT" then false
  else if id =~ "PXE_PGP_WRONG_KEY" then false
  else if id =~ "PXE_PGP_CORRUPT_ARMOR" then false
  else if id =~ "DTERR_FIELD_OVERFLOW" then false
  else if id =~ "DTERR_BAD_FORMAT" then false
  else if id =~ "DTERR_MD_FIELD_OVERFLOW" then false
  else if id =~ "DTERR_TZDISP_OVERFLOW" then false
  else if id =~ "PGRES_POLLING_FAILED" then false
  else if id =~ "InvalidOid" then false
  else true


let rec is_error_code_test ee =
  match ee with

  | (((Ast_c.Binary (e1, Ast_c.Logical Ast_c.NotEq, (((Ast_c.Constant (Ast_c.Int ("0", _))), typ2), ii2))), typ), ii) ->
    (match (Def.is_pointer e1) with
       Def.IsntPtr -> Some true
     | _           -> None)

  | (((Ast_c.Binary (e1, Ast_c.Logical Ast_c.SupEq, (((Ast_c.Constant (Ast_c.Int ("0", _))), typ2), ii2))), typ), ii)
  |  (((Ast_c.Binary (e1, Ast_c.Logical Ast_c.Eq, (((Ast_c.Constant (Ast_c.Int ("0", _))), typ2), ii2))), typ), ii) ->
    (match (Def.is_pointer e1) with
       Def.IsntPtr -> None
     | _           -> Some true)

  | (((Ast_c.Binary (e1, (Ast_c.Logical Ast_c.OrLog), e2)), typ), ii) ->
    (match (is_error_code_test e1, is_error_code_test e2) with
       (Some true, _)
     | (_, Some true) -> Some true
     | (None, _)
     | (_, None)      -> None
     | _              -> Some false)

  | (((Ast_c.Binary (e1, (Ast_c.Logical Ast_c.AndLog), e2)), typ), ii) ->
    (match (is_error_code_test e1, is_error_code_test e2) with
       (Some true, Some true) -> Some true
     | (None, _)
     | (_, None)      -> None
     | _              -> Some false)

  | (((Ast_c.Binary (e1, (Ast_c.Arith Ast_c.And), e2)), typ), ii)
  | (((Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Or), e2)), typ), ii) -> None

  | (((Ast_c.FunCall ((((Ast_c.Ident (Ast_c.RegularName ("IS_ERR", _))), _), _),
                      _)), _), _)
  | (((Ast_c.Binary (_, Ast_c.Logical Ast_c.Eq , (((Ast_c.Unary (_, Ast_c.Not)),
                                                   _), _))), _), _)
  | (((Ast_c.Binary (_, Ast_c.Logical Ast_c.Eq , (((Ast_c.Unary (_,
                                                                 Ast_c.UnMinus)),
                                                   _), _))), _), _) -> Some true

  | (((Ast_c.Unary  (e1, Ast_c.Not)), typ), ii) ->
    (match (Def.is_pointer e1) with
       Def.IsntPtr -> Some false
     | _           -> Some true)

  | (((Ast_c.Binary (_, (Ast_c.Logical Ast_c.Eq), ((Ast_c.Ident
                                                      (Ast_c.RegularName("NULL",_)),
                                                    _), _))), _), _)
  | (((Ast_c.Binary (_, Ast_c.Logical Ast_c.Inf, (((Ast_c.Constant (Ast_c.Int
                                                                      ("0",
                                                                       _))), _),
                                                  _))), _), _) -> Some true

  | (((Ast_c.Ident (ident)), typ), ii) ->
    (match Def.is_pointer (((Ast_c.Ident (ident)), typ), ii) with
       Def.IsntPtr -> Some true
     | _           -> None)

  | _ ->  Some false

let is_error_handling_block is_assigned_error_code
    (Block (_, exp, _, _, part, blk_strtlineno, _, stmtlist)) =
  match Def.return_exists_in_list stmtlist with
    None    -> false
  | Some st ->
    let return_expr =
      match Ast_c.unwrap st with
        Ast_c.Jump (Ast_c.ReturnExpr (e, _)) -> Some e
      | _ -> None
    in
    (* Return true if expr has been assigned an error return code or
     * if we are on the error branch of a test_case *)
    let is_error_handling_block_aux fin_lineno expr =
      is_assigned_error_code fin_lineno expr ||
      (match part with
         Def.Then ->
         (match is_error_code_test exp with
            Some true -> true
          | _         -> false)

       | Def.Else ->
         (match is_error_code_test exp with
            Some _ -> false
          | _      -> true))
    in
    (match return_expr with
       Some ((Ast_c.Ident (Ast_c.RegularName(id, ii))), typ) ->
       if (not (return_built_in_constant id))
       then true
       else
         let fin_lineno = blk_strtlineno in
         let expr = (((Ast_c.Ident (Ast_c.RegularName(id, ii))), typ), []) in
         is_error_handling_block_aux fin_lineno expr

     | Some ((Ast_c.ParenExpr ((((Ast_c.Ident (Ast_c.RegularName(id, ii))),
                                 _), _))), _) ->
       if (not (return_built_in_constant id))
       then true
       else
         let fin_lineno = Def.find_startline_no (Def.create_stmtlist st) in
         let expr = (((Ast_c.Ident (Ast_c.RegularName(id, ii))),
                      Ast_c.noType ()), []) in
         is_error_handling_block_aux fin_lineno expr

     | Some ((Ast_c.Cast (_, (((Ast_c.ParenExpr ((((Ast_c.Ident ident),
                                                   _), _))), _), _))), _)
     | Some ((Ast_c.Cast (_, (((Ast_c.Ident ident), _), _))), _) ->
       let fin_lineno = Def.find_startline_no (Def.create_stmtlist st) in
       let expr = (((Ast_c.Ident ident), Ast_c.noType ()), []) in
       is_error_handling_block_aux fin_lineno expr

     | _ -> true)


(*
let is_error_handling_block (Block (_, exp, _, _, part, _, _, stmtlist)) =
  (match part with
     Def.Then ->
     (match is_error_code_test exp with
        Some true -> true
      | _         -> false)

   | Def.Else ->
     (match is_error_code_test exp with
        Some _ -> false
      | _      -> true))
  ||
  (match Def.return_exists_in_list stmtlist with
     None    -> false
   | Some st ->
     let return_expr =
       match Ast_c.unwrap st with
         Ast_c.Jump (Ast_c.ReturnExpr (e, _)) -> Some e
       | _ -> None
     in
     (match return_expr with
        Some ((Ast_c.Ident (Ast_c.RegularName(id, _))), _)
      | Some ((Ast_c.ParenExpr ((((Ast_c.Ident (Ast_c.RegularName(id, _))),
                                  _), _))), _) ->
        not (return_built_in_constant id)

      | Some ((Ast_c.Cast (_, (((Ast_c.ParenExpr ((((Ast_c.Ident ident),
                                                    _), _))), _), _))), _)
      | Some ((Ast_c.Cast (_, (((Ast_c.Ident ident), _), _))), _) ->
        false
      | _ -> true))

let get_returned_expression_and_line (Block (_, exp, _, _, part,
                                    blk_strtlineno, _, stmtlist)) =
  let blank_type_info = ref (None, Ast_c.NotTest) in
  match Def.return_exists_in_list stmtlist with
    None    -> None
  | Some st ->
    let return_expr =
      match Ast_c.unwrap st with
        Ast_c.Jump (Ast_c.ReturnExpr (e, _)) -> Some e
      | _ -> None
    in

    (match return_expr with
       Some ((Ast_c.Ident (Ast_c.RegularName(id, ii))), typ) ->
       let fin_lineno = blk_strtlineno in
       let expr = (((Ast_c.Ident (Ast_c.RegularName(id, ii))), typ), []) in
       Some (fin_lineno, expr)
     | Some ((Ast_c.ParenExpr ((((Ast_c.Ident (Ast_c.RegularName(id, ii))),
                                 _), _))), _) ->
       let fin_lineno = Def.find_startline_no (Def.create_stmtlist st) in
       let expr = (((Ast_c.Ident (Ast_c.RegularName(id, ii))),
                    blank_type_info), []) in
       Some (fin_lineno, expr)

     | Some ((Ast_c.Cast (_, (((Ast_c.ParenExpr ((((Ast_c.Ident ident),
                                                   _), _))), _), _))), _)
     | Some ((Ast_c.Cast (_, (((Ast_c.Ident ident), _), _))), _) ->
       let fin_lineno = Def.find_startline_no (Def.create_stmtlist st) in
       let expr = (((Ast_c.Ident ident), blank_type_info), []) in
       Some (fin_lineno, expr)

     | _ -> None)
*)

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

let find_ptr_args_list args =
  let pointer_test p acc =
    match Def.is_pointer p with
      Def.IsPtr
    | Def.UnknownType -> p::acc
    | _ -> acc
  in
  let find_ptr_args_list_aux arg acc =
    match arg with
      (((Ast_c.Cast (_, p)), _), _) ->
      pointer_test p acc
    | _ ->
      pointer_test arg acc
  in
  List.fold_right find_ptr_args_list_aux args []


let exp_exists_in_stmtlist_new exp =
  let exp_exists_in_stmtlist_new_aux statement =
    match Ast_c.unwrap statement with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (_, es)), _), _)) ->
      let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
      Def.exp_exists_in_list exp args_list
    | _ -> false
  in
  List.exists exp_exists_in_stmtlist_new_aux


let rec unused_ptr_args list = function
    []-> []
  | h::t-> if (exp_exists_in_stmtlist_new h list) then
      unused_ptr_args list t
    else h::(unused_ptr_args list t)


let all_exp_exists_in_list tail ptr_args_list =
  List.for_all (fun arg -> exp_exists_in_stmtlist_new arg tail) ptr_args_list

let rec create_argslist args_list = function
    []   -> args_list
  | h::t ->
    match Ast_c.unwrap h with
      Common.Left ((exp, typ), ii) ->
      create_argslist (args_list@[Some ((exp, typ), ii)]) t
    | _ -> create_argslist args_list t

let args_contains_array = function
    (((Ast_c.ArrayAccess _), _), _) -> true
  | _ -> false

let rec remove_int = function
    []   -> []
  | h::t ->
    match Def.is_pointer h with
      Def.IsntPtr -> remove_int t
    | _ -> h::remove_int t

let first_arg_is_array args_list =
  match remove_int args_list with
    []   -> false
  | h::t -> args_contains_array h

(* return the list a resource release statements present in the block *)
let get_resource_release (Block (_, _, _, _, _, _, _, statements)) acc =
  let get_resource_release_aux (rr_ops_list, tail) statement =
    let ntail =
      if tail = []
      then []
      else List.tl tail
    in
    match Ast_c.unwrap statement with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall ((((Ast_c.Ident
                                                       (Ast_c.RegularName (_,
                                                                           [ii3]))),
                                                   _), _), es)), _), _)) ->
      let args_list = Def.remove_optionlist (create_argslist [] es) in
      if (List.length args_list) = 0 &&
         not (Def.stmt_exists_in_list statement (List.map snd rr_ops_list))
      then
        ((([], statement)::rr_ops_list), ntail)
      else
      if not (first_arg_is_array args_list ||
              Def.string_exists_in_explist args_list ||
              Def.stmt_exists_in_list statement (List.map snd rr_ops_list))
      then
        let ptr_args_list = find_ptr_args_list args_list in
        if not (ptr_args_list = [] ||
                all_exp_exists_in_list tail ptr_args_list)
        then
          let unused_args = unused_ptr_args tail ptr_args_list in
          if no_exp_exists_in_stmtlist tail unused_args
          then
            (((unused_args, statement)::rr_ops_list), ntail)
          else
            (rr_ops_list, ntail)
        else
          (rr_ops_list, ntail)
      else
        (rr_ops_list, ntail)
    | _ -> (rr_ops_list, ntail)
  in
  fst (List.fold_left get_resource_release_aux (acc, List.tl statements) statements)

let get_returned_expression (Block (_, _, _, _, _, _, _, stmtlist)) =
  Def.return_exists_in_list stmtlist

let find_all_resource_release_without_argument (Block (_, _, _, _, _, _, _,
                                                       statements)) =
  let find_all_resource_release_without_argument_aux s acc =
    match Ast_c.unwrap s with
      Ast_c.ExprStatement (Some (((Ast_c.FunCall  (_, es)), _), _)) ->
      let args_list = Def.create_argslist [] es in
      (match args_list with
         [] -> s::acc
       | _  -> acc)
    | _ -> acc
  in
  List.fold_right find_all_resource_release_without_argument_aux statements []


let rec any_exp_exists_in_stmt stmt = function
    []-> false
  | h::t-> if (Def.exp_exists_in_stmt (Some h) stmt) then true
    else any_exp_exists_in_stmt stmt t


let rec create_stmtlist = function
    []->[]
  | h::t-> ((Ast_c.ExprStatement (Some h),[]))::(create_stmtlist t)

let return_st_access_resource miss_st (Block (_, _, _, _, _, _, _,
                                              statements))=
  let rec return_st_access_resource_aux = function
      []   -> false
    | h::t ->
      (match Ast_c.unwrap h with
         Ast_c.Jump (Ast_c.ReturnExpr e1) ->
         (match Ast_c.unwrap miss_st with
            Ast_c.ExprStatement (Some (((Ast_c.FunCall  (e, es)), typ), ii)) ->
            let args_list = Def.remove_optionlist (Def.create_argslist [] es) in
            let args_stmtlist = create_stmtlist (find_ptr_args_list args_list) in
            (match args_list with
               [] -> false
             | _  ->
               any_exp_exists_in_stmt h args_list ||
               exp_exists_in_stmtlist (Some e1) args_stmtlist)
          | _ -> false)
       | _ -> return_st_access_resource_aux t)
  in
  return_st_access_resource_aux statements


