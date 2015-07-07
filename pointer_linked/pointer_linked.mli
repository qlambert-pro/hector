(* Return the list of reference each variable can store ? *)
val gather_all_ref_var:
  (Ast_c.statementbis * 'a) list ->
  (Ast_c.expression * Ast_c.expression list) list
