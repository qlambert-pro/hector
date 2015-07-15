(* Add statements having unused arguments to the list of resource release
 * operation *)
val stack_rr_op_new :
  (Ast_c.expression list * Ast_c.statement) list ->
  Ast_c.statement list ->
  (Ast_c.expression list * Ast_c.statement) list

(* Find missing resource releasing operations from error handling block *)
val find_missing_rr_ops_new :
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ('a * Ast_c.statement) list ->
  (Ast_c.statement * 'a * Ast_c.statement) list
