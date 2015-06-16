val stack_rr_op_new :
  'a ->
  'b ->
  'c ->
  (Ast_c.expression list * Ast_c.statement) list ->
  Ast_c.statement list ->
  'd ->
  Ast_c.statement list -> (Ast_c.expression list * Ast_c.statement) list

val find_missing_rr_ops_new :
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ('a * Ast_c.statement) list ->
  (Ast_c.statement * 'a * Ast_c.statement) list
