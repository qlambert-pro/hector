val interproc_new :
  Ast_c.name ->
  (Ast_c.name * 'a * (Ast_c.statementbis * 'b) list) list ->
  int ->
  ('c * 'd * 'e * int * 'f * Ast_c.statement list) list ->
  Ast_c.statement list ->
  (Ast_c.name * Ast_c.statement list) list ->
  ((Ast_c.statementbis * 'g) * 'h * Ast_c.statement) list ->
  ((Ast_c.statementbis * 'g) * 'h * Ast_c.statement) list ->
  ((Ast_c.statementbis * 'g) * 'h * Ast_c.statement) list

val find_interproc_calls :
  Ast_c.statement list ->
  (Ast_c.name * 'a * Ast_c.statement list) list ->
  int ->
  'b ->
  Ast_c.statement list ->
  ('c * 'd * Ast_c.statement) list ->
  ('c * 'd * Ast_c.statement) list -> ('c * 'd * Ast_c.statement) list
