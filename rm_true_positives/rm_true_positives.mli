val code_for_goto :
  Ast_c.name ->
  (Ast_c.name * (Ast_c.statementbis * 'a) list) list ->
  (Ast_c.statementbis * 'a) list

val gather_goto_code :
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list -> Ast_c.statement list -> Ast_c.statement list

val remove_blks_that_returns_resource :
  (Ast_c.statementbis * 'a) list ->
  ('b * 'c * 'd * 'e * 'f * 'g * 'h * (Ast_c.statementbis * 'i) list) list ->
  ('j * 'k * 'l * 'm * 'n * 'o * 'p * (Ast_c.statementbis * 'q) list) list ->
  ('j * 'k * 'l * 'm * 'n * 'o * 'p * (Ast_c.statementbis * 'q) list) list

val return_resource_new :
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ('a * 'b * (Ast_c.statementbis * 'c)) list ->
  ('a * 'b * (Ast_c.statementbis * 'c)) list ->
  ('a * 'b * (Ast_c.statementbis * 'c)) list

val rls_in_exe_paths :
  int ->
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ('a * 'b * 'c * int * 'd * Ast_c.statement list) list ->
  Ast_c.expression ->
  Ast_c.statement list ->
  Ast_c.statement list ->
  (Ast_c.statement * Ast_c.expression list * Ast_c.statement) list ->
  (Ast_c.statement * Ast_c.expression list * Ast_c.statement) list ->
  (Ast_c.statement * Ast_c.expression list * Ast_c.statement) list

val is_rrwa_alloc :
  ('a * 'b * 'c * int * 'd * Ast_c.statement list) list ->
  'e ->
  int ->
  (Ast_c.statement * 'f * Ast_c.statement) list ->
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  (Ast_c.statement * 'f * Ast_c.statement) list ->
  (Ast_c.statement * 'f * Ast_c.statement) list

val resource_is_not_allocated_yet :
  ('a * 'b * 'c * int * int * Ast_c.statement list) list ->
  int ->
  Ast_c.statement list ->
  (Ast_c.name * Ast_c.statement list) list ->
  (Ast_c.statement * 'd * Ast_c.statement) list ->
  (Ast_c.statement * 'd * Ast_c.statement) list ->
  (Ast_c.statement * 'd * Ast_c.statement) list

val is_resource_having_same_def_new :
  'a ->
  'b ->
  ((Ast_c.statementbis * 'c list) * Ast_c.expression list * Ast_c.statement)
  list ->
  int ->
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ('d * 'e * 'f * int * 'g * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ((Ast_c.statementbis * 'c list) * Ast_c.expression list * Ast_c.statement)
  list ->
  ((Ast_c.statementbis * 'c list) * Ast_c.expression list * Ast_c.statement)
  list
