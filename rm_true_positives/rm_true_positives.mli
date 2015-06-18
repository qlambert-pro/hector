val remove_blks_that_returns_resource :
  (Ast_c.statementbis * 'a) list ->
  ('b * 'c * 'd * 'e * 'f * 'g * 'h * (Ast_c.statementbis * 'i) list) list ->
  ('j * 'k * 'l * 'm * 'n * 'o * 'p * (Ast_c.statementbis * 'q) list) list ->
  ('j * 'k * 'l * 'm * 'n * 'o * 'p * (Ast_c.statementbis * 'q) list) list

(* Return list of instances where the returned resource has been allocated *)
val return_resource_new :
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ('a * 'b * (Ast_c.statementbis * 'c)) list ->
  ('a * 'b * (Ast_c.statementbis * 'c)) list

(* Return the list of resources released on the execution path *)
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

(* Return the list of resource release without arguments ? *)
val is_rrwa_alloc :
  ('a * 'b * 'c * int * 'd * Ast_c.statement list) list ->
  int ->
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  (Ast_c.statement * 'f * Ast_c.statement) list ->
  (Ast_c.statement * 'f * Ast_c.statement) list

(* Return a list of resources that are not allocated yet *)
val resource_is_not_allocated_yet :
  ('a * 'b * 'c * int * int * Ast_c.statement list) list ->
  int ->
  Ast_c.statement list ->
  (Ast_c.name * Ast_c.statement list) list ->
  (Ast_c.statement * 'd * Ast_c.statement) list ->
  (Ast_c.statement * 'd * Ast_c.statement) list

(* Filtering out the resources having several allocation points *)
val is_resource_having_same_def_new :
  int ->
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ('d * 'e * 'f * int * 'g * Ast_c.statement list) list ->
  Ast_c.statement list ->
  ((Ast_c.statementbis * 'c list) * Ast_c.expression list * Ast_c.statement)
  list ->
  ((Ast_c.statementbis * 'c list) * Ast_c.expression list * Ast_c.statement)
  list
