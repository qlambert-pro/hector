(* Return wether or not the statement is a jump backward in the function body *)
val goto_jump_toback :
  Ast_c.statement -> (Ast_c.name * Ast_c.statement list) list -> bool

(* Return a list of statement following through goto *)
val gather_goto_code :
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  Ast_c.statement list -> Ast_c.statement list

(* Return true if any of the var in the list is used in the statement *)
val any_var_access_stmt :
  Ast_c.statement -> (Ast_c.expression * Ast_c.expression list) list -> bool

(* Return true if the code following the line# given access the given expression *)
val is_following_code_access_exp :
  (Ast_c.expression * Ast_c.expression list) list ->
  int -> bool -> int -> Ast_c.statement list -> bool

(* Generates different execution path *)
val generate_exe_paths_simple :
  int ->
  Ast_c.statement list list ->
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list -> Ast_c.statement list list

(* Find the latest expression assigned to the first argument in each path *)
val find_recent_id_values_paths_second :
  Ast_c.expression ->
  Ast_c.expression list ->
  (Ast_c.statementbis * 'a) list list -> Ast_c.expression list

(* Find the latest expression assigned to the first argument in each path *)
val find_recent_id_values_paths :
  Ast_c.expression ->
  Ast_c.expression list ->
  Ast_c.statement list list -> Ast_c.expression list

(* Return a list of couple representing the latest expression assigned to each
 * element of the first list and for each path *)
val list_of_id_values :
  Ast_c.statement list list ->
  Ast_c.expression list -> (Ast_c.expression * Ast_c.expression list) list

(* Return true if the identifier is the name of a known resource allocation
 * function *)
val defined_alloc : string -> bool

(* Return true if there are error handling blocks in the function *)
val exists_error_handling_block :
  Ast_c.expression -> C_function.c_function ->  Ast_c.statement list ->
  Def.block_part -> int -> bool

(* Return a list of element representing error handling code *)
val find_errorhandling :
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  (int * Ast_c.expression * Ast_c.statement option * Ast_c.statement list *
   Def.block_part * int * int * Ast_c.statement list)
  list
