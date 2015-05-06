val goto_jump_toback :
  Ast_c.statement -> (Ast_c.name * Ast_c.statement list) list -> bool

val code_for_goto :
  Ast_c.name ->
  (Ast_c.name * (Ast_c.statementbis * 'a) list) list ->
  (Ast_c.statementbis * 'a) list

val gather_goto_code :
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  Ast_c.name list -> Ast_c.statement list -> Ast_c.statement list

val any_ref_var_access_stmt :
  Ast_c.statement -> Ast_c.expression list -> bool

val any_var_access_stmt :
  Ast_c.statement -> (Ast_c.expression * Ast_c.expression list) list -> bool

val is_following_code_access_exp :
  (Ast_c.expression * Ast_c.expression list) list ->
  int -> bool -> int -> Ast_c.statement list -> bool

val generate_exe_paths_simple :
  int ->
  Ast_c.statement list list ->
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list -> Ast_c.statement list list

val find_recent_id_values_paths_second :
  Ast_c.expression ->
  Ast_c.expression list ->
  (Ast_c.statementbis * 'a) list list -> Ast_c.expression list

val find_ptr_args_list :
  (('a * ((('b * (Ast_c.typeCbis * 'c)) * 'd) option * 'e) ref) * 'f) list ->
  (('a * ((('b * (Ast_c.typeCbis * 'c)) * 'd) option * 'e) ref) * 'f) list

val find_recent_id_values_paths :
  'a ->
  'b ->
  Ast_c.expression ->
  Ast_c.expression list -> Ast_c.statement list list -> Ast_c.expression list

val list_of_id_values :
  'a ->
  'b ->
  Ast_c.statement list list ->
  Ast_c.expression list -> (Ast_c.expression * Ast_c.expression list) list

val defined_alloc : string -> bool

val remove_not_errorblks :
  'a ->
  'b ->
  Ast_c.expression ->
  (Ast_c.name * Ast_c.statement list) list ->
  Ast_c.statement list ->
  Ast_c.statement list -> Def.block_part -> int -> bool

val find_errorhandling :
  (Ast_c.name * Ast_c.statement list) list ->
  'a ->
  Ast_c.statement list ->
  (int * Ast_c.expression * Ast_c.statement option * Ast_c.statement list *
   Def.block_part * int * int * Ast_c.statement list)
  list
