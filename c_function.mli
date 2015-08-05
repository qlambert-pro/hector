type c_function

val mk_c_function: Ast_c.statement list -> c_function

(*val get_init_labels: c_function -> Ast_c.statement list*)

val exists_same_return_value: Ast_c.expression -> c_function -> bool

val goto_jump_toback:
  c_function -> Ast_c.statement -> bool

val find_final_return:
  c_function -> int

val is_locally_main:
  int -> Ast_c.name -> bool -> c_function -> bool

val is_following_code_access_exp:
  (Ast_c.expression * Ast_c.expression list) list ->
  int -> bool -> int -> c_function -> bool

val find_interproc_calls:
  Ast_c.statement list ->
  (Ast_c.name * 'a * Ast_c.statement list) list ->
  int ->
  c_function ->
  ('c * 'd * Ast_c.statement) list ->
  ('c * 'd * Ast_c.statement) list

(* Return a list of statement following through goto *)
val gather_goto_code:
  c_function -> Ast_c.statement list ->
  Ast_c.statement list -> Ast_c.statement list

(* Return a list of error handling blocks *)
val find_errorhandling:
  c_function ->
  (int * Ast_c.expression * Ast_c.statement option * Ast_c.statement list *
   Def.block_part * int * int * Ast_c.statement list)
  list

(* Generates different execution path *)
val generate_exe_paths_simple:
  int -> Ast_c.statement list list ->
  c_function ->
  Ast_c.statement list list

(* It should return a block *)
val find_model_block_by_release_statement:
  c_function ->
  Ast_c.statement ->
  (int * Ast_c.statement list) option

(* TODO This has to be rewritten, init_labels may not be required *)
val rr_in_exe_paths_init_lbl:
  Ast_c.statement ->
  c_function ->
  Ast_c.statement -> bool

val get_identifier_values:
  c_function ->
  int ->
  Ast_c.expression list ->
  Ast_c.expression list

val check_each_path:
  c_function -> Ast_c.statement -> Ast_c.statement -> Ast_c.expression list ->
  Ast_c.expression list -> int -> Ast_c.expression -> Ast_c.statement list ->
  int ->
  ('a * Ast_c.expression * 'b * int * int * Ast_c.statement list) list ->
  bool
