type block

val mk_block:
  int -> Ast_c.expression -> Ast_c.statement option ->
  Ast_c.statement list -> Def.block_part -> int -> int ->
  Ast_c.statement list -> block

val mk_block_simple: int -> Ast_c.statement list -> block

(* TODO probably not satisfying *)
val extract_statements: block -> Ast_c.statement list
val extract_branch_start: block -> int
val extract_test_case: block -> Ast_c.expression

val compare_branch_start: block -> block -> int
val compare_branch_end_with_start: block -> block -> int

val is_error_handling_block: (int -> Ast_c.expression -> bool) -> block -> bool

val get_returned_expression: block -> Ast_c.statement option

(* Return the returned expression as well as the line where it dies, its not
 * generic the current version should not assume anything about the behaviour of
 * that function *)
(* val get_returned_expression_and_line: block -> (int * Ast_c.expression)
 * option *)

val expression_used_as_argument: block -> Ast_c.expression -> bool
val does_block_contains_statement: block -> Ast_c.statement  -> bool
val contains_expression: block -> Ast_c.expression -> bool

val has_goto: block -> bool

(* TODO clean that *)
val get_resource_release:
  block ->
  Resource.release list ->
  Resource.release list

val find_all_resource_release_without_argument:
  block -> Ast_c.statement list

val return_st_access_resource:
  Ast_c.statement ->
  block ->
  bool
