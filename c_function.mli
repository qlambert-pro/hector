type c_function

val mk_c_function: Ast_c.statement list -> c_function

val find_interproc_calls:
  (Ast_c.name * 'a * Ast_c.statement list) list ->
  Block.block ->
  c_function ->
  Resource.resource list ->
  Resource.resource list

(* Return a list of error handling blocks *)
val find_errorhandling:
  c_function -> Block.block list

val is_resource_allocated_properly:
  Block.block -> Block.block list -> c_function -> Resource.resource -> bool

val is_resource_released:
  Block.block -> Block.block list -> c_function -> Resource.resource -> bool

val resource_of_release_temp:
  Block.block -> Block.block list -> c_function -> Resource.resource -> bool

val get_resources:
  c_function -> Block.block list -> Resource.resource list
