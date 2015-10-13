(* Find missing resource releasing operations from error handling block *)
val find_missing_rr_ops_new :
  C_function.c_function ->
  Block.block ->
  Resource.release list ->
  Resource.resource list
