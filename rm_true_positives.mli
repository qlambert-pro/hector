val remove_blks_that_returns_resource :
  C_function.c_function ->
  Block.block list ->
  Block.block list

(* Return list of instances where the returned resource has been allocated *)
val return_resource_new :
  Block.block ->
  Resource.resource list ->
  Resource.resource list

(* Return the list of resources released on the execution path *)
val rls_in_exe_paths :
  Block.block ->
  C_function.c_function ->
  Block.block list ->
  Resource.resource list ->
  Resource.resource list

(* Return the list of resource release without arguments ? *)
val is_rrwa_alloc :
  Block.block list ->
  Block.block ->
  C_function.c_function ->
  Resource.resource list ->
  Resource.resource list

(* Return a list of resources that are not allocated yet *)
val resource_is_not_allocated_yet :
  Block.block list ->
  Block.block ->
  C_function.c_function ->
  Resource.resource list ->
  Resource.resource list

(* Filtering out the resources having several allocation points *)
(* TODO this should not depend on block ! *)
val is_resource_having_same_def_new :
  Block.block ->
  C_function.c_function ->
  Block.block list ->
  Resource.resource list ->
  Resource.resource list
