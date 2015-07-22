let version = "1.0.0"

let path =
  try (Sys.getenv "COCCINELLE_HOME")
  with Not_found->"/usr/local/lib/coccinelle"

let std_iso = ref (Filename.concat path "standard.iso")
let std_h   = ref (Filename.concat path "standard.h")

let dynlink_is_native = Dynlink.is_native
let get_temp_dir_name = (Filename.get_temp_dir_name ())
