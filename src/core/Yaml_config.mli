(* This file is free software. See file "license" for more details. *)

exception Config_error of string

val load_file : string -> Definitions.t
val load_yaml_string : string -> string -> Definitions.t
val load_json_string : string -> string -> Definitions.t
val decode : Ezjsonm.value -> string -> Definitions.t
val config_template : string
