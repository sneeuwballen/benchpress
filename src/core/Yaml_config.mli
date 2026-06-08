(* This file is free software. See file "license" for more details. *)

exception Config_error of string

val load_file : previous:Definitions.t -> string -> Definitions.t

val load_yaml_string :
  previous:Definitions.t -> string -> cur_dir:string -> Definitions.t

val load_json_string :
  previous:Definitions.t -> string -> cur_dir:string -> Definitions.t

val decode :
  previous:Definitions.t -> Config_value.value -> string -> Definitions.t

val config_template : string
