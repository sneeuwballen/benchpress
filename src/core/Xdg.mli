(** {1 Basic XDG config}

    We follow losely
    https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html *)

val name_of_project : string ref

val get_home : unit -> string

val interpolate_home : string -> string
(** Replace [$HOME] by the home directory in this string *)

val config_dir : unit -> string
(** Where to search for configuration *)

val data_dir : unit -> string
(** Where to store permanent data *)

val cache_dir : unit -> string
(** Where to store disposable data that is only useful for improving perf
but can be erased at any point *)

val runtime_dir : unit -> string
(** Where to store runtime files such as unix sockets *)
