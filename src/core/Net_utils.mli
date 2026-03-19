(** Networking and socket utilities *)

(** Pretty-print an inet address *)
val pp_inet_addr : Format.formatter -> Unix.inet_addr -> unit

(** Pretty-print a Unix address *)
val pp_unix_addr : Format.formatter -> Unix.sockaddr -> unit

(** Cmdliner converter for IP addresses *)
val ip_addr_conv : Unix.inet_addr Cmdliner.Arg.conv

(** Get the localhost address *)
val localhost_addr : unit -> Unix.inet_addr

(** Accept a connection on a socket, retrying on EINTR *)
val accept_non_intr : Unix.file_descr -> Unix.file_descr * Unix.sockaddr

(** Create and bind a socket to a sockaddr *)
val mk_socket : Unix.sockaddr -> Unix.file_descr * string

(** Start a server on a socket *)
val start_server : int -> (in_channel -> out_channel -> unit) -> Unix.file_descr -> unit

(** Establish a server on a sockaddr *)
val establish_server : int -> (in_channel -> out_channel -> unit) -> Unix.sockaddr -> unit

(** Build a shell command with options and target *)
val mk_shell_cmd : ?options:(string * string option) list -> ?target:string -> string -> string
