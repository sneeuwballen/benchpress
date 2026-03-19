(** Networking and socket utilities *)

val pp_inet_addr : Format.formatter -> Unix.inet_addr -> unit
(** Pretty-print an inet address *)

val pp_unix_addr : Format.formatter -> Unix.sockaddr -> unit
(** Pretty-print a Unix address *)

val ip_addr_conv : Unix.inet_addr Cmdliner.Arg.conv
(** Cmdliner converter for IP addresses *)

val localhost_addr : unit -> Unix.inet_addr
(** Get the localhost address *)

val accept_non_intr : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
(** Accept a connection on a socket, retrying on EINTR *)

val mk_socket : Unix.sockaddr -> Unix.file_descr * string
(** Create and bind a socket to a sockaddr *)

val start_server :
  int -> (in_channel -> out_channel -> unit) -> Unix.file_descr -> unit
(** Start a server on a socket *)

val establish_server :
  int -> (in_channel -> out_channel -> unit) -> Unix.sockaddr -> unit
(** Establish a server on a sockaddr *)

val mk_shell_cmd :
  ?options:(string * string option) list -> ?target:string -> string -> string
(** Build a shell command with options and target *)
