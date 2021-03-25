

type json = Yojson.Safe.t

module type IO = sig
  include Linol.IO

  val read : in_channel -> bytes -> int -> int -> unit t
  val read_line : in_channel -> string t

  val write : out_channel -> bytes -> int -> int -> unit t
  val write_string : out_channel -> string -> unit t

  val spawn : (unit -> unit t) -> unit

  val fail : exn -> unit t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module type S = sig
  module IO : IO

  type t
  (** A jsonrpc2 connection. *)

  include module type of Linol.Make(IO)

  val create :
    ic:IO.in_channel ->
    oc:IO.out_channel ->
    server ->
    t
  (** Create a connection from the pair of channels *)

  val run :
    ?cancelled:(unit -> bool) ->
    t -> unit IO.t
  (** Listen for incoming messages and responses *)
end

module Make(IO : IO) : S with module IO = IO

(** Blocking IO with a new thread for each [spawn] *)
module Blocking_IO :
  IO with type 'a t = 'a
     and type in_channel = in_channel
     and type out_channel = out_channel
