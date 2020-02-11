
module Protocol_j = Protocol_j
module Protocol_t = Protocol_t

module Signal = Signal

(** Main module *)
module Ctx = Api_ctx

(* TODO: hooks for making it easier to plug into [Serve] with /api/ endpoint

   something like that?
   [ module Http_server : sig
      val make :
        Ctx.t ->
        (string * json) Signal.Sink.t ->
        < stop:unit -> bool;
          response: (http_code * json) Signal.Source.t;
        >
   ]

*)

(* TODO: client based on curly *)


(* TODO: simple TCP server? *)
(* TODO: simple TCP client? *)
