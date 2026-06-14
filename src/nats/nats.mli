(** Minimal NATS client using Eio.

    Connect to a NATS server, publish, subscribe, and make requests. Supports
    plain messages and messages with headers (HPUB/HMSG). The connection lives
    on a switch; close it by calling {!close} or finishing the switch. *)

(** {2 Types} *)

type t
(** A connection to a NATS server. *)

type sub
(** A subscription handle. *)

type header = string * string
(** A header key-value pair. *)

(** {2 Connection} *)

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  ?token:string ->
  ?user:string ->
  ?pass:string ->
  ?host:string ->
  ?port:int ->
  unit ->
  t
(** Connect to a NATS server at [host]:[port]. [host] must be an IPv4 or IPv6
    address.
    @param host ip address, or localhost by default
    @param port port, or 4222 by default *)

val with_connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  ?token:string ->
  ?user:string ->
  ?pass:string ->
  ?host:string ->
  ?port:int ->
  unit ->
  (t -> 'a) ->
  'a
(** Combines {!connect} and {!close} *)

(** {2 Messaging} *)

val pub : t -> subject:string list -> ?reply_to:string -> string -> unit
(** Publish a plain message. Subject components are joined with '.'; each must
    be non-empty and must not contain '.' or ' '. or '>' or '*'. *)

val hpub :
  t ->
  subject:string list ->
  ?reply_to:string ->
  ?headers:header list ->
  string ->
  unit
(** Publish a message with headers. *)

type msg = {
  subject: string list;
  sid: int;
  reply_to: string option;
  headers: header list option;
  payload: string;
}

val sub :
  t ->
  sw:Eio.Switch.t ->
  subject:string list ->
  ?queue:string ->
  (msg -> unit) ->
  sub
(** Subscribe to [subject] with an optional [queue] group. The callback receives
    optional reply-to, optional headers, and the payload. Auto-unsubscribed when
    [sw] finishes. Subject components are joined with '.'; each must be
    non-empty and must not contain '.' or ' '. Wildcards '>' and '*' are
    allowed. *)

val unsub : t -> ?max_msgs:int -> sub -> unit
(** Explicitly unsubscribe. *)

val request :
  t ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  subject:string list ->
  timeout:float ->
  string ->
  (msg, [> `Timeout ]) result
(** Send a request, wait for a single reply up to [timeout] seconds. *)

(** {2 Lifecycle} *)

val resolve_host : string -> string
(** Resolve a hostname or IP address to a dotted IP string. Raises [Failure] if
    the hostname can't be resolved. *)

val wait : t -> unit
(** Await for the client to be done (exit, shutdown, etc.) *)

val close : t -> unit
(** Shut down the underlying socket, causing the reader fiber to exit. *)

(** {2 Retry} *)

val with_retry :
  clock:_ Eio.Time.clock ->
  ?delay:float ->
  ?max_retries:int ->
  connect:(unit -> t) ->
  unit ->
  (t -> 'a) ->
  'a
(** [with_retry ~clock ~delay ~max_retries ~connect () f] calls [connect]
    repeatedly on failure, sleeping [delay] seconds between attempts. If
    [max_retries] is [Some n], gives up after [n] retries. The connection is
    automatically closed after [f] returns.

    Can be used with [let@ conn = with_retry ~clock ~connect () in ....] as
    well. *)
