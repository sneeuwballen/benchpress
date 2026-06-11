(** Lightweight observer pattern — multiple subscribers, called immediately on
    {!publish}. Each subscriber is uniquely identified by a handle. *)

type 'a t
type handle

val create : unit -> 'a t
(** Create a new signal. *)

val subscribe : 'a t -> sw:Eio.Switch.t -> ('a -> unit) -> handle
(** [subscribe t ~sw f] registers [f] to be called on every {!publish}. Returns
    a handle that can be passed to {!unsubscribe}. The subscription is
    automatically removed when [sw] is turned off. The callback is forked in
    [sw] so a slow subscriber does not block others. *)

val subscribe_sync : 'a t -> ('a -> unit) -> handle
(** Like {!subscribe} but the callback is called synchronously during
    {!publish}. The subscriber is never automatically cleaned up — use
    {!unsubscribe} explicitly. Intended for callbacks that must propagate
    exceptions (e.g. SSE writers that raise on disconnect). *)

val unsubscribe : 'a t -> handle -> unit
(** [unsubscribe t h] removes the subscriber identified by handle [h].
    Idempotent — calling with an already-removed handle is a no-op. *)

val publish : 'a t -> 'a -> unit
(** [publish t v] calls every subscriber's callback with [v]. Each callback is
    forked in its subscriber's switch so a slow subscriber does not block
    others. The subscriber list is snapshotted before iteration to avoid
    reentrancy issues. *)
