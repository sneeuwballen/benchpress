(** Parallel execution utilities using Eio *)

val map_p : j:int -> ('a -> 'b) -> 'a list -> 'b list
(** Map on the list with at most [j] parallel fibers *)

val map_with_resource :
  resources:'r list -> ('r -> 'a -> 'b) -> 'a list -> 'b list
(** Map on the list [l] with each call to [f] being associated one of the
    resources from [resources] that is guaranteed not to be used concurrently by
    another call to [f]. *)

val with_affinity : int -> (unit -> 'a) -> 'a
(** Execute a function with CPU affinity set to a specific core *)

val with_affinity_opt : int option -> (unit -> 'a) -> 'a
(** Execute a function with optional CPU affinity *)
