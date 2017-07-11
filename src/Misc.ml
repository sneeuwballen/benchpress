
(* This file is free software. See file "license" for more details. *)

module Str_map = CCMap.Make(String)

module Debug : sig
  val set_level : int -> unit

  val debugf : int -> ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit
  val debug : int -> string -> unit
end = struct
  let lev_ = ref 0
  let set_level = (:=) lev_

  let lock_ = CCLock.create ()

  let debugf l k =
    if l <= !lev_ then (
      CCLock.with_lock lock_
        (fun () ->
        k (Format.kfprintf
            (fun fmt -> Format.fprintf fmt "@.")
            Format.std_formatter))
    )

  let debug l msg = debugf l (fun k->k "%s" msg)
end

(** make sure that we are a session leader; that is, our children die if we die *)
let ensure_session_leader : unit -> unit =
  let thunk = lazy (
    if not Sys.win32 && not Sys.cygwin
    then ignore (Unix.setsid ())
  ) in
  fun () -> Lazy.force thunk

let die_on_sigterm : unit -> unit =
  let thunk = lazy (
    Sys.set_signal 15
      (Sys.Signal_handle
         (fun _ ->
            print_endline "received sigterm, exiting";
            exit 1)))
  in fun () -> Lazy.force thunk

(** Parallel map *)
module Par_map = struct
  (* map on the list with at most [j] parallel threads *)
  let map_p ~j f l =
    if j<1 then invalid_arg "map_p: ~j";
    die_on_sigterm();
    let module P = CCPool.Make(struct
        let min_size = 1
        let max_size = j
      end) in
    CCList.map
      (fun x -> P.Fut.make1 f x)
      l
    |> P.Fut.sequence_l
    |> P.Fut.get
end
