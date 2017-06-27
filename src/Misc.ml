
(* This file is free software. See file "license" for more details. *)

module Str_map = CCMap.Make(String)

module Debug : sig
  val set_level : int -> unit

  val debugf : int -> ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit
  val debug : int -> string -> unit
end = struct
  let lev_ = ref 0
  let set_level = (:=) lev_

  let debugf l k =
    if l <= !lev_ then (
      k (Format.kfprintf
          (fun fmt -> Format.fprintf fmt "@.")
          Format.std_formatter)
    )

  let debug l msg = debugf l (fun k->k "%s" msg)
end

(** Parallel map *)
module Par_map = struct
  (* map on the list with at most [j] parallel threads *)
  let map_p ~j f l =
    if j<1 then invalid_arg "map_p: ~j";
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
