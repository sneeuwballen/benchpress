
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
