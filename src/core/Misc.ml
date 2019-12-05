(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
module Str_map = CCMap.Make(String)

let _lock = CCLock.create()

let reset_line = "\x1b[2K\r"
let synchronized f = CCLock.with_lock _lock f

module Debug : sig
  val set_level : int -> unit

  val debugf : int -> ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit
  val debug : int -> string -> unit
end = struct
  let lev_ = ref 0
  let set_level = (:=) lev_

  let debugf l k =
    if l <= !lev_ then (
      synchronized
        (fun () ->
           k (Format.kfprintf
               (fun fmt -> Format.fprintf fmt "@.")
               Format.std_formatter))
    )

  let debug l msg = debugf l (fun k->k "%s" msg)
end

let pp_list ?(sep=" ") f out l =
  let sep out () = Fmt.fprintf out "%s@," sep in
  Fmt.list ~sep f out l

let die_on_sigterm : unit -> unit =
  let thunk = lazy (
    Sys.set_signal 15
      (Sys.Signal_handle
         (fun _ ->
            print_endline "received sigterm, exiting";
            Unix.kill 0 15; (* kill children *)
            exit 1)))
  in fun () -> Lazy.force thunk

(** Human readable duration *)
let human_time (f:float) : string =
  let nb_sec_minute = 60 in
  let nb_sec_hour = 60 * nb_sec_minute in
  let nb_sec_day = 24 * nb_sec_hour in
  let n = int_of_float f in
  let aux n div = n / div, n mod div in
  let n_day, n = aux n nb_sec_day in
  let n_hour, n = aux n nb_sec_hour in
  let n_min, n = aux n nb_sec_minute in
  let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
  (print_aux "d" n_day) ^
  (print_aux "h" n_hour) ^
  (print_aux "m" n_min) ^
  (string_of_int n) ^ "s"

(** Human readable size *)
let human_size (x:int) : string =
  if x >= 1_000_000 then Printf.sprintf "%d.%dM" (x / 1_000_000) ((x/1000) mod 1_000)
  else if x >= 1_000 then Printf.sprintf "%d.%dK" (x/1000) ((x/100) mod 10)
  else string_of_int x

(** Parallel map *)
module Par_map = struct
  (* map on the list with at most [j] parallel threads *)
  let map_p ~j f l =
    if j<1 then invalid_arg "map_p: ~j";
    die_on_sigterm();
    (* NOTE: for some reason the pool seems to spawn one too many thread
       in some cases. So we add a guard to respect [-j] properly. *)
    let sem = CCSemaphore.create j in
    let f_with_sem x =
      CCSemaphore.with_acquire ~n:1 sem ~f:(fun () -> f x)
    in
    let module P = CCPool.Make(struct
        let min_size = 0
        let max_size = j
      end) in
    let res =
      CCList.map (fun x -> P.Fut.make1 f_with_sem x) l
      |> P.Fut.sequence_l
      |> P.Fut.get
    in
    P.stop();
    res
end

module Json = struct
  type t = Yojson.Basic.t
  module J = Yojson.Basic
  module Encode = struct
    include Decoders_yojson.Basic.Encode
    type 'a t = 'a encoder
  end
  module Decode = struct
    include Decoders_yojson.Basic.Decode
    type 'a t = 'a decoder
    let (>>::) x f = uncons f x
    let list1 s = list s >>= function [x] -> succeed x | _ -> fail "need unary list"
  end
end
