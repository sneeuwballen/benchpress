(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
module Str_map = CCMap.Make(String)
module E = CCResult
type 'a or_error = ('a, string) E.t

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
      let out = Format.std_formatter in
      synchronized
        (fun () ->
           Format.fprintf out "[%d] " l;
           k (Format.kfprintf
               (fun fmt -> Format.fprintf fmt "@.") out))
    )

  let debug l msg = debugf l (fun k->k "%s" msg)
end

let pp_list ?(sep=" ") f out l =
  let sep out () = Fmt.fprintf out "%s@," sep in
  Fmt.list ~sep f out l

module Pp = struct
  let pp_l = pp_list
  let pp_f what f out x = Fmt.fprintf out "@ (@[%s@ %a@])" what f x
  let pp_opt what f out = function
    | None -> ()
    | Some x -> Fmt.fprintf out "@ (@[%s@ %a@])" what f x
  let pp_str out s = Sexp_loc.pp out (Sexp_loc.atom s)
  let pp_regex out r = Fmt.fprintf out "%S" r
end

(** Replace in [s] the list of key/values in [l]. Ignore other keys. *)
let str_replace l (s:string) : string =
  let buf = Buffer.create 32 in
  Buffer.add_substitute buf
    (fun k -> match List.assoc k l with
       | v -> v
       | exception Not_found -> "$" ^ k)
    s;
  Buffer.contents buf

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

(* ensure [s] is not too long *)
let truncate_left (n:int) (s:string) : string =
  if String.length s > n then "…" ^ String.sub s 1 (n-1) else s
let truncate_right (n:int) (s:string) : string =
  if String.length s > n then String.sub s 0 (n-1) ^ "…" else s
      
let get_cmd_out cmd =
  Debug.debugf 10 (fun k->k "get-cmd-out %S" cmd);
  CCUnix.with_process_in cmd
    ~f:(fun ic -> CCIO.read_all ic |> String.trim)

let mk_abs_path (s:string) : string =
  if Filename.is_relative s then Filename.concat (Sys.getcwd()) s
  else s

let guess_cpu_count () =
  try get_cmd_out "grep -c processor /proc/cpuinfo" |> int_of_string
  with _ -> 2

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
    Debug.debugf 5 (fun k->k "par-map: create pool j=%d" j);
    let module P = CCPool.Make(struct
        let max_size = j
      end) in
    let res =
      CCList.map (fun x -> P.Fut.make1 f_with_sem x) l
      |> P.Fut.sequence_l
      |> P.Fut.get
    in
    Debug.debugf 5 (fun k->k "par-map: stop pool");
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

module Git = struct
  (* obtain the current commit name *)
  let get_commit (dir:string) : string =
    get_cmd_out (
      Printf.sprintf "git -C %s rev-parse HEAD" dir)

  let get_branch dir : string =
    get_cmd_out (
      Printf.sprintf "git -C %s branch | grep '*' | cut -d ' ' -f2" dir)
end
