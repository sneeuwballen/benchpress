(* This file is free software. See file "license" for more details. *)

(** Human-readable formatting for display *)

module Fmt = CCFormat

(** Human readable duration *)
let human_duration (f : float) : string =
  let nb_sec_minute = 60 in
  let nb_sec_hour = 60 * nb_sec_minute in
  let nb_sec_day = 24 * nb_sec_hour in
  let n = int_of_float f in
  if n >= 1 then (
    let aux n div = n / div, n mod div in
    let n_day, n = aux n nb_sec_day in
    let n_hour, n = aux n nb_sec_hour in
    let n_min, n = aux n nb_sec_minute in
    let print_aux s n =
      if n <> 0 then
        string_of_int n ^ s
      else
        ""
    in
    print_aux "d" n_day ^ print_aux "h" n_hour ^ print_aux "m" n_min
    ^ string_of_int n
    ^ (if f -. floor f >= 0.1 then (
         let s = Printf.sprintf "%.1f" (f -. floor f) in
         "." ^ snd @@ CCString.Split.left_exn ~by:"." s
         (* remove the leading "0." *)
       ) else
         "")
    ^ "s"
  ) else
    Printf.sprintf "%.3fs" f

let pp_human_duration out f = Fmt.string out (human_duration f)

let human_datetime (f : float) : string =
  let t = Unix.gmtime f in
  let wday i = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |].(i) in
  Printf.sprintf "%d/%02d, %s the %02d, at %dh%02d:%02d GMT" (1900 + t.tm_year)
    (t.tm_mon + 1) (wday t.tm_wday) t.tm_mday t.tm_hour t.tm_min t.tm_sec

let pp_human_datetime out f = Fmt.string out (human_datetime f)

(** Compact representation of a datetime *)
let datetime_compact (t : Ptime.t) : string =
  let (y, m, d), ((h, min, z), _tz) = Ptime.to_date_time t in
  Printf.sprintf "%4d%02d%02dT%02d%02d%02d" y m d h min z

(** Human readable size *)
let human_size (x : int) : string =
  if x >= 1_000_000 then
    Printf.sprintf "%d.%dM" (x / 1_000_000) (x / 1000 mod 1_000)
  else if x >= 1_000 then
    Printf.sprintf "%d.%dK" (x / 1000) (x / 100 mod 10)
  else
    string_of_int x

(** Ensure [s] is not too long *)
let truncate_left (n : int) (s : string) : string =
  if String.length s > n then
    "…" ^ String.sub s (String.length s - n + 1) (n - 1)
  else
    s

let truncate_right (n : int) (s : string) : string =
  if String.length s > n then
    String.sub s 0 (n - 1) ^ "…"
  else
    s

(** Timer for measuring elapsed time *)
module Chrono : sig
  type t

  val start : unit -> t
  val since_last : t -> float
  val elapsed : t -> float
end = struct
  type t = { start: float; mutable last: float }

  let start () : t =
    let t = Ptime_clock.now () |> Ptime.to_float_s in
    { start = t; last = t }

  let elapsed (self : t) =
    let now = Ptime_clock.now () |> Ptime.to_float_s in
    self.last <- now;
    now -. self.start

  let since_last (self : t) =
    let now = Ptime_clock.now () |> Ptime.to_float_s in
    let r = now -. self.last in
    self.last <- now;
    r
end
