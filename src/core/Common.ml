module Fmt = CCFormat
module Db = Sqlite3_utils
module CCOpt = Containers.Option
module Str_map = CCMap.Make (String)
module Str_set = CCSet.Make (String)
module PB = PrintBox

let spf = Printf.sprintf
let now_s () = Ptime_clock.now () |> Ptime.to_float_s
let reset_line = "\x1b[2K\r"

(** Sidekick-style idiom for resource handling *)
let ( let@ ) f x = f x
