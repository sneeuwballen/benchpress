(* This file is free software. See file "license" for more details. *)

(** Miscellaneous utilities *)

module Log = (val Logs.src_log (Logs.Src.create "benchpress.misc"))

(* Re-exports for backward compatibility *)
module Fmt = CCFormat
module Str_map = CCMap.Make (String)
module Str_set = CCSet.Make (String)
module Db = Sqlite3_utils
module PB = PrintBox
module Par_map = Par_map
module Pp = Pp
module Chrono = Human.Chrono

module Git = struct
  let get_commit (dir : string) : string =
    let cmd = Printf.sprintf "git -C %s rev-parse HEAD" dir in
    Log.debug (fun k -> k "get-cmd-out %S" cmd);
    CCUnix.with_process_in cmd ~f:(fun ic -> CCIO.read_all ic |> String.trim)

  let get_branch dir : string =
    let cmd =
      Printf.sprintf "git -C %s branch | grep '*' | cut -d ' ' -f2" dir
    in
    Log.debug (fun k -> k "get-cmd-out %S" cmd);
    CCUnix.with_process_in cmd ~f:(fun ic -> CCIO.read_all ic |> String.trim)
end

let spf = Printf.sprintf

(* Deprecated: use Log_setup.setup_logs directly *)
let setup_logs = Log_setup.setup_logs

(* Human-readable display (re-export from Human) *)
let human_duration = Human.human_duration
let pp_human_duration = Human.pp_human_duration
let human_datetime = Human.human_datetime
let pp_human_datetime = Human.pp_human_datetime
let datetime_compact = Human.datetime_compact
let human_size = Human.human_size
let truncate_left = Human.truncate_left
let truncate_right = Human.truncate_right

(* DB utilities (re-export from Db_utils) *)
let db_has_table = Db_utils.has_table
let err_of_db = Db_utils.err_of
let unwrap_db = Db_utils.unwrap
let unwrap_str = Db_utils.unwrap_str

(* Networking utilities (re-export from Net_utils) *)
let pp_inet_addr = Net_utils.pp_inet_addr
let pp_unix_addr = Net_utils.pp_unix_addr
let ip_addr_conv = Net_utils.ip_addr_conv
let localhost_addr = Net_utils.localhost_addr
let accept_non_intr = Net_utils.accept_non_intr
let mk_socket = Net_utils.mk_socket
let start_server = Net_utils.start_server
let establish_server = Net_utils.establish_server
let mk_shell_cmd = Net_utils.mk_shell_cmd

(* Parallel execution (re-export from Par_map) *)
include Global_lock

let with_affinity = Par_map.with_affinity
let with_affinity_opt = Par_map.with_affinity_opt

(* Pretty-printing utilities *)
let pp_list = Pp.pp_list

(** Run command and get stdout *)
let get_cmd_out cmd =
  Log.debug (fun k -> k "get-cmd-out %S" cmd);
  CCUnix.with_process_in cmd ~f:(fun ic -> CCIO.read_all ic |> String.trim)

(** Extract the program part of a command *)
let get_binary_of_cmd (cmd : string) : string =
  let cmd = String.trim cmd in
  try fst @@ CCString.Split.left_exn ~by:" " cmd with Not_found -> cmd

(** Replace in [s] the list of key/values in [l]. Ignore other keys. *)
let str_replace l (s : string) : string =
  let buf = Buffer.create 32 in
  Buffer.add_substitute buf
    (fun k ->
      match List.assoc k l with
      | v -> v
      | exception Not_found -> "$" ^ k)
    s;
  Buffer.contents buf

let mk_abs_path (s : string) : string =
  match CCString.chop_prefix ~pre:"file://" s with
  | Some s -> s (* URIs from lsp *)
  | None ->
    if Filename.is_relative s then
      Filename.concat (Sys.getcwd ()) s
    else
      s

(** Guess how many cores we have on the CPU *)
let guess_cpu_count () =
  try get_cmd_out "grep -c processor /proc/cpuinfo" |> int_of_string
  with _ -> 2

(** Ensure directory exists, recursively *)
let rec mkdir_rec (d : string) =
  if not (Sys.file_exists d) then (
    let d2 = Filename.dirname d in
    mkdir_rec d2;
    try Unix.mkdir d 0o755
    with _ -> Log.debug (fun k -> k "mkdir %S failed" d)
  )

(** [file_for_uuid pref ?dir ~timestamp uuid ext] builds a filename of the form
    "[pref]-[timestamp]-[uuid].[ext]" *)
let file_for_uuid pref ?(dir = Xdg.data_dir) ~timestamp uuid ext =
  let filename =
    Printf.sprintf "%s-%s-%s.%s" pref
      (match Ptime.of_float_s timestamp with
      | None -> Printf.sprintf "<time %.1fs>" timestamp
      | Some t -> Human.datetime_compact t)
      (Uuidm.to_string uuid) ext
  in
  let data_dir = Filename.concat (dir ()) !Xdg.name_of_project in
  mkdir_rec data_dir;
  Filename.concat data_dir filename

let is_zst_file s =
  Filename.check_suffix s ".sqlite.zst"
  || Filename.check_suffix s ".sqlite.zstd"

let strip_zst_suffix s =
  if Filename.check_suffix s ".sqlite.zst" then
    Filename.chop_suffix s ".zst"
  else
    Filename.chop_suffix s ".zstd"

(** Concatenate list into a path *)
let rec filename_concat_l = function
  | [] -> "."
  | [ s ] -> s
  | s :: tl -> Filename.concat s @@ filename_concat_l tl

let mk_uuid () : Uuidm.t =
  (* UUID v7: 48-bit ms timestamp + version 7 + 74 random bits *)
  let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
  let b = Bytes.create 16 in
  (* bytes 0-5: 48-bit big-endian millisecond timestamp *)
  for i = 0 to 5 do
    Bytes.set b i
      (Char.chr
         (Int64.to_int
            (Int64.logand
               (Int64.shift_right_logical now_ms (40 - (i * 8)))
               0xffL)))
  done;
  (* bytes 6-15: random *)
  let rng = Random.State.make_self_init () in
  for i = 6 to 15 do
    Bytes.set b i (Char.chr (Random.State.bits rng land 0xff))
  done;
  (* version 7: set high nibble of byte 6 to 0x7 *)
  Bytes.set b 6 (Char.chr (Char.code (Bytes.get b 6) land 0x0f lor 0x70));
  (* variant: set high bits of byte 8 to 10xxxxxx *)
  Bytes.set b 8 (Char.chr (Char.code (Bytes.get b 8) land 0x3f lor 0x80));
  Uuidm.of_bytes (Bytes.to_string b)
  |> CCOpt.get_exn_or "mk_uuid: invalid uuid bytes"

(** Modify filename of a lexing buffer, for future locations *)
let set_lexbuf_filename buf filename =
  let open Lexing in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename }

let ( // ) = Filename.concat
let data_dir () = Xdg.data_dir () // !Xdg.name_of_project
let config_dir () = Xdg.config_dir () // !Xdg.name_of_project
let default_config () = config_dir () // "conf.sexp"

(** Get current time in seconds since epoch *)
let now_s () = Ptime_clock.now () |> Ptime.to_float_s

let reset_line = "\x1b[2K\r"
