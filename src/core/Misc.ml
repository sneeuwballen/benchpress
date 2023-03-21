(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
module Str_map = CCMap.Make(String)
module Str_set = CCSet.Make(String)
module Db = Sqlite3_utils
module PB = PrintBox
let spf = Printf.sprintf

let _lock = CCLock.create()

let now_s() = Ptime_clock.now () |> Ptime.to_float_s

let reset_line = "\x1b[2K\r"
let synchronized f = CCLock.with_lock _lock f

module Log_report = struct
  let buf_fmt () =
    let b = Buffer.create 512 in
    let fmt = Fmt.formatter_of_buffer b in
    Fmt.set_color_tag_handling fmt;
    let flush() =
      Format.fprintf fmt "@]@?";
      let m = Buffer.contents b in
      Buffer.reset b;
      m
    in
    fmt, flush

  let pp_h out (src,lvl,_) =
    let src = Logs.Src.name src in
    let src = if src = "application" then "" else src in
    match lvl with
    | Logs.Debug -> Fmt.fprintf out "[@{<black>debug@}:%s]" src
    | Logs.Info -> Fmt.fprintf out "[@{<cyan>info@}:%s]" src
    | Logs.Error -> Fmt.fprintf out "[@{<Red>error@}:%s]" src
    | Logs.Warning -> Fmt.fprintf out "[@{<yellow>warning@}:%s]" src
    | Logs.App -> Fmt.fprintf out "[@{<blue>app@}:%s]" src

  let reporter () =
    let lock = CCLock.create() in
    Fmt.set_color_default true;
    let buf_out, buf_flush = buf_fmt () in
    let pp_header fmt header =
      let now = now_s() in
      Format.fprintf fmt "@[<2>[%a|t%d] %a@ "
        ISO8601.Permissive.pp_datetime now Thread.(id @@ self()) pp_h header
    in
    let out, err =
      match Sys.getenv "LOGS_FILE" with
      | "" -> stdout, stderr
      | file ->
        begin
          try
            let oc = open_out file in
            at_exit (fun () -> close_out_noerr oc);
            oc, oc
          with e ->
            Printf.eprintf "error: cannot open log file '%s': %s\n%!"
              file (Printexc.to_string e);
            stdout,stderr
          end
      | exception Not_found -> stdout, stderr
    in
    let report src level ~over k msgf =
      let k _ =
        let write_str out s =
          CCLock.with_lock lock @@ fun () ->
          Printf.fprintf out "%s%s%!" reset_line s
        in
        let msg = buf_flush() in
        write_str (match level with Logs.App -> out | _ -> err) msg;
        over ();
        k ()
      in
      msgf (fun ?header ?tags:_ fmt ->
          Format.kfprintf k buf_out ("%a@[" ^^ fmt ^^ "@]@.") pp_header (src, level, header))
    in
    { Logs.report = report }
end

(** Setup the logging infra *)
let setup_logs (lvl:Logs.level option) : unit =
  Logs.set_reporter (Log_report.reporter());
  Logs.set_level ~all:true lvl;
  Logs.debug (fun k->k "logs are setup");
  ()

let pp_list ?(sep=" ") f out l =
  let sep out () = Fmt.fprintf out "%s@," sep in
  Fmt.list ~sep f out l

module Pp = struct
  let pp_l = pp_list
  let pp_pair f g out (x,y) = Fmt.fprintf out "(@[%a@ %a@])" f x g y
  let pp_f what f out x = Fmt.fprintf out "@ (@[%s@ %a@])" what f x
  let pp_opt what f out = function
    | None -> ()
    | Some x -> Fmt.fprintf out "@ (@[%s@ %a@])" what f x
  let pp_l1 f out l = if l=[] then () else Fmt.fprintf out "@,%a" (pp_l f) l
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
let human_duration (f:float) : string =
  let nb_sec_minute = 60 in
  let nb_sec_hour = 60 * nb_sec_minute in
  let nb_sec_day = 24 * nb_sec_hour in
  let n = int_of_float f in
  if n >= 1 then (
    let aux n div = n / div, n mod div in
    let n_day, n = aux n nb_sec_day in
    let n_hour, n = aux n nb_sec_hour in
    let n_min, n = aux n nb_sec_minute in
    let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
    (print_aux "d" n_day) ^
    (print_aux "h" n_hour) ^
    (print_aux "m" n_min) ^
    (string_of_int n) ^
    (if f -. floor f >= 0.1 then (
        let s = Printf.sprintf "%.1f" (f -. floor f) in
        "." ^  (snd @@CCString.Split.left_exn ~by:"." s) (* remove the leading "0." *)
      ) else "")
    ^ "s"
  ) else (
    Printf.sprintf "%.3fs" f
  )

let pp_human_duration out f = Fmt.string out (human_duration f)

let human_datetime (f:float) : string =
  let t = Unix.gmtime f in
  let wday i = [|"Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"|].(i) in
  Printf.sprintf "%d/%02d, %s the %02d, at %dh%02d:%02d GMT"
    (1900+t.tm_year)
    (t.tm_mon+1)
    (wday t.tm_wday)
    t.tm_mday
    t.tm_hour t.tm_min t.tm_sec

let pp_human_datetime out f = Fmt.string out (human_datetime f)

(** Compact representation of a datetime *)
let datetime_compact (t:Ptime.t) : string =
  let (y,m,d), ((h,min,z),_tz) = Ptime.to_date_time t in
  Printf.sprintf "%4d%02d%02dT%02d%02d%02d" y m d h min z

(** Human readable size *)
let human_size (x:int) : string =
  if x >= 1_000_000 then Printf.sprintf "%d.%dM" (x / 1_000_000) ((x/1000) mod 1_000)
  else if x >= 1_000 then Printf.sprintf "%d.%dK" (x/1000) ((x/100) mod 10)
  else string_of_int x

(** ensure [s] is not too long *)
let truncate_left (n:int) (s:string) : string =
  if String.length s > n then "…" ^ String.sub s (String.length s-n+1) (n-1) else s
let truncate_right (n:int) (s:string) : string =
  if String.length s > n then String.sub s 0 (n-1) ^ "…" else s

(** Run command and get stdout *)
let get_cmd_out cmd =
  Logs.debug (fun k->k "get-cmd-out %S" cmd);
  CCUnix.with_process_in cmd
    ~f:(fun ic -> CCIO.read_all ic |> String.trim)

(** Extract the program part of a command *)
let get_binary_of_cmd (cmd:string) : string =
  let cmd = String.trim cmd in
  (try fst @@ CCString.Split.left_exn ~by:" " cmd
   with Not_found -> cmd)

let mk_abs_path (s:string) : string =
  match CCString.chop_prefix ~pre:"file://" s with
  | Some s -> s (* URIs from lsp *)
  | None ->
    if Filename.is_relative s then Filename.concat (Sys.getcwd()) s
    else s

(** Guess how many cores we have on the CPU *)
let guess_cpu_count () =
  try get_cmd_out "grep -c processor /proc/cpuinfo" |> int_of_string
  with _ -> 2

(** Ensure directory exists, recursively *)
let rec mkdir_rec (d:string) =
  if not (Sys.file_exists d) then (
    let d2 = Filename.dirname d in
    mkdir_rec d2;
    (try Unix.mkdir d 0o755
     with _ -> Logs.debug (fun k->k "mkdir %S failed" d));
  )

(** concatenate list into a path *)
let rec filename_concat_l = function
  | [] -> "."
  | [s] -> s
  | s :: tl -> Filename.concat s @@ filename_concat_l tl

let mk_uuid () : Uuidm.t =
  Uuidm.v4_gen (Random.State.make_self_init()) ()

(** Modify filename of a lexing buffer, for future locations *)
let set_lexbuf_filename buf filename =
  let open Lexing in
  buf.lex_curr_p <- {buf.lex_curr_p with pos_fname = filename}

(** Check whether the DB contains a table named [tbl] *)
let db_has_table db (tbl:string) : bool =
    try
      CCResult.is_ok @@ Db.check_ret () @@
      Sqlite3.finalize @@
      Sqlite3.prepare db (Printf.sprintf "select count(*) from %s" tbl)
    with _ -> false

let err_of_db (e:Db.Rc.t) : Error.t =
  Error.makef "DB error: %s" @@ Db.Rc.to_string e

let unwrap_str msg (x:('a, string) result) : 'a =
  match x with
  | Ok x -> x
  | Error e ->
    let err = Error.make e |> Error.wrap (msg()) in
    Error.raise err

let unwrap_db msg (x:('a,Db.Rc.t) result) : 'a =
  match x with
  | Ok x -> x
  | Error e ->
    let err = err_of_db e |> Error.wrap (msg()) in
    Error.raise err

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
    Logs.debug (fun k->k "par-map: create pool j=%d" j);
    let module P = CCPool.Make(struct
        let max_size = j
      end) in
    let res = match l with
      | [] -> []
      | _ ->
        P.Fut.map_l (fun x -> P.Fut.make1 f_with_sem x) l
        |> P.Fut.get
    in
    Logs.debug (fun k->k "par-map: stop pool");
    P.stop();
    res
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

let (//) = Filename.concat

let data_dir () = Xdg.data_dir () // !(Xdg.name_of_project)
let config_dir () = Xdg.config_dir () // !(Xdg.name_of_project)
let default_config () = config_dir() // "conf.sexp"

module Chrono : sig
  type t
  val start : unit -> t
  val since_last : t -> float
  val elapsed : t -> float
end = struct
  type t = {
    start: float;
    mutable last: float;
  }
  let start () : t =
    let t = now_s() in
    { start=t; last=t }
  let elapsed (self:t) =
    let now = now_s() in
    self.last <- now;
    now -. self.start
  let since_last (self:t) =
    let now = now_s() in
    let r = now -. self.last in
    self.last <- now;
    r
end

module Json = struct
  type t =
    [ `String of string
    | `List of t list
    | `Assoc of (string * t) list
    | `Int of int
    | `Float of float
    | `Null
    ]

  let rec pp out (self:t) : unit =
    match self with
    | `String s -> Fmt.fprintf out "%S" s
    | `List l -> Fmt.fprintf out "[@[%a@]]" (Fmt.list ~sep:(Fmt.return ",@ ") pp) l
    | `Assoc l ->
      let pp_pair out (s,v) =
        Fmt.fprintf out "@[<1>%S:@ %a@]" s pp v
      in
      Fmt.fprintf out "{@[%a@]}" (Fmt.list ~sep:(Fmt.return ",@ ") pp_pair) l
    | `Int i -> Fmt.int out i
    | `Float f -> Fmt.float out f
    | `Null -> Fmt.string out "null"

  let to_string (self:t) : string = Fmt.asprintf "%a" pp self
end

