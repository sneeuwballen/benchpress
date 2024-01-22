(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
module Str_map = CCMap.Make (String)
module Str_set = CCSet.Make (String)
module Db = Sqlite3_utils
module PB = PrintBox

let spf = Printf.sprintf
let _lock = CCLock.create ()
let now_s () = Ptime_clock.now () |> Ptime.to_float_s
let reset_line = "\x1b[2K\r"
let synchronized f = CCLock.with_lock _lock f

module Log_report = struct
  let buf_fmt () =
    let b = Buffer.create 512 in
    let fmt = Fmt.formatter_of_buffer b in
    Fmt.set_color_tag_handling fmt;
    let flush () =
      Format.fprintf fmt "@]@?";
      let m = Buffer.contents b in
      Buffer.reset b;
      m
    in
    fmt, flush

  let pp_h out (src, lvl, _) =
    let src = Logs.Src.name src in
    let src =
      if src = "application" then
        ""
      else
        src
    in
    match lvl with
    | Logs.Debug -> Fmt.fprintf out "[@{<black>debug@}:%s]" src
    | Logs.Info -> Fmt.fprintf out "[@{<cyan>info@}:%s]" src
    | Logs.Error -> Fmt.fprintf out "[@{<Red>error@}:%s]" src
    | Logs.Warning -> Fmt.fprintf out "[@{<yellow>warning@}:%s]" src
    | Logs.App -> Fmt.fprintf out "[@{<blue>app@}:%s]" src

  let reporter () =
    let lock = CCLock.create () in
    Fmt.set_color_default true;
    let buf_out, buf_flush = buf_fmt () in
    let pp_header fmt header =
      let now = now_s () in
      Format.fprintf fmt "@[<2>[%a|t%d] %a@ " ISO8601.Permissive.pp_datetime now
        Thread.(id @@ self ())
        pp_h header
    in
    let out, err =
      match Sys.getenv "LOGS_FILE" with
      | "" -> stdout, stderr
      | file ->
        (try
           let oc = open_out file in
           at_exit (fun () -> close_out_noerr oc);
           oc, oc
         with e ->
           Printf.eprintf "error: cannot open log file '%s': %s\n%!" file
             (Printexc.to_string e);
           stdout, stderr)
      | exception Not_found -> stdout, stderr
    in
    let report src level ~over k msgf =
      let k _ =
        let write_str out s =
          CCLock.with_lock lock @@ fun () ->
          Printf.fprintf out "%s%s%!" reset_line s
        in
        let msg = buf_flush () in
        write_str
          (match level with
          | Logs.App -> out
          | _ -> err)
          msg;
        over ();
        k ()
      in
      msgf (fun ?header ?tags:_ fmt ->
          Format.kfprintf k buf_out
            ("%a@[" ^^ fmt ^^ "@]@.")
            pp_header (src, level, header))
    in
    { Logs.report }
end

(** Setup the logging infra *)
let setup_logs (lvl : Logs.level option) : unit =
  Logs.set_reporter (Log_report.reporter ());
  Logs.set_level ~all:true lvl;
  Logs.debug (fun k -> k "logs are setup");
  ()

let pp_list ?(sep = " ") f out l =
  let sep out () = Fmt.fprintf out "%s@," sep in
  Fmt.list ~sep f out l

module Pp = struct
  let pp_l = pp_list
  let pp_pair f g out (x, y) = Fmt.fprintf out "(@[%a@ %a@])" f x g y
  let pp_f what f out x = Fmt.fprintf out "@ (@[%s@ %a@])" what f x

  let pp_opt what f out = function
    | None -> ()
    | Some x -> Fmt.fprintf out "@ (@[%s@ %a@])" what f x

  let pp_fl1 what f out = function
    | [] -> ()
    | l -> pp_f what (pp_l f) out l

  let pp_l1 f out l =
    if l = [] then
      ()
    else
      Fmt.fprintf out "@,%a" (pp_l f) l

  let pp_str out s = Sexp_loc.pp out (Sexp_loc.atom s)
  let pp_regex out r = Fmt.fprintf out "%S" r
end

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

let die_on_sigterm : unit -> unit =
  let thunk =
    lazy
      (Sys.set_signal 15
         (Sys.Signal_handle
            (fun _ ->
              print_endline "received sigterm, exiting";
              Unix.kill 0 15;
              (* kill children *)
              exit 1)))
  in
  fun () -> Lazy.force thunk

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

(** ensure [s] is not too long *)
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

(** Run command and get stdout *)
let get_cmd_out cmd =
  Logs.debug (fun k -> k "get-cmd-out %S" cmd);
  CCUnix.with_process_in cmd ~f:(fun ic -> CCIO.read_all ic |> String.trim)

(** Extract the program part of a command *)
let get_binary_of_cmd (cmd : string) : string =
  let cmd = String.trim cmd in
  try fst @@ CCString.Split.left_exn ~by:" " cmd with Not_found -> cmd

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
    with _ -> Logs.debug (fun k -> k "mkdir %S failed" d)
  )

(** [file_for_uuid pref ?dir ~timestamp uuid ext] builds a filename of the form
    "[pref]-[timestamp]-[uuid].[ext]" *)
let file_for_uuid pref ?(dir = Xdg.data_dir) ~timestamp uuid ext =
  let filename =
    Printf.sprintf "%s-%s-%s.%s" pref
      (match Ptime.of_float_s timestamp with
      | None -> Printf.sprintf "<time %.1fs>" timestamp
      | Some t -> datetime_compact t)
      (Uuidm.to_string uuid) ext
  in
  let data_dir = Filename.concat (dir ()) !Xdg.name_of_project in
  mkdir_rec data_dir;
  Filename.concat data_dir filename

(** concatenate list into a path *)
let rec filename_concat_l = function
  | [] -> "."
  | [ s ] -> s
  | s :: tl -> Filename.concat s @@ filename_concat_l tl

let mk_uuid () : Uuidm.t = Uuidm.v4_gen (Random.State.make_self_init ()) ()

(** Modify filename of a lexing buffer, for future locations *)
let set_lexbuf_filename buf filename =
  let open Lexing in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename }

(** Check whether the DB contains a table named [tbl] *)
let db_has_table db (tbl : string) : bool =
  try
    CCResult.is_ok @@ Db.check_ret () @@ Sqlite3.finalize
    @@ Sqlite3.prepare db (Printf.sprintf "select count(*) from %s" tbl)
  with _ -> false

let err_of_db (e : Db.Rc.t) : Error.t =
  Error.makef "DB error: %s" @@ Db.Rc.to_string e

let unwrap_str msg (x : ('a, string) result) : 'a =
  match x with
  | Ok x -> x
  | Error e ->
    let err = Error.make e |> Error.wrap (msg ()) in
    Error.raise err

let unwrap_db msg (x : ('a, Db.Rc.t) result) : 'a =
  match x with
  | Ok x -> x
  | Error e ->
    let err = err_of_db e |> Error.wrap (msg ()) in
    Error.raise err

(** Parallel map *)
module Par_map = struct
  (* map on the list with at most [j] parallel threads *)
  let map_p ~j f l =
    if j < 1 then invalid_arg "map_p: ~j";
    die_on_sigterm ();
    (* NOTE: for some reason the pool seems to spawn one too many thread
       in some cases. So we add a guard to respect [-j] properly. *)
    let sem = CCSemaphore.create j in
    let f_with_sem x = CCSemaphore.with_acquire ~n:1 sem ~f:(fun () -> f x) in
    Logs.debug (fun k -> k "par-map: create pool j=%d" j);
    let module P = CCPool.Make (struct
      let max_size = j
    end) in
    let res =
      match l with
      | [] -> []
      | _ -> P.Fut.map_l (fun x -> P.Fut.make1 f_with_sem x) l |> P.Fut.get
    in
    Logs.debug (fun k -> k "par-map: stop pool");
    P.stop ();
    res

  (* Map on the list [l] with each call to [f] being associated one of the
     resources from [resources] that is guaranteed not to be used concurrently
     by another call to [f]. *)
  let map_with_resource ~resources f l =
    match l with
    | [] -> []
    | _ ->
      if CCList.is_empty resources then
        invalid_arg "map_with_resource: ~resources";
      die_on_sigterm ();
      let jobs = List.length resources in
      let queue = CCBlockingQueue.create jobs in
      List.iter (CCBlockingQueue.push queue) resources;
      let f x =
        let resource = CCBlockingQueue.take queue in
        Fun.protect
          ~finally:(fun () -> CCBlockingQueue.push queue resource)
          (fun () -> f resource x)
      in
      Logs.debug (fun m -> m "par-map: create pool j=%d" jobs);
      let module P = CCPool.Make (struct
        let max_size = jobs
      end) in
      let res = P.Fut.map_l (P.Fut.make1 f) l |> P.Fut.get in
      Logs.debug (fun m -> m "par-map: stop pool");
      P.stop ();
      res
end

module Git = struct
  (* obtain the current commit name *)
  let get_commit (dir : string) : string =
    get_cmd_out (Printf.sprintf "git -C %s rev-parse HEAD" dir)

  let get_branch dir : string =
    get_cmd_out
      (Printf.sprintf "git -C %s branch | grep '*' | cut -d ' ' -f2" dir)
end

let ( // ) = Filename.concat
let data_dir () = Xdg.data_dir () // !Xdg.name_of_project
let config_dir () = Xdg.config_dir () // !Xdg.name_of_project
let default_config () = config_dir () // "conf.sexp"

module Chrono : sig
  type t

  val start : unit -> t
  val since_last : t -> float
  val elapsed : t -> float
end = struct
  type t = { start: float; mutable last: float }

  let start () : t =
    let t = now_s () in
    { start = t; last = t }

  let elapsed (self : t) =
    let now = now_s () in
    self.last <- now;
    now -. self.start

  let since_last (self : t) =
    let now = now_s () in
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
    | `Null ]

  let rec pp out (self : t) : unit =
    match self with
    | `String s -> Fmt.fprintf out "%S" s
    | `List l ->
      Fmt.fprintf out "[@[%a@]]" (Fmt.list ~sep:(Fmt.return ",@ ") pp) l
    | `Assoc l ->
      let pp_pair out (s, v) = Fmt.fprintf out "@[<1>%S:@ %a@]" s pp v in
      Fmt.fprintf out "{@[%a@]}" (Fmt.list ~sep:(Fmt.return ",@ ") pp_pair) l
    | `Int i -> Fmt.int out i
    | `Float f -> Fmt.float out f
    | `Null -> Fmt.string out "null"

  let to_string (self : t) : string = Fmt.asprintf "%a" pp self
end

(** [mk_shell_cmd ?options ?target exec] makes a command that executes [exec] \
    with the options [options] on the target [target]. *)
let mk_shell_cmd ?(options = []) ?target exec =
  let buf = Buffer.create 32 in
  Buffer.add_string buf exec;
  List.iter
    (fun (k, v_opt) ->
      Buffer.add_string buf
        (match v_opt with
        | Some v -> " " ^ k ^ " " ^ v
        | None -> " " ^ k))
    options;
  CCOption.iter (fun s -> Buffer.add_string buf (" " ^ s)) target;
  Buffer.contents buf

let pp_inet_addr fmt a = Format.fprintf fmt "%s" (Unix.string_of_inet_addr a)

let pp_unix_addr fmt addr =
  match addr with
  | Unix.ADDR_UNIX s -> Format.fprintf fmt "ADDR_UNIX %s" s
  | Unix.ADDR_INET (addr, id) ->
    Format.fprintf fmt "ADDR_INET (%a, %d)" pp_inet_addr addr id

let ip_addr_conv =
  Cmdliner.Arg.conv
    ( (fun str ->
        try Ok (Unix.inet_addr_of_string str) with Failure s -> Error (`Msg s)),
      fun fmt addr -> Format.fprintf fmt "%s" (Unix.string_of_inet_addr addr) )

let localhost_addr () =
  (Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)

let rec accept_non_intr s =
  try Unix.accept ~cloexec:true s
  with Unix.Unix_error (EINTR, _, _) -> accept_non_intr s

(** [mk_socket sockaddr] makes a socket, binds it to [sockaddr] and returns it
    along with the name of the service if it is a unix socket or the port number
    if it is an internet socket. *)
let mk_socket sockaddr =
  let open Unix in
  let sock = socket ~cloexec:true (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  sock, (Unix.getnameinfo (Unix.getsockname sock) []).ni_service

(** [start_server n server_fun sock] starts a server on the socket [sock],
    assumes that the socket is correcly bound to a valid address.
    Allows up to [n] connections and runs the function [server_fun] for each
    connection on a separate thread (uses threads, doesn't fork the process).*)
let start_server n server_fun sock =
  let open Unix in
  listen sock 5;
  CCThread.Arr.join
  @@ CCThread.Arr.spawn n (fun _ ->
         let s, _caller = accept_non_intr sock in
         let inchan = in_channel_of_descr s in
         let outchan = out_channel_of_descr s in
         server_fun inchan outchan)

(** [establish_server n server_fun sockaddr] same as
    [Unix.establish_server], but it uses threads instead of forking the process
    after each connection, and only accepts [n] connections  *)
let establish_server n server_fun sockaddr =
  let sock, _ = mk_socket sockaddr in
  start_server n server_fun sock

(* Not actually a ramdisk on windows *)
let ramdisk_ref =
  ref
    (if Sys.unix then
      Some "/dev/shm"
    else
      None)

let has_ramdisk () = Option.is_some !ramdisk_ref

let get_ramdisk () =
  match !ramdisk_ref with
  | Some ramdisk -> ramdisk
  | None -> Filename.get_temp_dir_name ()

let set_ramdisk ramdisk = ramdisk_ref := Some ramdisk

(* Turns out that [Filename.open_temp_file] is not thread-safe *)
let open_temp_file = CCLock.create Filename.open_temp_file

let open_ram_file prefix suffix =
  let temp_dir = get_ramdisk () in
  CCLock.with_lock open_temp_file @@ fun open_temp_file ->
  open_temp_file ~temp_dir prefix suffix

let with_ram_file prefix suffix f =
  let fname, oc = open_ram_file prefix suffix in
  Fun.protect
    ~finally:(fun () ->
      close_out oc;
      try Sys.remove fname with Sys_error _ -> ())
    (fun () -> f fname)

let with_copy_to_ram fname f =
  if has_ramdisk () then (
    let suffix = Filename.basename fname in
    let fname =
      CCIO.with_in ~flags:[ Open_binary ] fname @@ fun ic ->
      let fname, oc = open_ram_file "" suffix in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () ->
          CCIO.with_out ~flags:[ Open_binary ] fname @@ fun oc ->
          CCIO.copy_into ~bufsize:(64 * 1024) ic oc;
          fname)
    in
    Fun.protect
      ~finally:(fun () -> try Sys.remove fname with Sys_error _ -> ())
      (fun () -> f fname)
  ) else
    f fname

let with_copy_from_ram fname f =
  if has_ramdisk () then (
    let suffix = Filename.basename fname in
    let ram_fname, oc = open_ram_file "" suffix in
    Fun.protect
      ~finally:(fun () ->
        ( CCIO.with_in ~flags:[ Open_binary ] ram_fname @@ fun ic ->
          CCIO.copy_into ~bufsize:(64 * 1024) ic oc );
        close_out oc;
        Sys.remove ram_fname)
      (fun () -> f ram_fname)
  ) else
    f fname

let with_copy_from_ram_opt fname f =
  match fname with
  | Some fname -> with_copy_from_ram fname (fun fname -> f (Some fname))
  | None -> f None

let with_affinity cpu f =
  let aff = Processor.Affinity.get_ids () in
  Processor.Affinity.set_ids [ cpu ];
  Fun.protect ~finally:(fun () -> Processor.Affinity.set_ids aff) f

let with_affinity_opt cpu f =
  match cpu with
  | None -> f ()
  | Some cpu -> with_affinity cpu f
