(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
module Str_map = CCMap.Make(String)
module Str_set = CCSet.Make(String)
module E = CCResult
module Db = Sqlite3_utils
type 'a or_error = ('a, string) E.t

let _lock = CCLock.create()

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
    Fmt.set_color_default true;
    let app, app_flush = buf_fmt () in
    let dst, dst_flush = buf_fmt () in
    let pp_header fmt header =
      let now = Unix.gettimeofday() in
      Format.fprintf fmt "@[<2>[%a] %a@ "
        ISO8601.Permissive.pp_datetime now pp_h header
    in
    let report src level ~over k msgf =
      let k _ =
        begin match level with
          | Logs.App -> output_string stdout (app_flush ()); flush stdout
          | _ -> output_string stderr (dst_flush ()); flush stderr
        end;
        over ();
        k ()
      in
      msgf (fun ?header ?tags:_ fmt ->
        let ppf = if level = App then app else dst in
        Format.kfprintf k ppf ("%a@[" ^^ fmt ^^ "@]@.") pp_header (src, level, header))
    in
    { Logs.report = report }
end

(** Setup the logging infra *)
let setup_logs (lvl:Logs.level option) : unit =
  Logs.set_reporter (Log_report.reporter());
  Logs.set_level ~all:true lvl;
  ()

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

(** Human readable size *)
let human_size (x:int) : string =
  if x >= 1_000_000 then Printf.sprintf "%d.%dM" (x / 1_000_000) ((x/1000) mod 1_000)
  else if x >= 1_000 then Printf.sprintf "%d.%dK" (x/1000) ((x/100) mod 10)
  else string_of_int x

(* ensure [s] is not too long *)
let truncate_left (n:int) (s:string) : string =
  if String.length s > n then "…" ^ String.sub s (String.length s-n+1) (n-1) else s
let truncate_right (n:int) (s:string) : string =
  if String.length s > n then String.sub s 0 (n-1) ^ "…" else s
      
let get_cmd_out cmd =
  Logs.debug (fun k->k "get-cmd-out %S" cmd);
  CCUnix.with_process_in cmd
    ~f:(fun ic -> CCIO.read_all ic |> String.trim)

let mk_abs_path (s:string) : string =
  if Filename.is_relative s then Filename.concat (Sys.getcwd()) s
  else s

let guess_cpu_count () =
  try get_cmd_out "grep -c processor /proc/cpuinfo" |> int_of_string
  with _ -> 2

let mk_uuid () : Uuidm.t =
  Uuidm.v4_gen (Random.State.make_self_init()) ()

(** A scope for {!err_with} *)
type 'a try_scope = {
  unwrap: 'x. ('x,'a) result -> 'x;
  unwrap_with: 'x 'b. ('b -> 'a) -> ('x,'b) result -> 'x;
}

(** Open a local block with an [unwrap] to unwrap results *)
let err_with ?(map_err=fun e -> e) f : (_,string) result =
  let module E = struct exception Local of string end in
  let scope = {
    unwrap=(function Ok x -> x | Error e -> raise (E.Local e));
    unwrap_with=(fun ferr -> function Ok x -> x | Error e -> raise (E.Local (ferr e)));
  } in
  try
    let res = f scope in
    Ok res
      with
      | E.Local e -> Error (map_err e)
      | Db.Type_error d ->
        Error (map_err @@ Printf.sprintf "db type error on %s" (Db.Data.to_string_debug d))

(** Turn the DB error into a normal string error *)
let db_err ~ctx (x:(_,Db.Rc.t) result) : _ result =
  E.map_err (fun e -> Printf.sprintf "DB error in %s: %s" ctx @@ Db.Rc.to_string e) x

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
    let res =
      CCList.map (fun x -> P.Fut.make1 f_with_sem x) l
      |> P.Fut.sequence_l
      |> P.Fut.get
    in
    Logs.debug (fun k->k "par-map: stop pool");
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

let (//) = Filename.concat

let data_dir () = Xdg.data_dir () // !(Xdg.name_of_project)
let config_dir () = Xdg.config_dir () // !(Xdg.name_of_project)
let default_config () = config_dir() // "conf.sexp"
