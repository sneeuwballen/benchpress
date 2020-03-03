(* This file is free software. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

module E = CCResult
module J = Misc.Json
module PB = PrintBox
module Db = Sqlite3_utils
module Fmt = CCFormat

type result = Prover.name Run_result.t

type 'a or_error = ('a, string) CCResult.t

let fpf = Format.fprintf

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

let pp_list_ p =
  Fmt.within "(" ")"
    (Fmt.hovbox
       (Fmt.(list ~sep:(return "@ ") p)))

let pp_hvlist_ p =
  Fmt.within "(" ")"
    (Fmt.hvbox
       (Fmt.(list ~sep:(return "@ ") p)))

let time_of_res e = e.Run_result.raw.rtime

let pb_v_record ?bars l =
  PB.grid_l ?bars
    (List.map (fun (field,value) -> [PB.text field; value]) l)

let pb_int_color c n =
  let open PB in
  if n=0 then int n
  else text_with_style (Style.set_bold true c) (string_of_int n)

(* list provers from the main table *)
let list_provers db : string list or_error =
  Db.exec_no_params db
    {| select distinct prover from prover_res order by prover; |}
    ~ty:Db.Ty.(p1 text, id) ~f:Db.Cursor.to_list_rev
  |> Misc.db_err ~ctx:"listing provers"

(** {2 Basic stats on results} *)

module Stat = struct
  type t = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
    timeout: int;
    memory: int;
    total: int;
    total_time: float; (* for sat+unsat *)
  }

  let as_printbox_record s : _ list =
    let open PB in
    [ "sat", pb_int_color Style.(fg_color Green) s.sat;
      "unsat", pb_int_color Style.(fg_color Green) s.unsat;
      "errors", pb_int_color Style.(fg_color Cyan) s.errors;
      "unknown", int s.unknown;
      "timeout", int s.timeout;
      "memory", int s.memory;
      "total", int s.total;
      "total_time", line (Misc.human_duration s.total_time);
    ]

  let to_printbox (s:t) : PrintBox.t =
    pb_v_record @@ as_printbox_record s

  let to_printbox_l =
    List.map (fun ((p:string), a) -> p, to_printbox a)

  (* obtain stats for this prover *)
  let of_db_for ~(prover:Prover.name) (db:Db.t) : t or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading stat(%s) from DB: %s" prover)
      (fun scope ->
         let f c = Db.Cursor.next c |> CCOpt.to_result "no result" |> scope.unwrap in
         let get_res r =
           Logs.debug (fun k->k "get-res %s" r);
           Db.exec db
             {| select count(*) from prover_res where prover=? and res=?; |}
             prover r ~ty:Db.Ty.(p2 text text, p1 (nullable int), CCOpt.get_or ~default:0) ~f
           |> scope.unwrap_with Db.Rc.to_string
         in
         let sat = get_res "sat" in
         let unsat = get_res "unsat" in
         let unknown = get_res "unknown" in
         let timeout = get_res "timeout" in
         let memory = get_res "memory" in
         let errors = get_res "error" in
         let total = sat+unsat+unknown+timeout+memory+errors in
         let total_time =
           Db.exec db {|
          select sum(rtime) from prover_res where prover=? and res in ('sat', 'unsat');
            |} prover
             ~ty:Db.Ty.(p1 text, p1 (nullable float), CCOpt.get_or ~default:0.) ~f
           |> scope.unwrap_with Db.Rc.to_string
         in
         { sat; unsat; timeout; memory; unknown; errors; total; total_time; }
      )

  let of_db (db:Db.t) : (Prover.name * t) list or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading stats from DB: %s")
      (fun scope ->
         let provers = list_provers db |> scope.unwrap in
         List.map
           (fun p -> p, of_db_for db ~prover:p |> scope.unwrap)
           provers)

  let pp out (s:t) : unit =
    fpf out
      "(@[<hv>:unsat %d@ :sat %d@ :solved %d@ :errors %d@ :unknown %d@ \
       :timeout %d@ :total %d@ :total_time %.2f@])"
      s.unsat s.sat (s.sat + s.unsat) s.errors s.unknown s.timeout
      (s.unsat + s.sat + s.errors + s.unknown + s.timeout)
      s.total_time
end

(** {2 Basic analysis of results} *)

type path_linker = Problem.path -> PrintBox.t
type prover_path_linker = Prover.name -> Problem.path -> PrintBox.t
type prover_path_res_linker = Prover.name -> Problem.path -> res:string -> PrintBox.t

let default_linker path = PB.text path
let default_pp_linker _ path = default_linker path
let default_ppr_linker _ _ ~res = default_linker res

module Analyze : sig
  type t = {
    improved  : int;
    ok        : int;
    disappoint: int;
    bad       : int; (* mismatch *)
    bad_full  : (Problem.t * Res.t * float) list; (* always materialized *)
    errors    : int;
    errors_full : (Problem.t * Res.t * float) list;
    total     : int;
  }

  val of_db_for : Db.t -> prover:Prover.name -> t or_error
  val of_db : Db.t -> (Prover.name * t) list or_error

  val to_printbox : t -> PrintBox.t
  val to_printbox_l : (Prover.name * t) list -> (string*PrintBox.t) list
  val to_printbox_bad : ?link:path_linker -> t -> PrintBox.t
  val to_printbox_bad_l :
    ?link:prover_path_linker ->
    (Prover.name * t) list -> (string*string list*PrintBox.t) list
  val to_printbox_errors : ?link:path_linker -> t -> PrintBox.t
  val to_printbox_errors_l :
    ?link:prover_path_linker ->
    (Prover.name * t) list -> (string*string list*PrintBox.t) list

  val is_ok : t -> bool

  val num_failed : t -> int

  val pp : t Fmt.printer
  val pp_bad : t Fmt.printer
end = struct
  type t = {
    improved  : int;
    ok        : int;
    disappoint: int;
    bad       : int; (* mismatch *)
    bad_full  : (Problem.t * Res.t * float) list; (* always materialized *)
    errors    : int;
    errors_full : (Problem.t * Res.t * float) list;
    total     : int;
  }

  let of_db_for (db:Db.t) ~prover : t or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading analyze(%s) from DB: %s" prover)
      (fun scope ->
         let get1_int ~ctx q ~ty p =
           Db.exec db q ~ty p
             ~f:(fun c ->
                 Db.Cursor.next c
                 |> CCOpt.to_result ("expected a result in "^ctx)
                 |> scope.unwrap)
           |> scope.unwrap_with Db.Rc.to_string
         in
         let ok =
           get1_int ~ctx:"get ok results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and res=file_expect and file_expect in ('sat','unsat'); |}
             prover
         and disappoint =
           get1_int ~ctx:"get disappoint results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and not (res in ('sat','unsat'))
                and file_expect in ('sat','unsat'); |}
             prover
         and improved =
           get1_int ~ctx:"get improved results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and res in ('sat','unsat')
                and not (file_expect in ('sat','unsat')); |}
             prover
         and total =
           get1_int ~ctx:"get total results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?;|} prover
         and bad =
           get1_int ~ctx:"get bad results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and res in ('sat','unsat')
                and res != file_expect
                and file_expect in ('sat','unsat'); |}
             prover
         and bad_full =
           Db.exec db
             {| select file, res, file_expect, rtime from prover_res
              where prover=? and res != file_expect and res in ('sat','unsat')
              and file_expect in ('sat','unsat'); |}
             prover
             ~ty:Db.Ty.(p1 text, p4 text text text float,
                        (fun file res expected t ->
                           Problem.make file (Res.of_string expected), Res.of_string res, t))
             ~f:Db.Cursor.to_list_rev
           |> scope.unwrap_with Db.Rc.to_string
         and errors_full =
           Db.exec db
             ~ty:Db.Ty.(p1 text, p4 text text text float,
                        (fun file res expected t ->
                           Problem.make file (Res.of_string expected), Res.of_string res, t))
             {| select file, res, file_expect, rtime from prover_res where prover=?
                and res in ('error') ; |}
             prover ~f:Db.Cursor.to_list_rev
           |> scope.unwrap_with Db.Rc.to_string
         in
         let errors = List.length errors_full in
         { ok; disappoint; improved; bad; bad_full; errors; errors_full; total; })

  (* TODO: create a function for "better"
        Sqlite3.create_fun2
  *)

  let of_db db : _ list or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading top-res from DB: %s")
      (fun scope ->
         let provers = list_provers db |> scope.unwrap in
         List.map (fun p -> p, of_db_for db ~prover:p |> scope.unwrap) provers)

  (* build statistics and list of mismatch from raw results *)

  let is_ok r = r.bad = 0
  let num_failed r = r.bad

  let to_printbox (r:t) : PrintBox.t =
    let open PB in
    let {improved; disappoint; ok; bad; total; errors; errors_full=_; bad_full=_} = r in
    let fields = [
      "improved", pb_int_color Style.(fg_color Green) improved;
      "ok", pb_int_color Style.(fg_color Green) ok;
      "disappoint", pb_int_color Style.(fg_color Blue) disappoint;
      "bad", pb_int_color Style.(fg_color Red) bad;
      "errors", pb_int_color Style.(fg_color Cyan) errors;
      "total", int total;
    ] in
    pb_v_record ~bars:true fields

  let to_printbox_l =
    List.map (fun (p, a) -> p, to_printbox a)

  let to_printbox_bad ?link:(mk_link=default_linker) r : PrintBox.t =
    let open PB in
    if r.bad <> 0 then (
      let l =
        List.map
          (fun (pb, res, t) ->
             [ mk_link pb.Problem.name;
               text (Res.to_string res);
               text (Res.to_string pb.Problem.expected);
               text (Misc.human_duration t);
             ])
          r.bad_full
      in
      let header =
        let tb = text_with_style Style.bold in
        [tb "problem"; tb "res"; tb "expected"; tb "time"] in
      grid_l (header :: l)
    ) else empty

  let to_printbox_bad_l ?(link=default_pp_linker) =
    CCList.filter_map
      (fun ((p:string), a) ->
         if a.bad = 0 then None
         else Some (p, List.map (fun (pb,_,_) -> pb.Problem.name) a.bad_full,
                    to_printbox_bad ~link:(link p) a))

  let to_printbox_errors ?link:(mk_link=default_linker) r : PrintBox.t =
    let open PB in
    if r.errors <> 0 then (
      let l =
        List.map
          (fun (pb, res, t) ->
             [ mk_link pb.Problem.name;
               text (Res.to_string res);
               text (Res.to_string pb.Problem.expected);
               text (Misc.human_duration t);
             ])
          r.errors_full
      in
      let header =
        let tb = text_with_style Style.bold in
        [tb "problem"; tb "res"; tb "expected"; tb "time"] in
      grid_l (header :: l)
    ) else empty

  let to_printbox_errors_l ?(link=default_pp_linker) =
    CCList.filter_map
      (fun ((p:string), a) ->
         if a.errors = 0 then None
         else Some (p, List.map (fun (pb,_,_) -> pb.Problem.name) a.errors_full,
                    to_printbox_errors ~link:(link p) a))

  let pp_bad out self =
    if self.bad <> 0 then (
      Format.fprintf out "@[<hv1>bad@ %a@]"
        PrintBox_text.pp (to_printbox_bad self)
    )

  let pp out self : unit =
    let pp_z_or_err out d =
      if d=0 then Fmt.int out d
      else Fmt.(with_color "Red" int) out d
    in
    Format.fprintf out
      "(@[<hv>:ok %d@ :improved %d@ :disappoint %d@ :bad %a@ :total %d@])%a"
      self.ok self.improved self.disappoint
      pp_z_or_err self.bad
      self.total
      pp_bad self
end

(** {2 Lightweight Comparison between bench runs} *)

module Comparison_short : sig
  type t = {
    better: int;
    worse: int;
    same: int;
  }

  val of_db : Db.t -> (Prover.name * Prover.name * t) list or_error

  val to_printbox : Prover.name -> Prover.name -> t -> PrintBox.t
  val to_printbox_l : (Prover.name * Prover.name * t) list -> (string*string*PrintBox.t) list

  (* TODO: a grid like display (pv1\pv2) *)
end = struct
  type t = {
    better: int;
    worse: int;
    same: int;
  }

  let of_db db : _ or_error =
    (* get a single integer *)
    let db_get db s x1 x2 =
      Db.exec db s x1 x2 ~ty:Db.Ty.(p2 text text, p1 int, id) ~f:Db.Cursor.to_list_rev
      |> Misc.db_err ~ctx:"extract comparison"
      |> E.flat_map (function [x] -> Ok x | _ -> Error "expected a single integer")
    in
    Misc.err_with
      ~map_err:(Printf.sprintf "comparison-short.of_db %s")
      (fun scope ->
         let provers = list_provers db |> scope.unwrap in
         CCList.diagonal provers
         |> List.rev_map
           (fun (p1,p2) ->
              assert (p1 <> p2);
              let better =
                db_get db
                  {|select count(r1.file) from prover_res r1, prover_res r2
                    where r1.prover=? and r2.prover=? and r1.file=r2.file
                    and r1.res in ('sat','unsat') and not (r2.res in ('sat','unsat')); |}
                  p1 p2 |> scope.unwrap
              and worse =
                db_get db
                  {|select count(r1.file) from prover_res r1, prover_res r2
                    where r1.prover=? and r2.prover=? and r1.file=r2.file
                    and not (r1.res in ('sat','unsat')) and (r2.res in ('sat','unsat')); |}
                  p1 p2 |> scope.unwrap
              and same =
                db_get db
                  {|select count(r1.file) from prover_res r1, prover_res r2
                    where r1.prover=? and r2.prover=? and r1.file=r2.file
                    and r1.res in ('sat','unsat') and (r2.res in ('sat','unsat')); |}
                  p1 p2 |> scope.unwrap
              in
              p1, p2, {better; worse; same}
           )
      )

  let to_printbox p1 p2 (self:t) : PB.t =
    let open PB in
    pb_v_record [
      "better for " ^ p1, int self.better;
      "better for " ^ p2, int self.worse;
      "same", int self.same;
    ]

  let to_printbox_l l = List.map (fun (p1,p2,r) -> p1, p2, to_printbox p1 p2 r) l

  (* TODO: grid display (-> to array, then by index + reverse when i<j?)
     let to_printbox_grid l : PB.t =
  *)
end

type metadata = {
  uuid: Uuidm.t; (* unique ID *)
  timestamp: float option; (* timestamp *)
  total_wall_time: float option;
  n_results: int;
  provers: Prover.name list;
}

module Metadata = struct
  type t = metadata

  let to_printbox ?link:(mk_link=default_linker) self : PB.t =
    let open PB in
    pb_v_record [
      "provers", vlist_map mk_link self.provers;
      "n_results", int self.n_results;
      "uuid", text @@ Uuidm.to_string self.uuid;
      "timestamp", (match self.timestamp with
          | None -> text "none"
          | Some f -> text @@ Misc.human_datetime f);
      "total_wall_time", (match self.total_wall_time with
          | None -> text "none" | Some f -> text @@ Misc.human_duration f);
    ]

  let db_prepare (db:Db.t) : _ or_error =
    Db.exec0 db {|
        create table if not exists
        meta(
          key text not null unique,
          value blob
          );
        create index if not exists meta_k on meta(key);
        |} |> Misc.db_err ~ctx:"top-res.db-prepare"

  let get_meta db k : _ =
    Db.exec_exn db {|select value from meta where key=? ;|}
      k
      ~ty:Db.Ty.(p1 text, p1 (nullable any_str), id)
      ~f:Db.Cursor.next
    |> CCOpt.to_result ("did not find metadata " ^ k)

  let to_db (db:Db.t) (self:t) : unit or_error =
    Db.exec_no_cursor db
      "insert or replace into meta values
      ('timestamp', ?), ('total-wall-time', ?), ('uuid', ?);"
      ~ty:Db.Ty.(p3 (nullable blob) (nullable blob) text)
      (CCOpt.map string_of_float self.timestamp)
      (CCOpt.map string_of_float self.total_wall_time)
      (Uuidm.to_string self.uuid)
    |> Misc.db_err ~ctx:"inserting metadata"

  let of_db db : t or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading metadata: %s")
      (fun scope ->
         let timestamp = get_meta db "timestamp" |> scope.unwrap in
         let total_wall_time = get_meta db "total-wall-time" |> scope.unwrap in
         let timestamp = CCOpt.map float_of_string timestamp in
         let uuid = get_meta db "uuid" |> scope.unwrap
                    |> CCOpt.flat_map Uuidm.of_string
                    |> CCOpt.to_result "no uuid found in DB"
                    |> scope.unwrap in
         let total_wall_time = CCOpt.map float_of_string total_wall_time in
         let n_results =
           Db.exec_no_params_exn db "select count(*) from prover_res;"
             ~ty:Db.Ty.(p1 int,id) ~f:Db.Cursor.next
           |> CCOpt.to_result "no prover results" |> scope.unwrap
         in
         let provers =
           Db.exec_no_params_exn db "select distinct name from prover;"
             ~f:Db.Cursor.to_list_rev ~ty:Db.Ty.(p1 any_str, id)
         in
         { timestamp; total_wall_time; uuid; n_results; provers; })

  let pp_l out (self:t) : unit =
    Fmt.fprintf out "@[<v>n-results: %d@ provers: [%s]@ timestamp: %s@ total-time: %s@ uuid: %a@]"
      self.n_results (String.concat ";" self.provers)
      (CCOpt.map_or ~default:"<no time>" Misc.human_datetime self.timestamp)
      (CCOpt.map_or ~default:"<no wall time>" Misc.human_duration self.total_wall_time)
      Uuidm.pp self.uuid

  let to_string self = Fmt.asprintf "%a" pp_l self
end

(** {2 Lightweight Results} *)

(** A kind of lightweight result *)
type compact_result = {
  cr_meta: metadata;
  cr_stat: (Prover.name * Stat.t) list;
  cr_analyze: (Prover.name * Analyze.t) list;
  cr_comparison: (Prover.name * Prover.name * Comparison_short.t) list;
}

module Compact_result = struct
  type t = compact_result

  let of_db db : t or_error =
    let open E.Infix in
    Metadata.of_db db >>= fun cr_meta ->
    Stat.of_db db >>= fun cr_stat ->
    Analyze.of_db db >>= fun cr_analyze ->
    Comparison_short.of_db db >>= fun cr_comparison ->
    Ok {cr_stat; cr_analyze; cr_comparison; cr_meta; }

  let pp out _self = Fmt.fprintf out "<compact result>"
end

(** {2 Cactus plots} *)

module Cactus_plot : sig
  type t

  val of_db : Db.t -> t or_error
  val of_file : string -> t or_error

  val show : t -> unit
  val save_to_file : t -> string -> unit
  val to_png : t -> string
end = struct
  module Gp = Gnuplot

  type t = {
    lines: (Prover.name * float list) list;
  }

  let of_db db =
    Misc.err_with
      ~map_err:(Printf.sprintf "while plotting DB: %s")
      (fun scope ->
         let provers = list_provers db |> scope.unwrap in
         Logs.debug (fun k->k "provers: [%s]" (String.concat ";" provers));
         let get_prover prover =
           Db.exec db
             {| select rtime from prover_res
              where prover=? and res in ('sat','unsat')
              order by rtime|} prover
             ~ty:Db.Ty.(p1 text, p1 float, id) ~f:Db.Cursor.to_list
           |> scope.unwrap_with Db.Rc.to_string
         in
         let lines = List.map (fun p -> p, get_prover p) provers in
         { lines }
      )

  let of_file file : t or_error =
    try
      Db.with_db ~timeout:500 ~mode:`READONLY file of_db
    with e -> E.of_exn_trace e

  let to_gp ~output self =
    Gp.with_ (fun gp ->
        let series =
          self.lines
          |> List.map
            (fun (prover,l) ->
               let l =
                 let sum = ref 0. in
                 List.mapi
                   (fun i rtime ->
                      sum := !sum +. rtime;
                      (float i, !sum))
                   l
               in
               Gp.Series.linespoints_xy ~title:prover l)
        in
        Gp.plot_many
          ~labels:(Gp.Labels.create ~y:"time (s)" ~x:"problems solved (accumulated)" ())
          ~title:"cumulative time for n° of problems solved" gp series ~output);
    ()

  let show (self:t) =
    to_gp self ~output:(Gp.Output.create `X11)

  let save_to_file (self:t) file =
    to_gp self ~output:(Gp.Output.create ~size:(1800,1024) @@ `Png file)

  let to_png (self:t) : string =
    CCIO.File.with_temp ~prefix:"benchpress_plot" ~suffix:".png"
      (fun file ->
         Logs.debug (fun k->k "plot into file %s" file);
         save_to_file self file;
         let s = CCIO.with_in file CCIO.read_all in
         Logs.debug (fun k->k "read %d bytes from file" (String.length s));
         s)
end

(** {2 Top Result}

    Main result of testing: a snapshot of the work done, + the analysis
    per prover *)

type top_result = {
  meta: metadata;
  events: Run_event.t list;
  stats: (Prover.name * Stat.t) list;
  analyze: (Prover.name * Analyze.t) list;
  db: Db.t; (* in-memory database *)
}

module Top_result : sig
  type t = top_result

  val pp : t Fmt.printer
  (** Full printer, including results *)

  val pp_header : t Fmt.printer
  (** Print only meta-information: UUID and timestamp *)

  val pp_compact : t Fmt.printer
  (** Print meta-informations + compact results *)

  val pp_bad : t Fmt.printer

  val is_ok : t -> bool

  val make :
    meta:metadata ->
    Prover.name Run_result.t list ->
    t or_error
  (** Make from a list of results *)

  val of_db : Db.t -> t or_error
  (** Parse from a DB *)

  val db_prepare : Db.t -> unit or_error

  val to_db : Db.t -> t -> unit or_error
  (** Dump into the DB *)

  val stat : t -> (Prover.name * Stat.t) list
  (** Compute or retrieve stats *)

  val analyze : t -> (Prover.name * Analyze.t) list

  val to_compact_result : t -> compact_result or_error

  (* TODO: move to another file
     type comparison_result = {
     both: ResultsComparison.t MStr.t;
     left: Analyze.t MStr.t;
     right: Analyze.t MStr.t;
     }

     val compare : t -> t -> comparison_result

     val pp_comparison : comparison_result Fmt.printer
     val comparison_to_printbox : ?short:bool -> comparison_result -> PrintBox.t
  *)

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  val to_table : ?provers:string list -> t -> table

  val table_to_csv : table -> Csv.t

  val table_to_printbox :
    ?link_pb:path_linker -> ?link_res:prover_path_res_linker ->
    table -> PrintBox.t

  val to_printbox_summary : t -> (string * PrintBox.t) list
  val to_printbox_stat : t -> (string * PrintBox.t) list

  val to_printbox_table :
    ?link_pb:path_linker -> ?link_res:prover_path_res_linker ->
    t -> PrintBox.t

  val to_printbox_bad : t -> (string * PrintBox.t) list
  val to_printbox_errors : t -> (string * PrintBox.t) list

  val to_csv : ?provers:string list -> t -> Csv.t

  val to_csv_chan : ?provers:string list -> out_channel -> t -> unit

  val to_csv_string : ?provers:string list -> t -> string

  val to_csv_file : ?provers:string list -> string -> t -> unit
  (** Write as CSV into given file *)
end = struct
  type t = top_result

  let analyze self = self.analyze
  let stat self = self.stats

  let is_ok self = List.for_all (fun (_,a) -> Analyze.is_ok a) @@ analyze self

  let pp_header out (self:t) : unit =
    Format.fprintf out "(@[(uuid %s)(date %a)@])"
      (Uuidm.to_string self.meta.uuid)
      (Misc.Pp.pp_opt "timestamp" Misc.pp_human_datetime) self.meta.timestamp

  let pp_compact out (self:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp res
    in
    let {analyze=a; _} = self in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header self (pp_list_ pp_tup) a

  let pp_bad out (r:t) : unit =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp_bad res
    in
    let a = analyze r in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header r (pp_list_ pp_tup) a

  let pp out (self:t) : unit =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp res
    in
    let a = analyze self in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header self (pp_list_ pp_tup) a

  let to_compact_result (self:t) : compact_result or_error =
    let open E.Infix in
    Comparison_short.of_db self.db >>= fun cr_comparison ->
    let cr_analyze = analyze self in
    let cr_stat = stat self in
    E.return {cr_analyze; cr_meta=self.meta; cr_stat; cr_comparison; }

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  let to_table ?provers (self:t): table =
    let line0 =
      Printf.sprintf "(snapshot :uuid %s :date %s)"
        (Uuidm.to_string self.meta.uuid)
        (CCOpt.map_or ~default:"<none>" Misc.human_datetime self.meta.timestamp)
    in
    Misc.err_with
      ~map_err:(Printf.sprintf "while converting to CSV table: %s")
      (fun scope ->
         let provers =
           match provers with
           | Some l -> l
           | None -> list_provers self.db |> scope.unwrap
         in
         let files = Db.exec_no_params self.db
             {| select distinct file from prover_res; |}
             ~ty:Db.Ty.(p1 text, id) ~f:Db.Cursor.to_list_rev
                     |> scope.unwrap_with Db.Rc.to_string
         in
         let t_rows =
           List.rev_map
             (fun file ->
                let tr_res =
                  Db.exec self.db
                    {| select prover, res, rtime from
                   prover_res where file=? order by prover ; |}
                    file
                    ~ty:Db.Ty.(p1 text, p3 text text float,
                               fun prover res t ->
                                 prover, Res.of_string res, t)
                    ~f:Db.Cursor.to_list_rev
                  |> scope.unwrap_with Db.Rc.to_string
                  |> List.filter (fun (p,_,_) -> List.mem p provers)
                in
                {tr_problem=file; tr_res})
             files
         in
         {t_meta=line0; t_provers=provers; t_rows}
      )
    |> (function
        | Ok x -> x
        | Error e ->
          Logs.err (fun k->k "conversion to CSV failed: %s" e);
          failwith ("error while converting to CSV: " ^ e))

  let time_to_csv (_:Res.t) f = Printf.sprintf "%.2f" f
  let res_to_csv (r:Res.t) = match r with
    | Res.Error -> "error"
    | Res.Timeout -> "timeout"
    | Res.Unknown -> "unknown"
    | Res.Sat -> "sat"
    | Res.Unsat -> "unsat"

  let table_to_csv (t:table): Csv.t =
    let header_line =
      "problem" ::
      t.t_provers @
      (List.map (fun p -> p ^ ".time") t.t_provers)
    in
    let lines =
      List.map
        (fun r ->
           r.tr_problem
           :: List.map (fun (_,res,_) ->  res_to_csv res) r.tr_res
           @ List.map (fun (_,res,t) -> time_to_csv res t) r.tr_res)
        t.t_rows
    in
    header_line :: lines

  let to_csv ?provers t : Csv.t =
    to_table ?provers t |> table_to_csv

  let table_to_printbox
      ?(link_pb=default_linker)
      ?(link_res=default_ppr_linker)
      (self:table) : PB.t =
    let header_line =
      List.map PB.(text_with_style Style.bold) @@
      "problem" ::
      self.t_provers @
      (List.map (fun p -> p ^ ".time") self.t_provers)
    in
    let lines =
      List.map
        (fun r ->
           link_pb r.tr_problem
           :: List.map (fun (prover,res,_) ->
               link_res prover r.tr_problem ~res:(res_to_csv res)) r.tr_res
           @ List.map (fun (_,res,t) -> PB.text @@ time_to_csv res t) r.tr_res)
        self.t_rows
    in
    PB.grid_l (header_line::lines)

  let to_printbox_stat (self:t) : (_ * PB.t) list =
    let a = stat self in
    List.map (fun (p, st) -> p, Stat.to_printbox st) a

  let to_printbox_summary (self:t) : (_ * PB.t) list =
    let a = analyze self in
    Analyze.to_printbox_l a

  let to_printbox_table ?link_pb ?link_res self =
    table_to_printbox ?link_pb ?link_res @@ to_table self

  let to_printbox_bad self =
    let a = analyze self in
    CCList.filter_map
      (fun (p, a) ->
         if a.Analyze.bad = 0 then None
         else Some (p, Analyze.to_printbox_bad a))
      a

  let to_printbox_errors self =
    let a = analyze self in
    CCList.map
      (fun (p, a) -> (p, Analyze.to_printbox_errors a))
      a

  (* TODO
     let comparison_to_printbox ?(short=true) (self:comparison_result) : PB.t =
     let open PB in
     let pm side m =
      MStr.to_list m
      |> List.map
        (fun (p,c) ->
           [text_with_style Style.bold (p ^ " ("^side^")");
            Analyze.to_printbox c])
     in
     grid_l @@ List.flatten [
      [
        MStr.to_list self.both
        |> List.map
          (fun (p,c) ->
             hlist [text_with_style Style.bold (p ^" (both)");
                    if short
                    then ResultsComparison.to_printbox_short c
                    else ResultsComparison.to_printbox c;
                   ])
      ];
      pm "left" self.left;
      pm "right" self.right;
     ]
  *)

  let to_csv_chan ?provers oc t =
    let chan = Csv.to_channel oc in
    Csv.output_all chan (to_csv ?provers t)

  let to_csv_file ?provers file t =
    let oc = open_out file in
    to_csv_chan ?provers oc t;
    close_out oc

  let to_csv_string ?provers t =
    let buf = Buffer.create 256 in
    let ch = Csv.to_buffer buf in
    Csv.output_all ch (to_csv ?provers t);
    Buffer.contents buf

  let db_prepare (db:Db.t) : _ or_error =
    let open E.Infix in
    Metadata.db_prepare db >>= fun () ->
    Run_event.db_prepare db >>= fun () ->
    Ok ()

  let to_db (db:Db.t) (self:t) : unit or_error =
    Logs.info (fun k->k "dump top-result into DB");
    Misc.err_with ~map_err:(Printf.sprintf "while dumping top-res to DB: %s")
      (fun scope ->
         scope.unwrap @@ db_prepare db;
         (* insert within one transaction, much faster *)
         scope.unwrap @@ Metadata.to_db db self.meta;
         Db.transact db (fun _ ->
             List.iter (fun ev -> scope.unwrap @@ Run_event.to_db db ev) self.events);
         ())

  let to_db_events_ (db:Db.t) (events:Run_event.t list) : unit or_error =
    Misc.err_with (fun scope ->
        scope.unwrap @@ db_prepare db;
        scope.unwrap @@ Run_event.db_prepare db;
        (* insert within one transaction, much faster *)
        Db.transact db (fun _ ->
            List.iter (fun ev -> scope.unwrap @@ Run_event.to_db db ev) events);
        ())

  let make ~meta
      (l:Prover.name Run_result.t list) : t or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "making top result: %s")
      (fun scope ->
         let l = List.rev_map Run_event.mk_prover l in
         (* create a temporary in-memory DB *)
         let db = Sqlite3.db_open ":memory:" in
         db_prepare db |> scope.unwrap;
         to_db_events_ db l |> scope.unwrap;
         Logs.debug (fun k->k "computing stats");
         let stats = Stat.of_db db |> scope.unwrap in
         Logs.debug (fun k->k "computing analyze");
         let analyze = Analyze.of_db db |> scope.unwrap in
         Logs.debug (fun k->k "done");
         { db; events=l; meta; stats; analyze; })

  let of_db (db:Db.t) : t or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading top-res from DB: %s")
      (fun scope ->
         Logs.debug (fun k->k "loading metadata from DB");
         let meta = Metadata.of_db db |> scope.unwrap in
         Logs.debug (fun k->k "loading events from DB");
         let events =
           Run_event.of_db_l db |> scope.unwrap
         in
         make ~meta events |> scope.unwrap)
end

module Detailed_res : sig
  type key = {
    prover: Prover.name;
    file: string;
    res: Res.t;
    file_expect: Res.t;
    rtime: float;
  }
  (** Summary of a result *)

  val list_keys : Db.t -> key list or_error
  (** List available results *)

  type t = Prover.t Run_result.t
  (** Detailed result *)

  val to_printbox : ?link:prover_path_linker -> t -> PrintBox.t * string * string
  (** Display an individual result + stdout + stderr *)

  val get_res : Db.t -> Prover.name -> string -> t or_error
  (** Get an individual result *)
end = struct
  type key = {
    prover: Prover.name;
    file: string;
    res: Res.t;
    file_expect: Res.t;
    rtime: float;
  }
  type t = Prover.t Run_result.t

  let list_keys db : key list or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "when listing detailed results: %s")
      (fun scope ->
         let l =
           Db.exec_no_params db
             {|select distinct prover, file, res, file_expect, rtime
              from prover_res order by file, prover desc; |}
             ~ty:Db.Ty.(p5 text any_str text text float,
                        fun prover file res file_expect rtime ->
                          let res=Res.of_string res in
                          let file_expect=Res.of_string file_expect in
                          {prover;file;res;file_expect;rtime})
             ~f:Db.Cursor.to_list_rev
           |> scope.unwrap_with Db.Rc.to_string
         in
         l
      )

  let get_res db prover file : _ or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "when get detailed result for %s on %s: %s" prover file)
      (fun scope ->
         let res: Prover.name Run_result.t =
           Db.exec db
             {|select
                  res, file_expect, timeout, errcode, stdout, stderr,
                  rtime, utime, stime
               from prover_res where prover=? and file=?; |}
             prover file
             ~f:Db.Cursor.next
             ~ty:Db.Ty.(p2 text text,
                        p2 text text @>> p4 int int any_str any_str
                        @>> p3 float float float,
                        fun res file_expect timeout errcode stdout stderr
                          rtime utime stime ->
                          Run_result.make prover ~timeout ~res:(Res.of_string res)
                            {Problem.name=file; expected=Res.of_string file_expect}
                            { Proc_run_result.errcode;stdout;stderr;rtime;utime;stime}
                       )
           |> scope.unwrap_with Db.Rc.to_string
           |> CCOpt.to_result "expected a result"
           |> scope.unwrap
         in
         let prover = Prover.of_db db prover |> scope.unwrap in
         Run_result.map ~f:(fun _ -> prover) res
      )

  let to_printbox ?link:(mk_link=default_pp_linker) (self:t) : PB.t*string*string =
    let open PB in
    let pp_res r =
      match r with
      | Res.Sat | Res.Unsat -> text_with_style Style.(set_fg_color Green@@bold) @@ Res.to_string r
      | Res.Error -> text_with_style Style.(set_fg_color Red@@bold) @@ Res.to_string r
      | _ -> text @@ Res.to_string r
    in
    v_record [
      "prover.name", text self.program.Prover.name;
      "prover.cmd", text self.program.Prover.cmd;
      "prover.version",
      (text @@ Format.asprintf "%a" Prover.Version.pp self.program.Prover.version);
      "prover.sat", text (CCOpt.get_or ~default:"<none>" self.program.Prover.sat);
      "prover.unsat", text (CCOpt.get_or ~default:"<none>" self.program.Prover.unsat);
      "prover.unknown", text (CCOpt.get_or ~default:"<none>" self.program.Prover.unknown);
      "prover.timeout", text (CCOpt.get_or ~default:"<none>" self.program.Prover.timeout);
      "prover.memory", text (CCOpt.get_or ~default:"<none>" self.program.Prover.memory);
      "problem.path", mk_link self.program.Prover.name self.problem.Problem.name;
      "problem.expected_res", pp_res self.problem.Problem.expected;
      "res", pp_res self.res;
      "rtime", text (Misc.human_duration self.raw.rtime);
      "stime", text (Misc.human_duration self.raw.stime);
      "utime", text (Misc.human_duration self.raw.utime);
      "errcode", int self.raw.errcode;
    ],
    self.raw.stdout, self.raw.stderr
end

(** {2 Benchmark, within one Top Result} *)

(* TODO
   module Bench : sig
   type per_prover = {
    stat: Stat.t;
    sat: (string * float) list;
    unsat: (string * float) list;
   }

   type t = {
    from: top_result;
    per_prover: (Prover.name * per_prover) list;
   }

   val make : top_result -> t

   val pp : t Fmt.printer
   (** Full printer that compares provers against one another *)
   end = struct
   type per_prover = {
    stat: Stat.t;
    sat: (string * float) list;
    unsat: (string * float) list;
   }

   type t = {
    from: top_result;
    per_prover: (Prover.name * per_prover) list;
   }

   let make (r:top_result): t =
    let stats = Top_result.stat r in
    let per_prover =
      MStr.map
        (fun (prover,stat) ->
           let stat = Raw.stat raw in
           let sat =
             MStr.fold
               (fun file res acc -> match res.Run_result.res with
                  | Res.Sat -> (file, time_of_res res) :: acc
                  | _ -> acc)
               raw []
           and unsat =
             MStr.fold
               (fun file res acc -> match res.Run_result.res with
                  | Res.Unsat -> (file, time_of_res res) :: acc
                  | _ -> acc)
               raw []
           in
           {stat; sat; unsat})
        (Lazy.force r.raw)
    in
    {from=r; per_prover}

   let pp out (r:t): unit =
    let pp_stat out (p,per_prover) =
      Format.fprintf out "@[<h2>%s:@ %a@]"
        p Stat.pp per_prover.stat
    and pp_full _out (_p,_res) =
      () (* TODO *)
    in
    let l = MStr.to_list r.per_prover in
    Format.fprintf out "(@[<v2>bench@ @[%a@]@ @[<v>%a@]@])"
      (pp_hvlist_ pp_stat) l
      (pp_hvlist_ pp_full) l
   end
*)
