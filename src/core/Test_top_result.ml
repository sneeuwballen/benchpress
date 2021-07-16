
open Misc
open Test

type t = {
  meta: Test_metadata.t;
  provers: Prover.t list;
  events: Run_event.t list;
  stats: (Prover.name * Test_stat.t) list;
  analyze: (Prover.name * Test_analyze.t) list;
  db: Db.t; (* in-memory database *)
}

(** Filter on the list of all results *)
type tr_filter =
  | TRF_all
  | TRF_different (** different results *)
  | TRF_bad (** inconsistent results *)

let string_of_trf = function
  | TRF_bad -> "bad"
  | TRF_different -> "different"
  | TRF_all -> "all"

let analyze self = self.analyze
let stat self = self.stats

let is_ok self = List.for_all (fun (_,a) -> Test_analyze.is_ok a) @@ analyze self

let pp_header out (self:t) : unit =
  Format.fprintf out "(@[(uuid %s)(date %a)@])"
    (Uuidm.to_string self.meta.uuid)
    (Misc.Pp.pp_opt "timestamp" Misc.pp_human_datetime) self.meta.timestamp

let pp_compact out (self:t) =
  let pp_tup out (p,res) =
    Format.fprintf out "@[<2>%a:@ @[%a@]@]"
      Fmt.string p Test_analyze.pp res
  in
  let {analyze=a; _} = self in
  Format.fprintf out "(@[<2>%a@ %a@])"
    pp_header self (pp_list_ pp_tup) a

let pp_bad out (r:t) : unit =
  let pp_tup out (p,res) =
    Format.fprintf out "@[<2>%a:@ @[%a@]@]"
      Fmt.string p Test_analyze.pp_bad res
  in
  let a = analyze r in
  Format.fprintf out "(@[<2>%a@ %a@])"
    pp_header r (pp_list_ pp_tup) a

let pp out (self:t) : unit =
  let pp_tup out (p,res) =
    Format.fprintf out "@[<2>%a:@ @[%a@]@]"
      Fmt.string p Test_analyze.pp res
  in
  let a = analyze self in
  Format.fprintf out "(@[<2>%a@ %a@])"
    pp_header self (pp_list_ pp_tup) a

let to_compact_result (self:t) : Test_compact_result.t or_error =
  let open E.Infix in
  Test_comparison_short.of_db self.db >>= fun cr_comparison ->
  let cr_analyze = analyze self in
  let cr_stat = stat self in
  E.return {Test_compact_result.cr_analyze; cr_meta=self.meta; cr_stat; cr_comparison; }

type table_row = {
  tr_problem: string;
  tr_res: (string * Res.t * float) list; (* prover, result, time *)
}

type table = {
  t_meta: string;
  t_rows: table_row list;
  t_provers: string list;
}

let db_to_table ?(offset=0) ?(page_size=max_int) ?provers
    ?(filter_pb="") ?(filter_res=TRF_all)
    (db:Db.t): table =
  let c = Misc.Chrono.start() in
  Misc.err_with
    ~map_err:(Printf.sprintf "while converting to table: %s")
    (fun scope ->
       Db.transact db @@ fun _ ->
       let meta = Test_metadata.of_db db |> scope.unwrap in
       let line0 =
         Printf.sprintf "(snapshot :uuid %s :date %s)"
           (Uuidm.to_string meta.uuid)
           (CCOpt.map_or ~default:"<none>" Misc.human_datetime meta.timestamp)
       in
       let provers =
         match provers with
         | Some l -> l
         | None -> list_provers db |> scope.unwrap
       in
       let filter_pb = clean_s true filter_pb in
       let filter_res_clause = match filter_res with
         | TRF_all -> ""
         | TRF_different ->
           " and exists (select p1.prover, p2.prover \
            from prover_res p1, prover_res p2 \
            where p1.prover != p2.prover and p1.res != p2.res \
            and p1.file = r.file and p2.file = r.file)"
         | TRF_bad ->
           " and exists (select p1.prover, p2.prover \
            from prover_res p1, prover_res p2 \
            where p1.prover != p2.prover and p1.res != p2.res \
            and p1.res in ('sat','unsat') and p2.res in ('sat','unsat') \
            and p1.file = r.file and p2.file = r.file)"
       in
       let files =
         Db.exec db
           (Printf.sprintf {| select distinct file from prover_res r
             where file like ? %s limit ? offset ?
           ; |} filter_res_clause)
           filter_pb page_size offset
           ~ty:Db.Ty.(p3 text int int, p1 text, id) ~f:Db.Cursor.to_list_rev
                   |> scope.unwrap_with Db.Rc.to_string
       in
       Logs.info (fun k->k"to_table: found %d files in %.3fs" (List.length files)
                 (Misc.Chrono.since_last c));
       let tags = Prover.tags_of_db db in
       Logs.info (fun k->k"to_table: found tags [%s]" (String.concat "," tags));
       let t_rows =
         List.rev_map
           (fun file ->
              let tr_res =
                Db.exec db
                  {| select prover, res, rtime from
                 prover_res where file=? order by prover; |}
                  file
                  ~ty:Db.Ty.(p1 text, p3 text text float,
                             fun prover res t ->
                               prover, Res.of_string ~tags res, t)
                  ~f:Db.Cursor.to_list_rev
                |> scope.unwrap_with Db.Rc.to_string
                |> List.filter (fun (p,_,_) -> List.mem p provers)
              in
              {tr_problem=file; tr_res})
           files
       in
       Logs.info (fun k->k"to_table: gathered lines in %.3fs" (Misc.Chrono.since_last c));
       {t_meta=line0; t_provers=provers; t_rows}
    )
  |> (function
      | Ok x -> x
      | Error e ->
        Logs.err (fun k->k "conversion to CSV failed: %s" e);
        failwith ("error while converting to CSV: " ^ e))

let to_table ?offset ?page_size ?provers (self:t): table =
  db_to_table ?offset ?page_size ?provers self.db

let time_to_csv (_:Res.t) f = Printf.sprintf "%.2f" f
let res_to_csv (r:Res.t) = match r with
  | Res.Error -> "error"
  | Res.Timeout -> "timeout"
  | Res.Unknown -> "unknown"
  | Res.Sat -> "sat"
  | Res.Unsat -> "unsat"
  | Res.Tag s -> s

let table_to_csv (t:table): Csv.t =
  let header_line =
    "problem" ::
    t.t_provers @
    (CCList.map (fun p -> p ^ ".time") t.t_provers)
  in
  let lines =
    CCList.map
      (fun r ->
         r.tr_problem
         :: CCList.map (fun (_,res,_) ->  res_to_csv res) r.tr_res
         @ CCList.map (fun (_,res,t) -> time_to_csv res t) r.tr_res)
      t.t_rows
  in
  header_line :: lines

let db_to_csv ?provers db : Csv.t = db_to_table ?provers db |> table_to_csv
let to_csv ?provers t : Csv.t = to_table ?provers t |> table_to_csv

let table_to_printbox
    ?(link_pb=default_linker)
    ?(link_res=default_ppr_linker)
    (self:table) : PB.t =
  let header_line =
    CCList.map PB.(text_with_style Style.bold) @@
    "problem" ::
    self.t_provers @
    (CCList.map (fun p -> p ^ ".time") self.t_provers)
  in
  let lines =
    CCList.map
      (fun r ->
         link_pb r.tr_problem
         :: CCList.map (fun (prover,res,_) ->
             link_res prover r.tr_problem ~res:(res_to_csv res)) r.tr_res
         @ CCList.map (fun (_,res,t) -> PB.text @@ time_to_csv res t) r.tr_res)
      self.t_rows
  in
  PB.grid_l (header_line::lines)

let to_printbox_stat (self:t) : PB.t =
  let a = stat self in
  Test_stat.to_printbox_l a

let to_printbox_summary (self:t) : PB.t =
  let a = analyze self in
  Test_analyze.to_printbox_l a

let db_to_printbox_table ?offset ?page_size ?link_pb
    ?link_res ?filter_pb ?filter_res db =
  table_to_printbox ?link_pb ?link_res @@
  db_to_table ?offset ?page_size ?filter_pb ?filter_res db

let to_printbox_table ?offset ?page_size ?link_pb ?link_res self =
  table_to_printbox ?link_pb ?link_res @@ to_table ?offset ?page_size self

let to_printbox_bad self =
  let a = analyze self in
  CCList.filter_map
    (fun (p, a) ->
       if a.Test_analyze.bad = 0 then None
       else Some (p, Test_analyze.to_printbox_bad a))
    a

let to_printbox_errors self =
  let a = analyze self in
  CCList.map
    (fun (p, a) -> (p, Test_analyze.to_printbox_errors a))
    a

(* TODO
   let comparison_to_printbox ?(short=true) (self:comparison_result) : PB.t =
   let open PB in
   let pm side m =
    MStr.to_list m
    |> CCList.map
      (fun (p,c) ->
         [text_with_style Style.bold (p ^ " ("^side^")");
          Test_analyze.to_printbox c])
   in
   grid_l @@ List.flatten [
    [
      MStr.to_list self.both
      |> CCList.map
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

let str_of_csv_ csv =
  let buf = Buffer.create 256 in
  let ch = Csv.to_buffer buf in
  Csv.output_all ch csv;
  Buffer.contents buf

let db_to_csv_string ?provers db =
  str_of_csv_ @@ db_to_csv ?provers db

let to_csv_string ?provers t =
  str_of_csv_ @@ to_csv ?provers t

let db_prepare (db:Db.t) : _ or_error =
  let open E.Infix in
  Test_metadata.db_prepare db >>= fun () ->
  Run_event.db_prepare db >>= fun () ->
  Prover.db_prepare db >>= fun () ->
  Ok ()

let to_db (db:Db.t) (self:t) : unit or_error =
  Logs.info (fun k->k "dump top-result into DB");
  Misc.err_with ~map_err:(Printf.sprintf "while dumping top-res to DB: %s")
    (fun scope ->
       scope.unwrap @@ db_prepare db;
       scope.unwrap @@ Test_metadata.to_db db self.meta;
       scope.unwrap @@ Prover.db_prepare db;
       (* insert within one transaction, much faster *)
       Db.transact db (fun _ ->
           List.iter (fun p -> Prover.to_db db p |> scope.unwrap) self.provers;
           List.iter (fun ev -> scope.unwrap @@ Run_event.to_db db ev) self.events);
       ())

let make ~meta ~provers
    (l:Prover.name Run_result.t list) : t or_error =
  Misc.err_with
    ~map_err:(Printf.sprintf "while reading top-res from DB: %s")
    (fun scope ->
       let events = List.rev_map Run_event.mk_prover l in
       (* create a temporary in-memory DB *)
       let db = Sqlite3.db_open ":memory:" in
       db_prepare db |> scope.unwrap;
       Test_metadata.to_db db meta |> scope.unwrap;
       Db.transact db (fun _ ->
           List.iter
             (fun p ->
                Prover.to_db db p
                |> scope.unwrap_with
                  (Printf.sprintf "while adding prover %s: %s" p.Prover.name))
             provers;
           List.iter (fun ev -> scope.unwrap @@ Run_event.to_db db ev) events);
       Logs.debug (fun k->k "computing stats");
       let stats = Test_stat.of_db db |> scope.unwrap in
       Logs.debug (fun k->k "computing analyze");
       let analyze = Test_analyze.of_db ~full:false db |> scope.unwrap in
       Logs.debug (fun k->k "done");
       { db; events; meta; provers; stats; analyze; })

let of_db (db:Db.t) : t or_error =
  Misc.err_with
    ~map_err:(Printf.sprintf "while reading top-res from DB: %s")
    (fun scope ->
       Logs.debug (fun k->k "loading metadata from DB");
       let meta = Test_metadata.of_db db |> scope.unwrap in
       let prover_names = Prover.db_names db |> scope.unwrap in
       let provers =
         CCList.map (fun p -> Prover.of_db db p |> scope.unwrap) prover_names in
       Logs.debug (fun k->k "loading events from DB");
       let events =
         Run_event.of_db_l db |> scope.unwrap
       in
       make ~meta ~provers events |> scope.unwrap)
