open Common

type filename = string
type prover = filename * Prover.name
type status = [ `Sat | `Unsat ]

let pb_v_record = Test.pb_v_record
let pb_int_color = Test.pb_int_color

(* Common helpers *)
let make_db f1 f2 =
  let db = Sqlite3.db_open ":memory:" in
  Db.exec_no_cursor db "attach database ? as db1;" ~ty:Db.Ty.(p1 text) f1
  |> Misc.unwrap_db (fun () -> spf "attaching DB %S" f1);
  Db.exec_no_cursor db "attach database ? as db2;" ~ty:Db.Ty.(p1 text) f2
  |> Misc.unwrap_db (fun () -> spf "attaching DB %S" f2);
  db

let cmp2sql = function
  | `Same ->
    {|
      (r1.res in ('sat', 'unsat') and r1.res = r2.res)
      or
      (r1.res not in ('sat', 'unsat') and r2.res not in ('sat', 'unsat'))
    |}
  | `Mismatch ->
    {|
      r1.res in ('sat', 'unsat') and
      r2.res in ('sat', 'unsat') and
      r1.res != r2.res
    |}
  | `Improved ->
    {| r1.res not in ('sat', 'unsat') and r2.res in ('sat', 'unsat') |}
  | `Regressed ->
    {| r1.res in ('sat', 'unsat') and r2.res not in ('sat', 'unsat') |}

let pp_cmp = Fmt.of_to_string cmp2sql

let status_to_sql = function
  | `Sat -> {| (r1.res = 'sat' or r2.res = 'sat') |}
  | `Unsat -> {| (r1.res = 'unsat' or r2.res = 'unsat') |}

let pp_status_sql = Fmt.of_to_string status_to_sql

let pp_opt ?(none = Fmt.const_string "") pp ppf = function
  | None -> none ppf ()
  | Some v -> pp ppf v

let pp_prefix prefix pp ppf v =
  Fmt.fprintf ppf "%s" prefix;
  pp ppf v

let unsafe_sql ?order ?limit ?offset ?filter ?status select =
  let pp_filter ppf filter =
    match filter with
    | None -> ()
    | Some filter -> Format.fprintf ppf "and (%a)" pp_cmp filter
  in
  let pp_status = pp_opt (pp_prefix " and " pp_status_sql) in
  let pp_select =
    Fmt.list
      ~sep:(fun ppf () -> Fmt.fprintf ppf ",@,")
      (fun ppf s -> Fmt.fprintf ppf "(%s)" s)
  in
  Fmt.asprintf
    {| select %a
        from db1.prover_res r1
        inner join db2.prover_res r2 on r2.file = r1.file
        where r1.prover = ? and r2.prover = ? %a %a %a %a %a
      |}
    pp_select select pp_filter filter pp_status status
    (pp_opt (pp_prefix " order by " Fmt.string))
    order
    (pp_opt (pp_prefix " limit " Fmt.int))
    limit
    (pp_opt (pp_prefix " offset " Fmt.int))
    offset

module Short = struct
  type t = {
    appeared: int; (* new problems *)
    disappeared: int; (* problems that disappeared *)
    improved: int;
    regressed: int;
    mismatch: int;
    same: int; (* same result *)
  }

  let to_printbox (self : t) =
    let open PrintBox in
    pb_v_record
      [
        "appeared", int self.appeared;
        "disappeared", int self.disappeared;
        "same", int self.same;
        "mismatch", pb_int_color Style.(fg_color Red) self.mismatch;
        "improved", pb_int_color Style.(fg_color Green) self.improved;
        "regressed", pb_int_color Style.(fg_color Cyan) self.regressed;
      ]

  let make1 ?status db p1 p2 : t =
    (* Note: first ? is p1, second ? is p2 *)
    let get_n q =
      Db.exec db q p1 p2
        ~ty:Db.Ty.(p2 text text, p1 int, fun x -> x)
        ~f:(fun c ->
          match Db.Cursor.next c with
          | None -> Error.failf "expected result for query\n%s" q
          | Some x -> x)
      |> Misc.unwrap_db (fun () -> spf "while running\n%s" q)
    in
    let appeared =
      get_n
        {| select count(r2.file) from db2.prover_res r2
                where not exists (select file from db1.prover_res where
                db1.prover_res.prover= ?
                and file = r2.file)
                and r2.prover = ?; |}
    and disappeared =
      get_n
        {| select count(r1.file) from db1.prover_res r1
                where r1.prover = ?
                and not exists (select file from db2.prover_res where
                db2.prover_res.prover=?
                and file = r1.file); |}
    and same = get_n (unsafe_sql ?status ~filter:`Same [ "count(r1.file)" ])
    and mismatch =
      get_n (unsafe_sql ?status ~filter:`Mismatch [ "count(r1.file)" ])
    and improved =
      get_n (unsafe_sql ?status ~filter:`Improved [ "count(r1.file)" ])
    and regressed =
      get_n (unsafe_sql ?status ~filter:`Regressed [ "count(r1.file)" ])
    in
    { appeared; disappeared; same; mismatch; improved; regressed }

  let make_provers ?status (f1, p1) (f2, p2) : t =
    Error.guard
      (Error.wrapf "short comparison of '%s/%s' and '%s/%s'" f1 p1 f2 p2)
    @@ fun () -> make1 ?status (make_db f1 f2) p1 p2

  let make ?status f1 f2 : (_ * t) list =
    Error.guard (Error.wrapf "short comparison of '%s' and '%s'" f1 f2)
    @@ fun () ->
    let db = Sqlite3.db_open ":memory:" in
    Db.exec_no_cursor db "attach database ? as db1;" ~ty:Db.Ty.(p1 text) f1
    |> Misc.unwrap_db (fun () -> spf "attaching DB %S" f1);
    Db.exec_no_cursor db "attach database ? as db2;" ~ty:Db.Ty.(p1 text) f2
    |> Misc.unwrap_db (fun () -> spf "attaching DB %S" f2);
    let provers =
      Db.exec_no_params db
        {| select distinct * from (select prover from db1.prover_res UNION
              select prover from db2.prover_res) ;|}
        ~ty:Db.Ty.(p1 text, id)
        ~f:Db.Cursor.to_list_rev
      |> Misc.unwrap_db (fun () -> "listing all provers")
    in
    Logs.debug (fun k -> k "provers: [%s]" (String.concat ";" provers));
    CCList.map (fun prover -> prover, make1 ?status db prover prover) provers
end

module Full = struct
  type filter = [ `Improved | `Regressed | `Mismatch | `Same ]
  type entry = string * Res.t * float * Res.t * float

  let make_filtered ?(page = 0) ?(page_size = 500) ?filter ?status (f1, p1)
      (f2, p2) =
    let tags = [] (* TODO? *) in
    let offset = page * page_size in
    let limit = page_size + 1 in
    Db.exec (make_db f1 f2)
      (unsafe_sql ~order:"r1.file desc" ~limit ~offset ?filter ?status
         [ "r1.file"; "r1.res"; "r2.res"; "r1.rtime"; "r2.rtime" ])
      p1 p2
      ~ty:
        Db.Ty.(
          ( p2 text text,
            [ any_str; text; text; float; float ],
            fun file res1 res2 time1 time2 ->
              let res1 = Res.of_string ~tags res1 in
              let res2 = Res.of_string ~tags res2 in
              file, res1, time1, res2, time2 ))
      ~f:Db.Cursor.to_list_rev
    |> Misc.unwrap_db (fun () -> spf "listing comparison results")

  let to_printbox ?(file_link = Test.default_pp_linker) vals =
    let open PrintBox in
    let hdr = text_with_style Style.bold in
    grid_l ~bars:true
      ([
         hdr "file";
         hdr "old res";
         hdr "old time";
         hdr "new res";
         hdr "new time";
       ]
      :: List.map
           (fun (fname, res1, time1, res2, time2) ->
             [
               file_link fname fname;
               Res.to_printbox res1;
               float time1;
               Res.to_printbox res2;
               float time2;
             ])
           vals)
end
