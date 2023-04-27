open Common

type filename = string

let pb_v_record = Test.pb_v_record
let pb_int_color = Test.pb_int_color

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

  let make1 db p1 p2 : t =
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
    and same =
      get_n
        {| select count(r1.file) from db1.prover_res r1, db2.prover_res r2
                where
                  r1.prover = ? and r2.prover = ?
                  and r1.file = r2.file
                  and (
                      (r1.res in ('sat', 'unsat') and r1.res = r2.res)
                      or
                      (not (r1.res in ('sat','unsat')) and not (r2.res in ('sat','unsat')))
                  ) ; |}
    and mismatch =
      get_n
        {| select count(r1.file) from db1.prover_res r1, db2.prover_res r2
                where
                  (r1.res in ('sat', 'unsat') or r2.res in ('sat', 'unsat'))
                  and r1.prover = ? and r2.prover = ?
                  and r1.file = r2.file and r1.res != r2.res; |}
    and improved =
      get_n
        {| select count(r1.file) from db1.prover_res r1, db2.prover_res r2
                where not (r1.res in ('sat', 'unsat'))
                  and r2.res in ('sat', 'unsat')
                  and r1.prover = ? and r2.prover = ?
                  and r1.file = r2.file ; |}
    and regressed =
      get_n
        {| select count(r1.file) from db1.prover_res r1, db2.prover_res r2
                where r1.res in ('sat', 'unsat')
                  and not (r2.res in ('sat', 'unsat'))
                  and r1.prover = ? and r2.prover = ?
                  and r1.file = r2.file ; |}
    in
    { appeared; disappeared; same; mismatch; improved; regressed }

  let make_provers (f1, p1) (f2, p2) : t =
    Error.guard
      (Error.wrapf "short comparison of '%s/%s' and '%s/%s'" f1 p1 f2 p2)
    @@ fun () ->
    let db = Sqlite3.db_open ":memory:" in
    Db.exec_no_cursor db "attach database ? as db1;" ~ty:Db.Ty.(p1 text) f1
    |> Misc.unwrap_db (fun () -> spf "attaching DB %S" f1);
    Db.exec_no_cursor db "attach database ? as db2;" ~ty:Db.Ty.(p1 text) f2
    |> Misc.unwrap_db (fun () -> spf "attaching DB %S" f2);
    make1 db p1 p2

  let make f1 f2 : (_ * t) list =
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
    CCList.map (fun prover -> prover, make1 db prover prover) provers
end
