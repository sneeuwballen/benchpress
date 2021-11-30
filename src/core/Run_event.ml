(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

module E = Or_error
module Db = Sqlite3_utils
module Fmt = CCFormat
module Log = (val Logs.src_log (Logs.Src.create "benchpress.run-event"))

type 'a or_error = 'a Or_error.t

type prover  = Prover.t
type checker = Proof_checker.t

type t =
  | Prover_run of (Prover.name, Res.t) Run_result.t
  | Checker_run of (Prover.name * Proof_checker.name, Proof_check_res.t) Run_result.t

type event = t

let pp out = function
  | Prover_run r -> Run_result.pp Fmt.string Res.pp out r
  | Checker_run r ->
    let pptuple out (p,c) = Fmt.fprintf out "prover:%s, checker: %s" p c in
    Run_result.pp pptuple Proof_check_res.pp out r

let mk_prover r = Prover_run r
let mk_checker r = Checker_run r

(* main schema for results! *)
let db_prepare (db:Db.t) : unit or_error =
  let open E.Infix in
  Prover.db_prepare db >>= fun () ->
  Db.exec0 db
    {|create table if not exists
      prover_res (
        prover text not null,
        file text not null,
        res text not null,
        file_expect text not null,
        timeout int,
        errcode int not null,
        stdout blob,
        stderr blob,
        rtime float,
        utime float,
        stime float,
        unique (prover, file) on conflict fail
      );

    create table if not exists
      proof_check_res (
        prover text not null,
        file text not null,
        checker text not null,
        res text not null,
        rtime float,
        stdout text,
        stderr text
    );

    create index if not exists pr_prover_res on prover_res(prover,res,file_expect);
    create index if not exists pr_file on prover_res(file);
    create index if not exists prf_all on proof_check_res(prover,checker,file);
    create index if not exists prf_file on proof_check_res(file);
    |}
  |> Misc.db_err_with ~ctx:"run-event.db-prepare"

let to_db_prover_result (db:Db.t) (self:(Prover.name,_) Run_result.t) : _ or_error =
  Db.exec_no_cursor db
    {|insert into prover_res
    values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
    |}
    ~ty:Db.Ty.([text; text; text; text; int; int; blob; blob; float; float; float])
    self.program
    self.problem.Problem.name
    (self.res |> Res.to_string)
    (self.problem.Problem.expected |> Res.to_string)
    (self.timeout |> Limit.Time.as_int Seconds)
    self.raw.errcode
    self.raw.stdout
    self.raw.stderr
    self.raw.rtime
    self.raw.utime
    self.raw.stime
  |> Misc.db_err_with ~ctx:"run-event.to-db-prover-result"

let to_db_check_result (db:Db.t) (self:_ Run_result.t) : _ or_error =
  let p, c = self.program in
  Db.exec_no_cursor db
    {|insert into proof_check_res (prover, file, checker, res, rtime, stdout, stderr)
      values (?, ?, ?, ?, ?, ?, ?); |}
    ~ty:Db.Ty.([text; text; text; text; float; blob; blob])
    p self.problem.Problem.name c (Proof_check_res.to_string self.res)
    self.raw.rtime self.raw.stdout self.raw.stderr
  |> Misc.db_err_with ~ctx:"run-event.to-db-checker-result"

let to_db db self : _ or_error =
  match self with
  | Prover_run r -> to_db_prover_result db r
  | Checker_run r -> to_db_check_result db r

let of_db_provers_map db ~f : _ list or_error =
  let tags = Prover.tags_of_db db in
  Db.exec_no_params db {|
    select
      prover, file, res, file_expect, timeout, errcode, stdout, stderr,
      rtime, utime, stime
     from prover_res;
    |}
    ~ty:Db.Ty.(
        p4 text text text text @>> p2 int int @>> p2 blob blob @>> p3 float float float,
        (fun pname pb_name res expected timeout errcode stdout stderr rtime utime stime ->
           let pb =
             {Problem.name=pb_name; expected=Res.of_string ~tags expected}
           in
           let timeout = Limit.Time.mk ~s:timeout () in
           let p =
             Run_result.make pname pb ~timeout ~res:(Res.of_string ~tags res)
               {errcode;stderr;stdout;rtime;utime;stime}
           in
           f p))
    ~f:Db.Cursor.to_list_rev
  |> Misc.db_err_with ~ctx:"run-event.of-db-map"

let of_db_checker_map' db ~f (scope:_ Misc.try_scope) : _ list or_error =
  let tags = Prover.tags_of_db db in
  Db.exec_no_params db {|
    select p.prover, p.file, e.file_expect, p.checker,
           p.res, p.rtime, p.stdout, p.stderr
      from proof_check_res p,
        (select distinct file, file_expect from prover_res) as e
      where p.file = e.file;
    |}
    ~ty:Db.Ty.(
        [text; text; blob; text; blob; float; blob; blob],
        fun prover file expected checker res rtime stdout stderr ->
          let pb =
            {Problem.name=file; expected=Res.of_string ~tags expected}
          and res =
            Proof_check_res.of_string res
            |> CCResult.map_err (Error.wrap "parse proof check res") |> scope.unwrap
          in
          let p =
            Run_result.make (prover,checker) pb
              ~timeout:Limit.(Time.mk ~s:0 ()) ~res
              {errcode=0; stderr;stdout;rtime;utime=0.;stime=0.}
          in
          f p)
    ~f:Db.Cursor.to_list_rev
  |> Misc.db_err_with ~ctx:"run-event.of-db-checker-map"

let of_db_checker_map db ~f : _ list or_error =
  Misc.err_with ~map_err:(Error.wrap "run-event.of-db") @@ fun scope ->

  if Misc.db_has_table db "proof_check_res" then (
    of_db_checker_map' db ~f scope |> scope.unwrap
  ) else (
    Log.debug (fun k->k"no table proof_check_res found");
    []
  )

let of_db_l db : t list or_error =
  let open CCResult.Infix in
  let* l1 = of_db_provers_map db ~f:(fun x->Prover_run x) in
  let* l2 = of_db_checker_map db ~f:(fun x->Checker_run x) in
  Ok (List.rev_append l1 l2)
