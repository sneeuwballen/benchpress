(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

module E = CCResult
module Db = Sqlite3_utils
module Fmt = CCFormat

type 'a or_error = ('a, string) CCResult.t

type prover  = Prover.t
type checker = unit

type t =
  | Prover_run of Prover.name Run_result.t
  | Checker_run of checker Run_result.t

type event = t

let pp out = function
  | Prover_run r -> Run_result.pp Fmt.string out r
  | Checker_run r -> Run_result.pp Fmt.unit out r

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
    create index if not exists pr_prover on prover_res(prover);
    create index if not exists pr_file on prover_res(file);
    |}
  |> Misc.db_err ~ctx:"run-event.db-prepare"

let to_db_prover_result (db:Db.t) (self:Prover.name Run_result.t) : _ or_error =
  Db.exec_no_cursor db
    {|insert into prover_res
    values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
    |}
    ~ty:Db.Ty.(p4 text text text text @>> p2 int int @>> p2 blob blob @>> p3 float float float)
    self.program
    self.problem.Problem.name
    (self.res |> Res.to_string)
    (self.problem.Problem.expected |> Res.to_string)
    self.timeout
    self.raw.errcode
    self.raw.stdout
    self.raw.stderr
    self.raw.rtime
    self.raw.utime
    self.raw.stime
  |> Misc.db_err ~ctx:"run-event.to-db-prover-result"

let to_db db self : _ or_error =
  match self with
  | Prover_run r -> to_db_prover_result db r
  | Checker_run _ ->
    Error "not implemented: conversion of checker res to DB" (* TODO *)

let of_db_map db ~f : _ list or_error =
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
             {Problem.name=pb_name; expected=Res.of_string expected}
           in
           let p =
             Run_result.make pname pb ~timeout ~res:(Res.of_string res)
               {errcode;stderr;stdout;rtime;utime;stime}
           in
           f p))
    ~f:Db.Cursor.to_list_rev
  |> Misc.db_err ~ctx:"run-event.of-db-map"

let of_db_l db : Prover.name Run_result.t list or_error =
  of_db_map ~f:(fun x->x) db
