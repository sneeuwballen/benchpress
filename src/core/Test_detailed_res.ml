open Common
open Test

type t = (Prover.t, Res.t) Run_result.t

type key = {
  prover: Prover.name;
  file: string;
  res: Res.t;
  file_expect: Res.t;
  rtime: float;
}

type expect_filter =
  | TD_expect_improved
  | TD_expect_ok
  | TD_expect_disappoint
  | TD_expect_bad
  | TD_expect_error

type proof_check_res = { res: Proof_check_res.t; stdout: string; rtime: float }

let get_proof_check db p file : proof_check_res option =
  if Misc.db_has_table db "proof_check_res" then (
    try
      let r =
        Db.exec db
          {|select res from proof_check_res where prover=? and file =?; |}
          ~ty:Db.Ty.([ text; text ], [ text ], fun i -> i)
          ~f:Db.Cursor.next p file
        |> Misc.unwrap_db (fun () ->
               spf "getting proof check res for '%s' on '%s'" p file)
        |> Error.unwrap_opt "expected result"
      and stdout, rtime =
        Db.exec db
          {| select stdout, rtime from proof_check_res where prover=? and file=?; |}
          ~ty:Db.Ty.([ text; text ], [ blob; float ], fun x y -> x, y)
          ~f:Db.Cursor.get_one_exn p file
        |> Misc.unwrap_db (fun () ->
               spf "getting proof check output for '%s' on '%s'" p file)
      in
      Some { res = Proof_check_res.of_string r; stdout; rtime }
    with Error.E e ->
      Log.err (fun k ->
          k "cannot get proof-check-res for '%s' on '%s':@ %a" p file Error.pp e);
      None
  ) else
    None

let list_keys ?(offset = 0) ?(page_size = 500) ?(filter_prover = "")
    ?(filter_pb = "") ?(filter_res = "") ?filter_expect db : key list * _ * _ =
  let filter_prover = clean_s true filter_prover in
  let filter_pb = clean_s true filter_pb in
  (* no wildcard here, as "sat" would match "unsat"… *)
  let filter_res = clean_s false filter_res in
  let filter_expect =
    match filter_expect with
    | None -> ""
    | Some TD_expect_error -> " and res = 'error'"
    | Some TD_expect_ok -> " and res = file_expect "
    | Some TD_expect_disappoint ->
      " and not (res in ('sat','unsat')) and file_expect in ('sat','unsat')"
    | Some TD_expect_bad ->
      " and res in ('sat','unsat') and res != file_expect\n\
      \      and file_expect in ('sat','unsat')"
    | Some TD_expect_improved ->
      " and res in ('sat', 'unsat') and not (file_expect in ('sat','unsat'))"
  in
  Error.guard (Error.wrap "listing detailed results") @@ fun () ->
  let tags = Prover.tags_of_db db in
  (* count total number of results *)
  let n =
    Db.exec db
      (Printf.sprintf
         {|select count(*)
             from prover_res
             where prover like ? and res like ? %s and file like ?|}
         filter_expect)
      ~ty:Db.Ty.(p3 text text text, p1 int, id)
      ~f:Db.Cursor.get_one_exn filter_prover filter_res filter_pb
    |> Misc.unwrap_db (fun () ->
           spf "counting results of '%s' with '%s'" filter_prover filter_res)
  in

  (* ask for [limit+1] entries *)
  let l =
    Db.exec db
      (Printf.sprintf
         {|select distinct prover, file, res, file_expect, rtime
             from prover_res
             where prover like ? and res like ? %s and file like ?
             order by file, prover desc
              limit ? offset ?
            ; |}
         filter_expect)
      ~ty:
        Db.Ty.(
          ( [ text; text; text; int; int ],
            [ text; any_str; text; text; float ],
            fun prover file res file_expect rtime ->
              let res = Res.of_string ~tags res in
              let file_expect = Res.of_string ~tags file_expect in
              { prover; file; res; file_expect; rtime } ))
      ~f:Db.Cursor.to_list_rev filter_prover filter_res filter_pb
      (page_size + 1) offset
    |> Misc.unwrap_db (fun () ->
           spf "listing %d results of '%s' with '%s'" (page_size + 1)
             filter_prover filter_res)
  in
  if List.length l > page_size then
    ( (* there are more result, cut the last one *)
      CCList.take page_size l,
      n,
      false )
  else
    l, n, true

let get_res db prover file : _ * proof_check_res option =
  Profile.with_ "detailed-res" ~args:[ "prover", prover ] @@ fun () ->
  Error.guard (Error.wrapf "getting results for '%s' on '%s'" prover file)
  @@ fun () ->
  let tags = Prover.tags_of_db db in
  let res : _ Run_result.t =
    Db.exec db
      {|select
        res, file_expect, timeout, errcode, stdout, stderr,
        rtime, utime, stime
        from prover_res where prover=? and file=? ; |}
      prover file ~f:Db.Cursor.next
      ~ty:
        Db.Ty.(
          ( [ text; text ],
            [
              any_str;
              any_str;
              int;
              int;
              nullable blob;
              nullable blob;
              float;
              float;
              float;
            ],
            fun x1 x2 x3 x4 x5 x6 x7 x8 x9 ->
              Logs.info (fun k -> k "got results");
              x1, x2, x3, x4, x5, x6, x7, x8, x9 ))
    |> Misc.unwrap_db (fun () -> spf "listing results")
    |> Error.unwrap_opt' (fun () ->
           spf "expected a non-empty result for prover='%s', file='%s'" prover
             file)
    |> fun ( res,
             file_expect,
             timeout,
             errcode,
             stdout,
             stderr,
             rtime,
             utime,
             stime ) ->
    Logs.info (fun k -> k "got results 2");
    let stdout = CCOpt.get_or ~default:"" stdout in
    let stderr = CCOpt.get_or ~default:"" stderr in
    Logs.debug (fun k -> k "res.of_string tags=[%s]" (String.concat "," tags));
    let expected = Res.of_string ~tags file_expect in
    Logs.debug (fun k -> k "got expected %a" Res.pp expected);
    let res = Res.of_string ~tags res in
    Logs.debug (fun k -> k "got res %a" Res.pp res);
    let timeout = Limit.Time.mk ~s:timeout () in
    Run_result.make prover ~timeout ~res
      { Problem.name = file; expected }
      { Run_proc_result.errcode; stdout; stderr; rtime; utime; stime }
  in
  let proof_check_res = get_proof_check db prover file in
  Logs.info (fun k -> k "try to get prover");
  let prover = Prover.of_db db prover in
  Logs.info (fun k -> k "got prover");
  Run_result.map ~f:(fun _ -> prover) res, proof_check_res

module PB = PrintBox

let to_printbox ?link:(mk_link = default_pp_linker) (self : t)
    (check_res : proof_check_res option) :
    PB.t * PB.t * string * string * string option =
  let open PB in
  Logs.debug (fun k -> k "coucou");
  let pp_proof_res = function
    | Proof_check_res.Valid ->
      text_with_style Style.(set_fg_color Green @@ bold) "valid"
    | Proof_check_res.Invalid ->
      text_with_style Style.(set_fg_color Red @@ bold) "invalid"
    | Proof_check_res.Unknown msg ->
      PB.hlist
        [
          text_with_style Style.(set_fg_color Yellow @@ bold) "unknown";
          text msg;
        ]
  in
  Logs.debug (fun k -> k "prover is %a" Prover.pp self.program);
  ( v_record @@ List.flatten
    @@ [
         [
           ( "problem.path",
             mk_link self.program.Prover.name self.problem.Problem.name );
           "problem.expected_res", Res.to_printbox self.problem.Problem.expected;
           "res", Res.to_printbox self.res;
         ];
         [
           "rtime", text (Misc.human_duration self.raw.rtime);
           "stime", text (Misc.human_duration self.raw.stime);
           "utime", text (Misc.human_duration self.raw.utime);
           "errcode", int self.raw.errcode;
         ];
         (match check_res with
         | None -> []
         | Some p ->
           [
             "proof_check.res", pp_proof_res p.res;
             "proof_check.time", text (Misc.human_duration p.rtime);
           ]);
       ],
    v_record
    @@ List.flatten
         [
           [
             "prover.name", text self.program.Prover.name;
             "prover.cmd", text self.program.Prover.cmd;
             ( "prover.version",
               text
               @@ Format.asprintf "%a" Prover.Version.pp
                    self.program.Prover.version );
             ( "prover.sat",
               text (CCOpt.get_or ~default:"<none>" self.program.Prover.sat) );
             ( "prover.unsat",
               text (CCOpt.get_or ~default:"<none>" self.program.Prover.unsat) );
             ( "prover.unknown",
               text (CCOpt.get_or ~default:"<none>" self.program.Prover.unknown)
             );
             ( "prover.timeout",
               text (CCOpt.get_or ~default:"<none>" self.program.Prover.timeout)
             );
           ];
           CCList.map
             (fun (tag, re) -> "prover.tag." ^ tag, text re)
             self.program.Prover.custom;
           [
             ( "prover.memory",
               text (CCOpt.get_or ~default:"<none>" self.program.Prover.memory)
             );
           ];
         ],
    self.raw.stdout,
    self.raw.stderr,
    CCOpt.map (fun p -> p.stdout) check_res )
