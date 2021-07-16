
open Misc
open Test

type t = Prover.t Run_result.t

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

let list_keys ?(offset=0) ?(page_size=500)
    ?(filter_prover="") ?(filter_pb="") ?(filter_res="") ?filter_expect
    db : (key list*_*_) or_error =
  let filter_prover = clean_s true filter_prover in
  let filter_pb = clean_s true filter_pb in
  (* no wildcard here, as "sat" would match "unsat"… *)
  let filter_res = clean_s false filter_res in
  let filter_expect = match filter_expect with
    | None -> ""
    | Some TD_expect_error -> " and res = 'error'"
    | Some TD_expect_ok ->
      " and res = file_expect "
    | Some TD_expect_disappoint ->
      " and not (res in ('sat','unsat')) and file_expect in ('sat','unsat')"
    | Some TD_expect_bad ->
      " and res in ('sat','unsat') \
      and res != file_expect
      and file_expect in ('sat','unsat')"
    | Some TD_expect_improved ->
      " and res in ('sat', 'unsat') and not (file_expect in ('sat','unsat'))"
  in
  Misc.err_with
    ~map_err:(Printf.sprintf "when listing detailed results: %s")
    (fun scope ->
       let tags = Prover.tags_of_db db in
       (* count total number of results *)
       let n =
         Db.exec db
           (Printf.sprintf {|select count(*)
             from prover_res
             where prover like ? and res like ? %s and file like ?|}
              filter_expect)
           ~ty:Db.Ty.(p3 text text text, p1 int, id) ~f:Db.Cursor.get_one_exn
           filter_prover filter_res filter_pb
         |> E.map_err
           (fun e -> Printf.sprintf "sqlite error: %s" (Db.Rc.to_string e))
         |> scope.unwrap
       in
       (* ask for [limit+1] entries *)
       let l =
         Db.exec db
           (Printf.sprintf {|select distinct prover, file, res, file_expect, rtime
             from prover_res
             where prover like ? and res like ? %s and file like ?
             order by file, prover desc
              limit ? offset ?
            ; |} filter_expect)
           ~ty:Db.Ty.(p5 text text text int int,
                      p5 text any_str text text float,
                      fun prover file res file_expect rtime ->
                        let res=Res.of_string ~tags res in
                        let file_expect=Res.of_string ~tags file_expect in
                        {prover;file;res;file_expect;rtime})
           ~f:Db.Cursor.to_list_rev
           filter_prover filter_res filter_pb (page_size+1) offset
         |> E.map_err
           (fun e -> Printf.sprintf "sqlite error: %s" (Db.Rc.to_string e))
         |> scope.unwrap
       in
       if List.length l > page_size then (
         (* there are more result, cut the last one *)
         CCList.take page_size l, n, false
       ) else (
         l, n, true
       )
    )

let get_res db prover file : _ or_error =
  Profile.with_ "detailed-res" ~args:["prover",prover] @@ fun () ->
  Misc.err_with
    ~map_err:(Printf.sprintf "in get_res for '%s' on '%s':\n%s" prover file)
    (fun scope ->
       let tags = Prover.tags_of_db db in
       let res: Prover.name Run_result.t =
         Db.exec db
           {|select
                res, file_expect, timeout, errcode, stdout, stderr,
                rtime, utime, stime
             from prover_res where prover=? and file=? ; |}
           prover file
           ~f:Db.Cursor.next
           ~ty:Db.Ty.(p2 text text,
                      p2 any_str any_str @>>
                      p4 int int (nullable blob) (nullable blob)
                      @>> p3 float float float,
                      fun x1 x2 x3 x4 x5 x6 x7 x8 x9 ->
                        Logs.info (fun k->k "got results");
                        x1,x2,x3,x4,x5,x6,x7,x8,x9)
         |> E.map_err
           (fun e -> Printf.sprintf "sqlite error: %s" (Db.Rc.to_string e))
         |> scope.unwrap
         |> CCOpt.to_result_lazy
           (fun () ->
              Printf.sprintf
                "expected a non-empty result for prover='%s', file='%s'"
                prover file)
         |> scope.unwrap
         |> (fun (res, file_expect, timeout, errcode, stdout, stderr, rtime, utime, stime) ->
             (* Still needed ?! *)
             Gc.compact();
             Gc.full_major();
             Logs.info (fun k->k "got results 2");
             let stdout=CCOpt.get_or ~default:"" stdout in
             let stderr=CCOpt.get_or ~default:"" stderr in
             Logs.debug (fun k->k"res.of_string tags=[%s]" (String.concat "," tags));
             let expected = Res.of_string ~tags file_expect in
             Logs.debug (fun k->k"got expected %a" Res.pp expected);
             let res = Res.of_string ~tags res in
             Logs.debug (fun k->k"got res %a" Res.pp res);
             let timeout = Limit.Time.mk ~s:timeout () in
             Run_result.make prover ~timeout ~res
               {Problem.name=file; expected}
               { Proc_run_result.errcode;stdout;stderr;rtime;utime;stime})
       in
       Logs.info (fun k->k "try to get prover");
       let prover = Prover.of_db db prover |> scope.unwrap in
       Logs.info (fun k->k "got prover");
       Run_result.map ~f:(fun _ -> prover) res
    )

let to_printbox ?link:(mk_link=default_pp_linker) (self:t) : PB.t*PB.t*string*string =
  let open PB in
  Logs.debug (fun k->k "coucou");
  let pp_res r =
    match r with
    | Res.Sat | Res.Unsat -> text_with_style Style.(set_fg_color Green@@bold) @@ Res.to_string r
    | Res.Error -> text_with_style Style.(set_fg_color Red@@bold) @@ Res.to_string r
    | _ -> text @@ Res.to_string r
  in
  Logs.debug (fun k->k "prover is %a" Prover.pp self.program);
  v_record @@ [
    "problem.path", mk_link self.program.Prover.name self.problem.Problem.name;
    "problem.expected_res", pp_res self.problem.Problem.expected;
    "res", pp_res self.res;
    "rtime", text (Misc.human_duration self.raw.rtime);
    "stime", text (Misc.human_duration self.raw.stime);
    "utime", text (Misc.human_duration self.raw.utime);
    "errcode", int self.raw.errcode;
  ],
  v_record @@ List.flatten [
    ["prover.name", text self.program.Prover.name;
     "prover.cmd", text self.program.Prover.cmd;
     "prover.version",
     (text @@ Format.asprintf "%a" Prover.Version.pp self.program.Prover.version);
     "prover.sat", text (CCOpt.get_or ~default:"<none>" self.program.Prover.sat);
     "prover.unsat", text (CCOpt.get_or ~default:"<none>" self.program.Prover.unsat);
     "prover.unknown", text (CCOpt.get_or ~default:"<none>" self.program.Prover.unknown);
     "prover.timeout", text (CCOpt.get_or ~default:"<none>" self.program.Prover.timeout);];
    CCList.map (fun (tag,re) -> "prover.tag." ^ tag, text re) self.program.Prover.custom;
    ["prover.memory", text (CCOpt.get_or ~default:"<none>" self.program.Prover.memory);
    ]
  ],
  self.raw.stdout, self.raw.stderr
