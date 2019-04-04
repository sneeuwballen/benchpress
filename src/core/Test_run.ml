
(* This file is free software. See file "license" for more details. *)

(** {1 Run Tests} *)

type 'a or_error = ('a, string) CCResult.t
type path = string

module C = Test.Config
module T = Test
module E = CCResult

let expect_of_config config = function
  | None -> E.return C.Auto
  | Some s ->
    let open E.Infix in
    begin match CCString.Split.left ~by:":" s with
      | Some ("program", p) ->
        Prover_set.find_config config p >|= fun p -> C.Program p
      | _ -> E.return (C.Res (Res.of_string s))
      | exception Not_found -> E.return (C.Res (Res.of_string s))
    end

let config_of_config ?(profile="test") config dirs =
  let getter =
    let open Config in
    let tbl = table profile in
    (try_tables [tbl; top] @@ int "parallelism" <|> pure 1) >>= fun j ->
    (try_tables [tbl; top] @@ int "timeout" <|> pure 5) >>= fun timeout ->
    (try_tables [tbl; top] @@ int "memory" <|> pure 1000) >>= fun memory ->
    let problem_pat =
      try_tables [tbl; top] @@ string "problems"
    in
    let default_expect =
      (try_tables [tbl; top] @@ string "default_expect" >|= fun x-> Some x)
      <|> pure None
    in
    begin match dirs with
      | [] -> try_tables [tbl; top] @@ string_list ~default:[] "dir"
      | _ -> pure dirs
    end >>= fun l ->
    map_l
      (fun dir_name ->
         let dir_tbl = tbl |>> table dir_name in
         begin
         (dir_tbl |>> string "directory" <|> pure dir_name) >>= fun dir ->
         (dir_tbl |>> string "problems" <|> problem_pat) >>= fun pat ->
         ( (( some @@ try_tables [dir_tbl; tbl; top] @@ string "expect")
            <|> default_expect)
           >>= fun e -> (expect_of_config config e |> pure_or_error) )
         >|= fun expect ->
         { C.directory = dir; pattern = pat; expect; }
         end |> add_ctxf "read config for directory `%s`" dir_name)
      l
    >>= fun problems ->
    ((try_tables [tbl; top] @@ string_list "provers") |> add_ctxf "get provers")
    >>= fun provers ->
    map_l
      (fun p -> Prover_set.find_config config p |> pure_or_error)
      provers
    >>= fun provers ->
    (default_expect >|= fun e-> CCOpt.flat_map (fun s->try Some(Res.of_string s) with _ -> None) e)
    >>= fun default_expect ->
    return { C.j; timeout; memory; provers; problems; default_expect }
  in
  Config.get config getter

let config_of_file ?profile file =
  Misc.Debug.debugf 1 (fun k->k "parse config file `%s`..." file);
  let open E.Infix in
  Config.parse_file file >>= fun c ->
  config_of_config ?profile c []

(* run one particular test *)
let run_pb_ ~config prover pb =
  Misc.Debug.debugf 2 (fun k->k "running %-15s/%-30s..."
    (Filename.basename prover.Prover.binary) pb.Problem.name);
  (* spawn process *)
  let result = Run.run_prover
      ~timeout:config.C.timeout
      ~memory:config.C.memory
      ~prover ~pb ()
  in
  Misc.Debug.debugf 4 (fun k->k "output for %s/%s: `%s`, `%s`, errcode %d"
    prover.Prover.binary pb.Problem.name
    result.Run_event.raw.Run_event.stdout
    result.Run_event.raw.Run_event.stderr
    result.Run_event.raw.Run_event.errcode);
  result

let run_pb ~config prover pb : _ E.t =
  try
    run_pb_ ~config prover pb |> E.return
  with e ->
    E.of_exn e

let pp_result ~w_prover ~w_pb (res:Test.result): unit =
  let module F = CCFormat in
  let p_res = Run_event.analyze_p res in
  let pp_res out () : unit =
    let str, c = match Problem.compare_res res.Run_event.problem p_res with
      | `Same -> "ok", "Green"
      | `Improvement -> "ok (improved)", "Blue"
      | `Disappoint -> "disappoint", "Cyan"
      | `Error -> "error", "Yellow"
      | `Mismatch -> "bad", "Red"
    in
    F.with_color c Format.pp_print_string out str
  in
  let prover = res.Run_event.program in
  let prover_name = Filename.basename prover.Prover.name in
  let pb_name = res.Run_event.problem.Problem.name in
  Misc.Debug.debugf 3 (fun k->k "result for `%s` with %s: %s (%.1fs)"
      prover_name pb_name (Res.to_string p_res) res.Run_event.raw.Run_event.rtime);
  Misc.synchronized
    (fun () ->
       Format.printf "%-*s%-*s : %a (%.1fs)@." w_prover prover_name w_pb pb_name
         pp_res () res.Run_event.raw.Run_event.rtime);
  ()

let nop_ _ = ()

let run ?(j=1) ?(on_solve = nop_) ?(on_done = nop_)
    ?timeout ?memory ~provers ~expect ~config (set:path list)
    : Test.top_result or_error =
  let open E.Infix in
  let config = C.update ?timeout ?memory config in
  E.map_l
    (fun pb_path ->
       (* transform into problem *)
       Problem_run.find_expect ?default_expect:config.C.default_expect ~expect pb_path >|= fun expect ->
       pb_path, expect)
    set
  >>= fun l ->
  (* build list of tasks *)
  E.map_l
    (fun (pb_path,expect) ->
       (* transform into problem *)
       Problem_run.make ~expect pb_path >>= fun pb ->
       E.map_l
         (fun prover -> E.return (prover,pb))
         provers)
    l
  >|= CCList.flatten
  >|=
  (* run provers *)
  Misc.Par_map.map_p ~j
    (fun (prover,pb) ->
      begin
        run_pb ~config prover pb >|= fun result ->
        on_solve result; (* callback *)
        result
      end
      |> E.add_ctxf "(@[running :prover %a :on %a@])"
        Prover.pp_name prover Problem.pp pb)
  >>= E.map_l CCFun.id
  >>= fun res ->
  let r = T.Top_result.make (List.map Run_event.mk_prover res) in
  on_done r;
  E.return r
