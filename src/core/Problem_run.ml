(* This file is free software. See file "license" for more details. *)

module E = CCResult

type 'a or_error = ('a, string) CCResult.t

type t = Problem.t
type path = string

(* regex + mark *)
let m_unsat_, unsat_ = Re.(str "unsat" |> no_case |> mark)
let m_sat_, sat_ = Re.(str "sat" |> no_case |> mark)
let m_unknown_, unknown_ = Re.(str "unknown" |> no_case |> mark)
let m_timeout_, timeout__ = Re.(str "timeout" |> no_case |> mark)
let m_error_, error_ = Re.(alt [str "error"; str "fail"] |> no_case |> mark)

(* "^ #expect: (unsat|sat|unknown|error)", basically *)
let re_expect_ =
  Re.(seq
        [ alt (List.map no_case [str "expect:"; str "expected:"]) ; rep blank;
          alt [unsat_; sat_; unknown_; error_] ]
      |> compile
     )

(* what is expected? *)
let find_expected_ ?default file =
  let content = CCIO.with_in file CCIO.read_all in
  begin match Re.exec_opt re_expect_ content with
    | Some g ->
      if Re.Mark.test g m_unsat_ then E.return Res.Unsat
      else if Re.Mark.test g m_sat_ then E.return Res.Sat
      else if Re.Mark.test g m_unknown_ then E.return Res.Unknown
      else if Re.Mark.test g m_timeout_ then E.return Res.Timeout
      else if Re.Mark.test g m_error_ then E.return Res.Error
      else E.fail_fprintf "could not parse the content of the `expect:` field in `%s`" file
    | None ->
      begin match default with
        | Some r -> E.return r
        | None ->
          E.fail_fprintf "could not find the `expect:` field in `%s`" file
      end
  end

let find_expect ?default_expect ~expect file : Res.t or_error =
  Misc.Debug.debugf 3 (fun k->k "find_expect `%s`…" file);
  begin match expect with
    | Test.Config.Auto -> find_expected_ ?default:None file
    | Test.Config.Res r -> find_expected_ ~default:r file
    | Test.Config.Program prover ->
      let pb = Problem.make file Res.Unknown in
      let event = Run.run_prover ~timeout:1 ~memory:1_000 ~prover ~pb () in
      match Run_event.analyze_p_opt event, default_expect with
      | Some r, _ -> E.return r
      | None, Some r -> E.return r
      | None, None ->
        E.fail_printf "cannot find expect for problem `%s`" file
  end

let make ~expect:res file =
  let pb = Problem.make file res in
  E.return pb

let of_dir ~filter d =
  try
    CCIO.File.walk_l d
    |> CCList.filter_map
      (fun (kind,f) -> match kind with
         | `File when filter f -> Some f
         | _ -> None)
    |> E.return
  with e ->
    E.of_exn e |> E.add_ctxf "of_dir %S" d

module Set = struct
  type t = Problem.problem_set

  let size = List.length

  let make ~find_expect l =
    let open E.Infix in
    E.map_l
      (fun pb ->
         find_expect pb >>= fun res ->
         make ~expect:res pb)
      l
    (* sort by alphabetic order *)
    >|= List.sort Problem.compare_name

  let of_dir ~expect ~filter d =
    let open E.Infix in
    of_dir ~filter d >>= fun l ->
    make ~find_expect:(find_expect ~expect) l

  let pp out set =
    Format.fprintf out "(@[<hv>%a@])"
      (CCFormat.list ~sep:(CCFormat.return "@ ") Problem.pp) set
end
