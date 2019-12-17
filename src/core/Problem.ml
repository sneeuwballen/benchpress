(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
module E = CCResult
module J = Misc.Json

type 'a or_error = ('a, string) CCResult.t

type path = string
type t = {
  name: path;  (* filename *)
  expected: Res.t; (* result expected *)
}

let src_log = Logs.Src.create "problem"
let basename t = Filename.basename t.name

let same_name t1 t2 = t1.name = t2.name
let hash_name t = CCHash.string t.name
let compare_name t1 t2 = CCOrd.compare t1.name t2.name

let make name expected =
  { name; expected; }

module Exp_ = struct
  (* regex + mark *)
  let m_unsat_, unsat_ = Re.(str "unsat" |> no_case |> mark)
  let m_sat_, sat_ = Re.(str "sat" |> no_case |> mark)
  let m_unknown_, unknown_ = Re.(str "unknown" |> no_case |> mark)
  let m_timeout_, _timeout = Re.(str "timeout" |> no_case |> mark)
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
end

(* find expected result for [file] *)
let find_expect ?default_expect ~expect file : Res.t or_error =
  Logs.debug ~src:src_log
    (fun k->k "(@[<2>find_expect `%s`@ using %a@])…" file Dir.pp_expect expect);
  let rec loop expect =
    match expect with
    | Dir.E_comment -> Exp_.find_expected_ ?default:default_expect file
    | Dir.E_const r -> Ok r
    | Dir.E_try l ->
      let rec try_ = function
        | [] ->
          E.fail_fprintf "no method for finding expect succeeded on %S" file
        | e :: tl ->
          match loop e with
          | Error _ -> try_ tl
          | Ok _ as res -> res
      in
      try_ l
    | Dir.E_program {prover} ->
      let raw = Prover.run ~timeout:1 ~memory:1_000 ~file prover in
      match Prover.analyze_p_opt prover raw, default_expect with
      | Some r, _ -> E.return r
      | None, Some r -> E.return r
      | None, None ->
        E.fail_printf "cannot find expect for problem `%s`" file
  in
  loop expect

let make_find_expect ~expect file : t or_error =
  let open E.Infix in
  find_expect ~expect file >|= fun expect ->
  make file expect

let compare_res pb res = match pb.expected, res with
  | Res.Unsat, Res.Unsat
  | Res.Sat, Res.Sat
  | Res.Timeout, Res.Timeout
  | Res.Unknown, Res.Unknown
  | Res.Error, Res.Error -> `Same
  | Res.Timeout, Res.Unknown
  | Res.Unknown, Res.Timeout
  | (Res.Sat | Res.Unsat | Res.Error), (Res.Unknown | Res.Timeout) -> `Disappoint
  | (Res.Unsat | Res.Error), Res.Sat
  | (Res.Sat | Res.Error), Res.Unsat -> `Mismatch
  | (Res.Sat | Res.Unknown | Res.Timeout | Res.Unsat), Res.Error ->
    `Error
  | (Res.Unknown | Res.Timeout), (Res.Sat | Res.Unsat) ->
    `Improvement

let pp out p =
  Format.fprintf out "@[<h>%s (expect: %a)@]" p.name Res.pp p.expected

let name p = p.name
let to_string = CCFormat.to_string pp

let decode =
  let open J.Decode in
  field "name" string >>= fun name ->
  field "expected" Res.decode >>= fun expected ->
  succeed {name; expected}
