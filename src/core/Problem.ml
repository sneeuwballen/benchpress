(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
open Misc

type 'a or_error = 'a Or_error.t

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
        else E.failf "could not parse the content of the `expect:` field in `%s`" file
      | None ->
        begin match default with
          | Some r -> E.return r
          | None ->
            E.failf "could not find the `expect:` field in `%s`" file
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
          E.failf "no method for finding expect succeeded on %S" file
        | e :: tl ->
          match loop e with
          | Error _ -> try_ tl
          | Ok _ as res -> res
      in
      try_ l
    | Dir.E_program {prover} ->
      let raw = Prover.run prover ~file
          ~limits:(Limit.All.mk
                     ~time:(Limit.Time.mk ~s:1 ())
                     ~memory:(Limit.Memory.mk ~m:200 ())
                     ())
      in
      match Prover.analyze_p_opt prover raw, default_expect with
      | Some r, _ -> E.return r
      | None, Some r -> E.return r
      | None, None ->
        E.failf "cannot find expect for problem `%s`" file
  in
  loop expect

let make_find_expect ~expect file : t or_error =
  let open E.Infix in
  find_expect ~expect file >|= fun expect ->
  make file expect

let compare_res pb res =
  let open Res in
  match pb.expected, res with
  | Unsat, Unsat
  | Sat, Sat
  | Timeout, Timeout
  | Unknown, Unknown
  | Error, Error -> `Same
  | Tag s1, Tag s2 when s1=s2 -> `Same
  | Timeout, Unknown
  | Unknown, Timeout
  | (Sat | Unsat | Error | Tag _), (Unknown | Timeout) -> `Disappoint
  | (Unsat | Sat | Error), Tag _
  | (Unsat | Error | Tag _), Sat
  | Tag _, Tag _
  | (Sat | Error | Tag _), Unsat -> `Mismatch
  | (Sat | Unknown | Timeout | Unsat | Tag _), Error ->
    `Error
  | (Unknown | Timeout), (Sat | Unsat | Tag _) ->
    `Improvement

let pp out p =
  Format.fprintf out "@[<h>%s (expect: %a)@]" p.name Res.pp p.expected

let name p = p.name
let to_string = CCFormat.to_string pp
