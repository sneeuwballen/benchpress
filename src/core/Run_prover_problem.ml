(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
module E = CCResult

type 'a or_error = ('a, string) CCResult.t
type path = string
type job_res= Prover.name Run_result.t

(* run one particular test *)
let run_exn_ ~limits prover pb =
  Profile.with_ "run-prover" ~args:["prover",prover.Prover.name] @@ fun () ->

  let timeout = CCOpt.get_or ~default:Limit.Time.(mk ~s:30 ()) limits.Limit.All.time in
  Logs.info
    (fun k->k"running %-15s/%-30s (timeout %a)..."
        prover.Prover.name pb.Problem.name Limit.Time.pp timeout);
  (* spawn process *)
  let raw = Prover.run ~limits ~file:pb.Problem.name prover in
  let result =
    Run_result.make_from_prover prover ~timeout pb raw
  in
  Logs.debug
    (fun k->
       let open Proc_run_result in
       k "output for %s/%s: `%s`, `%s`, errcode %d"
         prover.Prover.binary pb.Problem.name
         raw.stdout raw.stderr raw.errcode);
  result

let run ~limits prover pb : _ E.t =
  try run_exn_ ~limits prover pb |> E.return
  with e -> E.of_exn_trace e

let pp_result ~w_prover ~w_pb out (res:Test.result): unit =
  let pp_res out () : unit =
    let str = ""^^match Problem.compare_res res.problem res.res with
      | `Same -> "@{<Green>ok@}"
      | `Improvement -> "@{<Green>ok@} @{<blue>(improved)@}"
      | `Disappoint -> "@{<Cyan>disappoint@}"
      | `Error -> "@{<Yellow>error@}"
      | `Mismatch -> "@{<Red>bad@}"
    in
    Fmt.fprintf out str
  in
  let prover = res.program in
  let prover_name = Filename.basename prover in
  let pb_name = res.problem.Problem.name in
  Logs.info (fun k->k "result for `%s` with %s: %s (%.1fs, expected %s)"
                prover_name pb_name (Res.to_string res.res) res.raw.rtime
                (Res.to_string res.problem.Problem.expected));
  Fmt.fprintf out
    "%-*s%-*s : %a (%.1fs)@."
    w_prover (Misc.truncate_right w_prover prover_name)
    w_pb (Misc.truncate_left w_pb pb_name)
    pp_res () res.raw.rtime;
  ()

let pp_result_progress ~w_prover ~w_pb r : unit =
  Misc.synchronized
    (fun () ->
       Format.printf "%s%a" Misc.reset_line (pp_result ~w_prover ~w_pb) r);
  ()
