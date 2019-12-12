(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
module E = CCResult

type 'a or_error = ('a, string) CCResult.t
type path = string
type job_res= Prover.t Run_event.result

(* run one particular test *)
let run_exn_ ~timeout ~memory prover pb =
  Logs.info
    (fun k->k"running %-15s/%-30s (timeout %d)..."
        prover.Prover.name pb.Problem.name timeout);
  (* spawn process *)
  let raw = Prover.run ~timeout ~memory ~file:pb.Problem.name prover in
  let result = 
    { Run_event.program = prover; timeout; problem = pb; raw; }
  in
  Logs.debug
    (fun k->
       let open Proc_run_result in
       k "output for %s/%s: `%s`, `%s`, errcode %d"
        prover.Prover.binary pb.Problem.name
        result.Run_event.raw.stdout
        result.Run_event.raw.stderr
        result.Run_event.raw.errcode);
  result

let run ~timeout ~memory prover pb : _ E.t =
  try run_exn_ ~timeout ~memory prover pb |> E.return
  with e -> E.of_exn_trace e

let pp_result ~w_prover ~w_pb out (res:Test.result): unit =
  let p_res = Run_event.analyze_p res in
  let pp_res out () : unit =
    let str = ""^^match Problem.compare_res res.Run_event.problem p_res with
      | `Same -> "@{<Green>ok@}"
      | `Improvement -> "@{<Green>ok@} @{<blue>(improved)@}"
      | `Disappoint -> "@{<Cyan>disappoint@}"
      | `Error -> "@{<Yellow>error@}"
      | `Mismatch -> "@{<Red>bad@}"
    in
    Format.fprintf out str
  in
  let prover = res.Run_event.program in
  let prover_name = Filename.basename prover.Prover.name in
  let pb_name = res.Run_event.problem.Problem.name in
  Logs.info (fun k->k "result for `%s` with %s: %s (%.1fs)"
      prover_name pb_name (Res.to_string p_res) res.Run_event.raw.rtime);
  Format.fprintf out 
    "%-*s%-*s : %a (%.1fs)@."
    w_prover (Misc.truncate_right w_prover prover_name)
    w_pb (Misc.truncate_left w_pb pb_name)
    pp_res () res.Run_event.raw.rtime;
  ()

let pp_result_progress ~w_prover ~w_pb r : unit =
  Misc.synchronized
    (fun () ->
       Format.printf "%s%a" Misc.reset_line (pp_result ~w_prover ~w_pb) r);
  ()
