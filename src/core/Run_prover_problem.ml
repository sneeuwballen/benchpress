(* This file is free software. See file "license" for more details. *)

open Common

module Log = (val Logs.src_log (Logs.Src.create "benchpress.run-prover-pb"))

type path = string
type job_res= (Prover.name, Res.t) Run_result.t
type check_res = (Prover.name * Proof_checker.name, Proof_check_res.t) Run_result.t

(* run one particular test *)
let run ~limits ~proof_file prover pb : _ Run_result.t =
  Error.guard (Error.wrapf "run prover '%s' on problem '%s' (proof file %a)"
                 prover.Prover.name pb.Problem.name
                 Fmt.Dump.(option string) proof_file) @@ fun () ->
  Profile.with_ "run-prover" ~args:["prover",prover.Prover.name] @@ fun () ->

  let timeout = CCOpt.get_or ~default:Limit.Time.(mk ~s:30 ()) limits.Limit.All.time in
  Log.info
    (fun k->k"running %-15s/%-30s (timeout %a)..."
        prover.Prover.name pb.Problem.name Limit.Time.pp timeout);
  Log.debug (fun k->k"proof file: %a" Fmt.Dump.(option string) proof_file);
  (* spawn process *)
  let raw = Prover.run ?proof_file ~limits ~file:pb.Problem.name prover in
  let result =
    Run_result.make_from_prover prover ~timeout pb raw
  in
  Logs.debug
    (fun k->
       let open Run_proc_result in
       k "output for %s/%s: `%s`, `%s`, errcode %d"
         prover.Prover.binary pb.Problem.name
         raw.stdout raw.stderr raw.errcode);
  result

let run_proof_check ~limits prover checker pb ~proof_file : check_res =
  Error.guard (Error.wrapf "run checker '%s' on proof file '%s' (problem '%s', prover '%s')"
                 checker.Proof_checker.name proof_file pb.Problem.name
                 prover.Prover.name) @@ fun () ->
  Profile.with_ "run-checker" ~args:["checker",checker.name] @@ fun () ->

  let raw =
    Proof_checker.run ~limits ~problem:pb.Problem.name ~proof_file checker
  in
  let res = Run_result.make_from_checker prover checker
      ~timeout:Limit.Time.default pb raw
  in
  res

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

let pp_check_result ~w_prover ~w_pb out (res:check_res): unit =
  let pp_res out () : unit =
    match res.res with
    | Proof_check_res.Valid -> Fmt.fprintf out "@{<Green>valid@}"
    | Proof_check_res.Invalid -> Fmt.fprintf out "@{<Red>invalid@}"
    | Proof_check_res.Unknown reason ->
      Fmt.fprintf out "@{<Yellow>check failure@} (%s)" reason
  in
  let prover, checker = res.program in
  let prover_name = Filename.basename prover in
  let pb_name = res.problem.Problem.name in
  Logs.info (fun k->k "checking proof of `%s` with `%s` on %s: %s (%.1fs)"
                prover_name checker pb_name (Proof_check_res.to_string res.res) res.raw.rtime
                );
  Fmt.fprintf out
    "%-*s%-*s%-*s : %a (%.1fs)@."
    w_prover (Misc.truncate_right w_prover prover_name)
    w_prover (Misc.truncate_right w_prover checker)
    w_pb (Misc.truncate_left w_pb pb_name)
    pp_res () res.raw.rtime;
  ()

let pp_check_result_progress ~w_prover ~w_pb r : unit =
  Misc.synchronized
    (fun () ->
       Format.printf "%s%a" Misc.reset_line (pp_check_result ~w_prover ~w_pb) r);
  ()
