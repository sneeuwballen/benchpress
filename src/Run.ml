
(* This file is free software. See file "license" for more details. *)

module E = CCResult

let fpf = Format.fprintf

(** {2 Main} *)

(* Start processes *)
type env = (string * string) array

let mk_cmd ?env ~timeout ~memory ~prover ~file () =
  Misc.Debug.debugf 5 (fun k->k "mk_cmd timeout: %d, memory: %d" timeout memory);
  (* limit time and memory ('v' is virtual memory, needed because 'm' ignored on linux) *)
  let memory' = memory * 1000 in
  let prefix =
    Printf.sprintf "ulimit -t %d -m %d -Sv %d; " timeout memory' memory'
  in
  let cmd = Prover.make_command ?env prover ~timeout ~memory ~file in
  prefix ^ cmd

let run_proc ~timeout cmd =
  let start = Unix.gettimeofday () in
  (* call process and block *)
  let p = CCUnix.call_full "%s" cmd in
  let errcode = p#errcode in
  Misc.Debug.debugf 5 (fun k->k "errcode: %d\n" errcode);
    (* Compute time used by the prover *)
  let rtime = Unix.gettimeofday () -. start in
  let utime = 0. in
  let stime = 0. in
  let stdout = p#stdout in
  let stderr = p#stderr in
  { Event.stdout; stderr; errcode; rtime; utime; stime; }

let run_prover ?env ~timeout ~memory ~prover ~pb () =
  let file = pb.Problem.name in
  let cmd = mk_cmd ?env ~timeout ~memory ~prover ~file () in
  let raw = run_proc ~timeout cmd in
  { Event.program = prover; problem = pb; raw; }
