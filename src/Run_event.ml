
(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

module E = CCResult

type 'a or_error = ('a, string) CCResult.t

type raw_result = {
  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
}

type prover  = Prover.t
type checker = unit

type +'a result = {
  program : 'a;
  problem : Problem.t;
  raw : raw_result;
}

let analyze_p t =
  let prover = t.program in
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re ->
      Re.execp (Re.Perl.compile_pat re) t.raw.stdout ||
      Re.execp (Re.Perl.compile_pat re) t.raw.stderr
  in
  if find_opt_ prover.Prover.sat then Res.Sat
  else if find_opt_ prover.Prover.unsat then Res.Unsat
  else if find_opt_ prover.Prover.timeout then Res.Timeout
  else if find_opt_ prover.Prover.unknown then Res.Unknown
  else if t.raw.errcode = 0 then Res.Unknown else Res.Error

type t =
  | Prover_run of prover result
  | Checker_run of checker result

type event = t

let program e = e.program
let problem e = e.problem
let raw e = e.raw

let pp_raw out (r:raw_result): unit =
  Format.fprintf out
    "(@[:errcode %d@ rtime %.2f@ :utime %.2f@ :stime %.2f@ \
     :stdout %S@ :stderr %S@])"
    r.errcode r.rtime r.utime r.stime r.stdout r.stderr

let pp_inner pp_prog out (r:_ result): unit =
  Format.fprintf out "(@[<hv2>:program %a@ :problem %a@ :raw %a@])"
    pp_prog (program r) Problem.pp (problem r) pp_raw (raw r)

let pp out = function
  | Prover_run r -> pp_inner Prover.pp_name out r
  | Checker_run r -> pp_inner CCFormat.unit out r

let to_string = CCFormat.to_string pp

let mk_prover r = Prover_run r
let mk_checker r = Checker_run r

type timestamp = float

type snapshot = {
  timestamp: timestamp;
  events: t list;
  meta: string; (* additional metadata *)
}

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

module Snapshot = struct
  type t = snapshot

  let make ?(meta="") ?(timestamp=Unix.gettimeofday()) l =
    { timestamp; events=l; meta; }

  let pp out (r:t) =
    Format.fprintf out
      "(@[<hv>snapshot@ :timestamp %.2f@ :events (@[<v>%a@])@])"
      r.timestamp CCFormat.(list ~sep:(return "@ ") pp) r.events

  let provers t =
    List.fold_left
      (fun set e -> match e with
         | Prover_run {program=p;_} ->
           Prover.Set.add p set
         | Checker_run _ -> set)
      Prover.Set.empty
      t.events
end

type prover_set = Prover.Set.t

type snapshot_meta = {
  s_timestamp: float;
  s_meta: string;
  s_provers: prover_set;
  s_len: int;
}

module Meta = struct
  type t = snapshot_meta
  let timestamp s = s.s_timestamp
  let provers s = s.s_provers
  let length s = s.s_len

  let pp out r: unit =
    Format.fprintf out
      "(@[<hv>meta@ :timestamp %.2f@ :len %d@])" r.s_timestamp r.s_len
end

let meta s = {
  s_timestamp=s.timestamp;
  s_meta=s.meta;
  s_provers=Snapshot.provers s;
  s_len=List.length s.events;
}
