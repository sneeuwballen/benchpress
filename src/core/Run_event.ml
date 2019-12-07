(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

module E = CCResult
module J = Misc.Json

type 'a or_error = ('a, string) CCResult.t

type prover  = Prover.t
type checker = unit

type +'a result = {
  program : 'a;
  problem : Problem.t;
  timeout: int;
  raw : Proc_run_result.t;
}

let analyze_p_opt self = Prover.analyze_p_opt self.program self.raw
let analyze_p (self:_ result) =
  match analyze_p_opt self with
  | Some x -> x
  | None ->
    if self.raw.errcode = 0 then Res.Unknown
    else if self.raw.rtime > float self.timeout then Res.Timeout
    else Res.Error

type t =
  | Prover_run of prover result
  | Checker_run of checker result

type event = t

let program e = e.program
let problem e = e.problem
let raw e = e.raw

let pp_inner pp_prog out (r:_ result): unit =
  Format.fprintf out "(@[<hv2>:program %a@ :problem %a@ :raw %a@])"
    pp_prog (program r) Problem.pp (problem r) Proc_run_result.pp (raw r)

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

let encode_result f self =
  let open J.Encode in
  let {program; problem; timeout; raw} = self in
  obj [
    "program", f program;
    "problem", Problem.encode problem;
    "timeout", int timeout;
    "raw", Proc_run_result.encode raw;
  ]

let decode_result f =
  let open J.Decode in
  field "problem" Problem.decode >>= fun problem ->
  field "timeout" int >>= fun timeout ->
  field "program" f >>= fun program ->
  field "raw" Proc_run_result.decode >>= fun raw ->
  succeed {problem;timeout;program;raw}

let encode self =
  let open J.Encode in
  match self with
  | Prover_run r -> list value [string "prover"; encode_result Prover.encode r]
  | Checker_run r -> list value [string "prover"; encode_result (fun ()->null) r]

let decode =
  let open J.Decode in
  string >>:: function
  | "prover" -> (list1 (decode_result Prover.decode) >|= fun r -> Prover_run r)
  | "checker" -> (list1 (decode_result @@ succeed ()) >|= fun r -> Checker_run r)
  | _ -> fail "expected prover/checker run event"
