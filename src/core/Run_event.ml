(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

module E = CCResult
module J = Misc.Json

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
  timeout: int;
  raw : raw_result;
}

let analyze_p_opt t =
  let prover = t.program in
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re ->
      let re = Re.Perl.compile_pat ~opts:[`Multiline] re in
      Re.execp re t.raw.stdout ||
      Re.execp re t.raw.stderr
  in
  if find_opt_ prover.Prover.sat then Some Res.Sat
  else if find_opt_ prover.Prover.unsat then Some Res.Unsat
  else if find_opt_ prover.Prover.timeout then Some Res.Timeout
  else if find_opt_ prover.Prover.unknown then Some Res.Unknown
  else None

let analyze_p t =
  match analyze_p_opt t with
  | Some x -> x
  | None ->
    if t.raw.errcode = 0 then Res.Unknown
    else if t.raw.rtime > 0.1 +. float t.timeout then Res.Timeout
    else Res.Error

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

let encode_raw_result r =
  let open J.Encode in
  let {errcode;stdout;stderr;rtime;utime;stime} = r in
  obj [
    "errcode", int errcode;
    "stdout", string stdout;
    "stderr", string stderr;
    "rtime", float rtime;
    "stime", float stime;
    "utime", float utime;
  ]

let decode_raw_result =
  let open J.Decode in
  field "errcode" int >>= fun errcode ->
  field "stdout" string >>= fun stdout ->
  field "stderr" string >>= fun stderr ->
  field "rtime" float >>= fun rtime ->
  field "stime" float >>= fun stime ->
  field "utime" float >>= fun utime ->
  succeed {stderr;stdout; errcode; stime; rtime; utime}

let encode_result f self =
  let open J.Encode in
  let {program; problem; timeout; raw} = self in
  obj [
    "program", f program;
    "problem", Problem.encode problem;
    "timeout", int timeout;
    "raw", encode_raw_result raw;
  ]

let decode_result f =
  let open J.Decode in
  field "problem" Problem.decode >>= fun problem ->
  field "timeout" int >>= fun timeout ->
  field "program" f >>= fun program ->
  field "raw" decode_raw_result >>= fun raw ->
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
