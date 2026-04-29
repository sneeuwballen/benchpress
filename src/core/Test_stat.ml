open Common
open Test
module PB = PrintBox

(** Aggregate function computing the mean and the standard deviation of data
    series using the Welford's algorithm. *)
module Stats = struct
  type acc = { n: int; total: float; mean: float * float; s: float * float }

  let prev = fst
  let curr = snd
  let init = { n = 0; total = 0.; mean = 0., 0.; s = 0., 0. }

  let step ({ n; total; mean; s } as acc) (x : float) =
    let n = n + 1 in
    let total = total +. x in
    if n = 1 then
      { acc with n; total; mean = x, x }
    else (
      let new_mean = prev mean +. ((x -. prev mean) /. Float.of_int n) in
      let new_s = prev s +. ((x -. prev mean) *. (x -. new_mean)) in
      { n; total; mean = curr mean, new_mean; s = curr s, new_s }
    )

  let total { total; _ } = total

  let mean { n; mean = _, mean; _ } =
    if n = 0 then
      0.
    else
      mean

  let sd { n; s = _, s; _ } =
    if n <= 1 then
      0.
    else
      Float.(sqrt (s /. of_int (n - 1)))
end

type detail_stats = { n: int; total: float; mean: float; sd: float }

type t = {
  unsat: detail_stats;
  sat: detail_stats;
  errors: int;
  unknown: detail_stats;
  timeout: int;
  memory: int;
  valid_proof: int;
  invalid_proof: int;
  custom: (string * detail_stats) list;
  total: int;
  total_time: float; (* for sat+unsat *)
}

let get_unsat r = r.unsat
let get_sat r = r.sat
let get_errors r = r.errors
let get_unknown r = r.unknown
let get_timeout r = r.timeout
let get_valid_proof r = r.valid_proof
let get_invalid_proof r = r.invalid_proof
let get_memory r = r.memory

let get_custom s r =
  try List.assoc s r.custom
  with Not_found -> { n = 0; total = 0.; mean = 0.; sd = 0. }

let get_total r = r.total
let get_total_time r = r.total_time

let pb_details_stats_color ~details c { n; total; mean; sd } =
  let open PB in
  if n = 0 || not details then
    align ~h:`Left ~v:`Center (int n)
  else
    v_record
      [
        "number", pb_int_color c n;
        "total", text @@ Misc.human_duration total;
        "mean", text @@ Misc.human_duration mean;
        "deviation", text @@ Misc.human_duration sd;
      ]

let to_printbox_l ?(details = false) ?to_link l : PB.t =
  let mk_row ?res lbl get_r mk_box : PB.t list =
    match to_link with
    | Some f ->
      let uri p =
        let res = CCOpt.get_or ~default:lbl res in
        f p res
      in
      PB.text lbl
      :: List.map
           (fun (p, r) ->
             let n = get_r r in
             if n > 0 then
               PB.link ~uri:(uri p) (mk_box n)
             else
               mk_box n)
           l
    | _ -> PB.text lbl :: List.map (fun (_p, r) -> mk_box (get_r r)) l
  and mk_row1 lbl get_r mk_box =
    PB.(align ~h:`Left ~v:`Center (text lbl))
    :: List.map (fun (_, r) -> mk_box @@ get_r r) l
  in
  let r1 =
    [
      mk_row1 "sat" get_sat
      @@ pb_details_stats_color ~details PB.Style.(fg_color Green);
      mk_row1 "unsat" get_unsat
      @@ pb_details_stats_color ~details PB.Style.(fg_color Green);
      mk_row1 "sat+unsat" (fun r -> r.sat.n + r.unsat.n)
      @@ pb_int_color PB.Style.(fg_color Green);
      mk_row ~res:"error" "errors" get_errors
      @@ pb_int_color PB.Style.(fg_color Cyan);
      mk_row1 "valid_proof" get_valid_proof
      @@ pb_int_color PB.Style.(fg_color Green);
      mk_row1 "invalid_proof" get_invalid_proof
      @@ pb_int_color PB.Style.(fg_color Red);
      mk_row1 "unknown" get_unknown
      @@ pb_details_stats_color ~details PB.Style.(fg_color White);
      mk_row "timeout" get_timeout PB.int;
    ]
  and r2 =
    let all_custom =
      CCList.flat_map (fun (_, s) -> List.map fst s.custom) l
      |> CCList.sort_uniq ~cmp:String.compare
    in
    CCList.map
      (fun tag ->
        mk_row ~res:tag ("tag." ^ tag) (fun t -> (get_custom tag t).n) PB.int)
      all_custom
  and r3 =
    [
      mk_row1 "memory" get_memory PB.int;
      mk_row1 "total" get_total PB.int;
      mk_row1 "total_time" get_total_time (fun s ->
          PB.line (Misc.human_duration s));
    ]
  in
  let header = List.map PB.text @@ ("provers" :: List.map fst l) in
  let rows = r1 @ r2 @ r3 in
  PB.grid_l ~bars:true (header :: rows)

let of_events_for ~(prover : Prover.name) (events : Run_event.t list) : t =
  let acc_by_res : (string, Stats.acc) Hashtbl.t = Hashtbl.create 8 in
  let get_acc res =
    match Hashtbl.find_opt acc_by_res res with
    | Some a -> a
    | None ->
      let a = Stats.init in
      Hashtbl.add acc_by_res res a;
      a
  in
  let valid_proof = ref 0 in
  let invalid_proof = ref 0 in
  let invalid_proof_full = ref [] in
  List.iter
    (function
      | Run_event.Prover_run r when r.Run_result.program = prover ->
        let res_s = Res.to_string r.Run_result.res in
        let rtime = r.Run_result.raw.Run_proc_result.rtime in
        Hashtbl.replace acc_by_res res_s (Stats.step (get_acc res_s) rtime)
      | Run_event.Checker_run r when fst r.Run_result.program = prover ->
        (match r.Run_result.res with
        | Proof_check_res.Valid -> incr valid_proof
        | Proof_check_res.Invalid ->
          incr invalid_proof;
          invalid_proof_full :=
            ( r.Run_result.problem,
              r.Run_result.res,
              r.Run_result.raw.Run_proc_result.rtime )
            :: !invalid_proof_full
        | Proof_check_res.Unknown _ -> ())
      | _ -> ())
    events;
  let detail res_s =
    match Hashtbl.find_opt acc_by_res res_s with
    | None -> { n = 0; total = 0.; mean = 0.; sd = 0. }
    | Some a ->
      {
        n = a.Stats.n;
        total = Stats.total a;
        mean = Stats.mean a;
        sd = Stats.sd a;
      }
  in
  let sat = detail "sat" in
  let unsat = detail "unsat" in
  let unknown = detail "unknown" in
  let timeout = (detail "timeout").n in
  let memory = (detail "memory").n in
  let errors = (detail "error").n in
  let standard_keys =
    [ "sat"; "unsat"; "unknown"; "timeout"; "memory"; "error" ]
  in
  let custom =
    Hashtbl.fold
      (fun k _ acc ->
        if List.mem k standard_keys then
          acc
        else
          (k, detail k) :: acc)
      acc_by_res []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let total =
    sat.n + unsat.n + unknown.n + timeout + memory + errors
    + List.fold_left (fun n (_, d) -> n + d.n) 0 custom
  in
  let total_time = sat.total +. unsat.total in
  ignore invalid_proof_full;
  {
    sat;
    unsat;
    errors;
    unknown;
    timeout;
    memory;
    valid_proof = !valid_proof;
    invalid_proof = !invalid_proof;
    custom;
    total;
    total_time;
  }

let of_events (events : Run_event.t list) : (Prover.name * t) list =
  let provers =
    List.filter_map
      (function
        | Run_event.Prover_run r -> Some r.Run_result.program
        | _ -> None)
      events
    |> List.sort_uniq String.compare
  in
  List.map (fun p -> p, of_events_for ~prover:p events) provers

let pp out (s : t) : unit =
  Fmt.fprintf out
    "(@[<hv>:unsat %d@ :sat %d@ :solved %d@ :errors %d@ :unknown %d@ \
     :valid-proof %d@ :invalid-proof %d@ :timeout %d@ :total %d@ :total_time \
     %.2f@])"
    s.unsat.n s.sat.n (s.sat.n + s.unsat.n) s.errors s.unknown.n s.valid_proof
    s.invalid_proof s.timeout
    (s.unsat.n + s.sat.n + s.errors + s.unknown.n + s.timeout)
    s.total_time
