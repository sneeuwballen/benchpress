(* This file is free software. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

module E = CCResult
module MStr = Misc.Str_map
module J = Misc.Json
module PB = PrintBox

type result = Run_event.prover Run_event.result

type 'a or_error = ('a, string) CCResult.t

let fpf = Format.fprintf
let spf = Format.asprintf

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

let pp_list_ p =
  CCFormat.within "(" ")"
    (CCFormat.hovbox
       (CCFormat.(list ~sep:(return "@ ") p)))

let pp_hvlist_ p =
  CCFormat.within "(" ")"
    (CCFormat.hvbox
       (CCFormat.(list ~sep:(return "@ ") p)))

let time_of_res e = e.Run_event.raw.Run_event.rtime

let printbox_record l =
  let fields, vals = List.split l in
  PB.grid (* TODO: when fixed in printbox ~pad:PB.align_right *)
    [|Array.of_list @@ List.map PB.text fields; Array.of_list vals|]

module Raw = struct
  type t = result MStr.t
  let empty = MStr.empty

  let add r raw =
    let pb = r.Run_event.problem.Problem.name in
    MStr.add pb r raw

  let merge =
    MStr.merge
      (fun _ a b -> if a=None then b else a)

  let of_list l = List.fold_left (fun acc r -> add r acc) empty l

  type stat = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
    timeout: int;
    total_time: float; (* for sat+unsat *)
  }

  let stat_empty =
    {unsat=0; sat=0; errors=0; unknown=0; timeout=0; total_time=0.; }

  let as_printbox_record s : _ list =
    let open PB in
    [ "sat", int s.sat; "unsat", int s.unsat; "errors", int s.errors;
      "unknown", int s.unknown; "timeout", int s.timeout;
      "total_time", float s.total_time ]


  let printbox_stat (s:stat) : PrintBox.t =
    printbox_record @@ as_printbox_record s

  let add_sat_ t s = {s with sat=s.sat+1; total_time=s.total_time+. t; }
  let add_unsat_ t s = {s with unsat=s.unsat+1; total_time=s.total_time+. t; }
  let add_unknown_ s = {s with unknown=s.unknown+1}
  let add_error_ s = {s with errors=s.errors+1}
  let add_timeout_ s = {s with timeout=s.timeout+1}

  let pp_stat out s =
    fpf out
      "(@[<hv>:unsat %d@ :sat %d@ :solved %d@ :errors %d@ :unknown %d@ \
       :timeout %d@ :total %d@ :total_time %.2f@])"
      s.unsat s.sat (s.sat + s.unsat) s.errors s.unknown s.timeout
      (s.unsat + s.sat + s.errors + s.unknown + s.timeout)
      s.total_time

  let stat r =
    (* stats *)
    let stat = ref stat_empty in
    let add_res time res =
      stat := (match res with
          | Res.Unsat -> add_unsat_ time | Res.Sat -> add_sat_ time
          | Res.Unknown -> add_unknown_ | Res.Error -> add_error_
          | Res.Timeout -> add_timeout_
        ) !stat
    in
    MStr.iter (fun _ r -> add_res (time_of_res r) (Run_event.analyze_p r)) r;
    !stat

  let encode (r:t) =
    let open J.Encode in
    list
      (Run_event.encode_result Prover.encode)
      (MStr.values r |> Iter.to_rev_list)

  let decode =
    let open J.Decode in
    list (Run_event.decode_result Prover.decode) >|= of_list

  let encode_stat r =
    let open J.Encode in
    let {sat;unsat;errors;unknown;timeout;total_time} = r in
    obj [
      "sat", int sat;
      "unsat", int unsat;
      "unknown", int unknown;
      "errors", int errors;
      "timeout", int timeout;
      "total_time", float total_time
    ]

  let decode_stat =
    let open J.Decode in
    field "sat" int >>= fun sat ->
    field "unsat" int >>= fun unsat ->
    field "unsat" int >>= fun unknown ->
    field "errors" int >>= fun errors ->
    field "timeout" int >>= fun timeout ->
    field "total_time" float >>= fun total_time ->
    succeed {sat;unsat;errors;unknown;timeout;total_time}

end

module Analyze = struct
  type t = {
    raw: Raw.t;
    stat: Raw.stat;
    improved: result list;
    ok: result list;
    disappoint: result list;
    errors: result list;
    bad: result list; (* mismatch *)
  }

  let analyse_ raw =
    let module M = OLinq.AdaptMap(Map.Make(String)) in
    let l =
      M.of_map raw
      |> OLinq.map snd
      |> OLinq.group_by
        (fun r -> Problem.compare_res r.Run_event.problem (Run_event.analyze_p r))
      |> OLinq.run_list ?limit:None
    in
    let improved = assoc_or [] `Improvement l in
    let ok = assoc_or [] `Same l in
    let bad = assoc_or [] `Mismatch l in
    let disappoint = assoc_or [] `Disappoint l in
    let errors = assoc_or [] `Error l in
    (* stats *)
    let stat = Raw.stat raw in
    improved, ok, bad, disappoint, errors, stat

  let make raw =
    let improved, ok, bad, disappoint, errors, stat = analyse_ raw in
    { raw; stat; improved; ok; disappoint; errors; bad; }

  (* build statistics and list of mismatch from raw results *)

  let is_ok r = r.bad = []
  let num_failed r = List.length r.bad

  let to_printbox (r:t) : PrintBox.t =
    let fields = [
      "improved", PB.int @@ List.length r.improved;
      "ok", PB.int @@ List.length r.ok;
      "disappoint", PB.int @@ List.length r.disappoint;
      "errors", PB.int @@ List.length r.errors;
      "bad", PB.int @@ List.length r.bad;

    ] @ Raw.as_printbox_record r.stat in
    printbox_record fields

  let pp_raw_res_ ?(color="reset") out r =
    fpf out "(@[<h>:problem %a@ :expected %a@ :result %a@ :time %.2f@])"
      CCFormat.(with_color color string) r.Run_event.problem.Problem.name
      (CCFormat.with_color color Res.print) r.Run_event.problem.Problem.expected
      (CCFormat.with_color color Res.print) (Run_event.analyze_p r)
      r.Run_event.raw.Run_event.rtime

  let pp_bad out t =
    if t.bad <> [] then (
      Format.fprintf out "(@[<hv1>bad@ %a@])"
        (pp_list_ (pp_raw_res_ ~color:"red")) t.bad
    )

  let pp_summary out t: unit =
    let pp_z_or_err out d =
      if d=0 then CCFormat.int out d
      else CCFormat.(with_color "Red" int) out d
    in
    Format.fprintf out
      "(@[<hv>:ok %d@ :improved %d@ :disappoint %d@ :bad %a@ :errors %a@ :total %d@])%a"
      (List.length t.ok)
      (List.length t.improved)
      (List.length t.disappoint)
      pp_z_or_err (List.length t.bad)
      pp_z_or_err (List.length t.errors)
      (MStr.cardinal t.raw)
      pp_bad t

  let pp out ({ raw=_; stat; improved; ok; disappoint; bad; errors } as r) =
    let pp_l = pp_list_ (pp_raw_res_ ?color:None) in
    let pp_l_red = pp_list_ (pp_raw_res_ ~color:"Red") in
    fpf out
      "(@[<hv2>:summary %a@ :stat %a@ :%-12s %a@ \
       :%-12s %a@ :%-12s %a@ :%-12s %a@ :%-12s %a@])"
      pp_summary  r
      Raw.pp_stat stat
      "ok" pp_l ok
      "improved" pp_l improved
      "disappoint" pp_l disappoint
      "bad" pp_l_red bad
      "errors" pp_l_red errors

  let pp_compact out ({stat; _} as r) =
    fpf out
      "(@[<hv2>:summary %a@ :stat %a@])"
      pp_summary r Raw.pp_stat stat
end

module Config = struct
  type expect =
    | Auto
    | Res of Res.t
    | Program of Prover.t

  type problem_set = {
    directory : string;
    pattern : string;
    expect : expect;
  }

  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    problems : problem_set list;
    provers: Prover.t list;
    default_expect: Res.t option;
  }

  let make ?(j=1) ?(timeout=5) ?(memory=1000) ?(dirs=[]) ?default_expect ~provers () =
    { j; timeout; memory; provers; problems=dirs; default_expect;}

  let update ?j ?timeout ?memory c =
    let j = CCOpt.get_or ~default:c.j j in
    let timeout = CCOpt.get_or ~default:c.timeout timeout in
    let memory = CCOpt.get_or ~default:c.memory memory in
    { c with j; timeout; memory; }
end

module ResultsComparison = struct
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t * float * float) list; (* same result *)
  }

  (* TODO: use outer_join? to also find the disappeared/appeared *)
  let compare (a: Raw.t) b : t =
    let open Run_event in
    let module M = OLinq.AdaptMap(Map.Make(String)) in
    let a = M.of_map a |> OLinq.map snd in
    let b = M.of_map b |> OLinq.map snd in
    let j =
      OLinq.join ~eq:Problem.same_name ~hash:(Problem.hash_name)
        (fun r -> r.problem) (fun r -> r.problem) a b
        ~merge:(fun pb r1 r2 ->
            assert (r1.problem.Problem.name = r2.problem.Problem.name);
            Some (pb, analyze_p r1, analyze_p r2, time_of_res r1, time_of_res r2))
      |> OLinq.group_by (fun (_,res1,res2,_,_) -> Res.compare res1 res2)
      |> OLinq.run_list
    in
    let tup3 = List.map (fun (a,b,c,_,_) -> a,b,c) in
    let improved = assoc_or [] `RightBetter j |> tup3 in
    let regressed = assoc_or [] `LeftBetter j |> tup3 in
    let mismatch = assoc_or [] `Mismatch j |> tup3 in
    let same = assoc_or [] `Same j |> List.rev_map (fun (pb,r,_,t1,t2) -> pb,r,t1,t2) in
    let disappeared =
      OLinq.diff a b
        ~eq:(CCFun.compose_binop Run_event.problem Problem.same_name)
        ~hash:(CCFun.compose Run_event.problem Problem.hash_name)
      |> OLinq.map (fun r -> r.problem, analyze_p r)
      |> OLinq.run_list
    and appeared =
      OLinq.diff b a
        ~eq:(CCFun.compose_binop Run_event.problem Problem.same_name)
        ~hash:(CCFun.compose Run_event.problem Problem.hash_name)
      |> OLinq.map (fun r -> r.problem, analyze_p r)
      |> OLinq.run_list
    in
    { appeared; disappeared; mismatch; same; regressed; improved; }

  let fpf = Format.fprintf
  let pp_pb_res out (pb,res) = fpf out "@[<h>%s: %a@]" pb.Problem.name Res.print res
  let pp_pb_same out (pb,res,t1,t2) =
    fpf out "@[<h>%s: %a (%.2f vs %.2f)@]" pb.Problem.name Res.print res t1 t2
  let pp_pb_res2 ?(color="reset") ~bold out (pb,res1,res2) =
    let module F = CCFormat in
    fpf out "@[<h>%s: %a@]" pb.Problem.name
      ((if bold then F.with_color (CCString.capitalize_ascii color) else F.with_color color)
         (fun out () -> fpf out "%a -> %a" Res.print res1 Res.print res2))
      ()

  let pp out (t:t) =
    fpf out "(@[<v2>comparison@ :appeared: %a@ :disappeared %a@ :same %a@ \
             :mismatch %a@ :improved: %a@ :regressed: %a@])"
      (pp_hvlist_ pp_pb_res) t.appeared
      (pp_hvlist_ pp_pb_res) t.disappeared
      (pp_hvlist_ pp_pb_same) t.same
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:"red")) t.mismatch
      (pp_hvlist_ (pp_pb_res2 ~bold:false ~color:"green")) t.improved
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:"yellow")) t.regressed

  let pp_short out (t:t) =
    fpf out "(@[<v2>comparison@ appeared: %d,@ disappeared: %d@ same: %d \
             @ mismatch(%d): %a@ improved(%d): %a@ regressed(%d): %a@])"
      (List.length t.appeared)
      (List.length t.disappeared)
      (List.length t.same)
      (List.length t.mismatch) (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:"reset")) t.mismatch
      (List.length t.improved) (pp_hvlist_ (pp_pb_res2 ~bold:false ~color:"green")) t.improved
      (List.length t.regressed) (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:"yellow")) t.regressed
end

type top_result = {
  timestamp: float; (* timestamp *)
  events: Run_event.t list;
  total_wall_time: float;
  raw: Raw.t Prover.Map_name.t lazy_t;
  analyze: Analyze.t Prover.Map_name.t lazy_t;
}

module Top_result = struct
  type t = top_result

  let snapshot ?meta t = Run_event.Snapshot.make ?meta t.events

  (* more recent first *)
  let compare_date a b: int = CCFloat.compare b.timestamp a.timestamp

  let make ?total_wall_time ?timestamp l =
    let timestamp = match timestamp with
      | None -> Unix.gettimeofday()
      | Some t -> t
    in
    let total_wall_time = match total_wall_time with
      | Some f -> f
      | None -> Unix.gettimeofday() -. timestamp in
    let raw = lazy (
      l
      |> List.fold_left
        (fun map e -> match e with
           | Run_event.Prover_run r ->
             let p = r.Run_event.program in
             let raw =
               try Prover.Map_name.find p map with Not_found -> Raw.empty
             in
             let analyze_raw = Raw.add r raw in
             Prover.Map_name.add p analyze_raw map
           | Run_event.Checker_run _ -> map)
        Prover.Map_name.empty
    ) in
    let analyze = lazy (
      Prover.Map_name.map Analyze.make (Lazy.force raw)
    ) in
    { timestamp; events=l; raw; analyze; total_wall_time; }

  let filter ~provers ~dir (t:t): t =
    (* predicates on events *)
    let prover_ok r: bool = match provers with
      | None -> true
      | Some l -> List.mem r.Run_event.program.Prover.name l
    and dir_ok r: bool = match dir with
      | [] -> true
      | l ->
        let path = r.Run_event.problem.Problem.name in
        List.exists (fun d -> CCString.mem ~sub:d path) l
    in
    let events =
      CCList.filter
        (function
          | Run_event.Prover_run r -> prover_ok r && dir_ok r
          | Run_event.Checker_run _ -> true)
        t.events
    in
    make ~timestamp:t.timestamp events

  let of_snapshot s =
    make ~timestamp:s.Run_event.timestamp s.Run_event.events

  let merge ?total_wall_time ?timestamp a b =
    make ?total_wall_time ?timestamp (List.rev_append a.events b.events)

  let merge_l ?total_wall_time ?timestamp l =
    let events = List.map (fun t->t.events) l |> List.flatten in
    make ?total_wall_time ?timestamp events

  let pp_header out t =
    Format.fprintf out "(@[(date %a)@])"
      ISO8601.Permissive.pp_datetime t.timestamp

  let pp_compact out (r:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Prover.pp_name p Analyze.pp_compact res
    in
    let {analyze=lazy a; _} = r in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header r (pp_list_ pp_tup) (Prover.Map_name.to_list a)

  let pp_bad out (r:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Prover.pp_name p Analyze.pp_bad res
    in
    let {analyze=lazy a; _} = r in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header r (pp_list_ pp_tup) (Prover.Map_name.to_list a)

  let pp out (r:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Prover.pp_name p Analyze.pp res
    in
    let {analyze=lazy a; _} = r in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header r (pp_list_ pp_tup) (Prover.Map_name.to_list a)

  type comparison_result = {
    both: ResultsComparison.t Prover.Map_name.t;
    left: Analyze.t Prover.Map_name.t;
    right: Analyze.t Prover.Map_name.t;
  }

  (* any common problem? *)
  let should_compare (a:t)(b:t): bool =
    let {raw=lazy a; _} = a in
    let {raw=lazy b; _} = b in
    Prover.Map_name.exists
      (fun p r1 -> match Prover.Map_name.get p b with
         | Some r2 -> MStr.exists (fun k _ -> MStr.mem k r2) r1
         | None -> false)
      a

  let compare (a:t) (b:t): comparison_result =
    let {analyze=lazy a; _} = a in
    let {analyze=lazy b; _} = b in
    let both, left =
      Prover.Map_name.fold
        (fun p r_left (both,left) ->
           try
             (* find same (problem,dir) in [b], and compare *)
             let r_right = Prover.Map_name.find p b in
             let cmp =
               ResultsComparison.compare r_left.Analyze.raw r_right.Analyze.raw
             in
             (p, cmp) :: both, left
           with Not_found ->
             both, (p,r_left)::left)
        a ([],[])
    in
    let right =
      Prover.Map_name.filter
        (fun p _ -> not (Prover.Map_name.mem p a))
        b
    in
    let both = Prover.Map_name.of_list both in
    let left = Prover.Map_name.of_list left in
    { both; left; right; }

  let pp_comparison out (r:comparison_result) =
    let pp_tup out (p,cmp) =
      Format.fprintf out "@[<2>%s:@ @[%a@]@]"
        (Prover.name p) ResultsComparison.pp cmp
    and pp_one which out (p,res) =
      Format.fprintf out "@[<2>%s (only on %s):@ @[%a@]@]"
        (Prover.name p) which Analyze.pp res
    in
    Format.fprintf out "(@[<hv>%a@ %a@ %a@])@."
      (pp_hvlist_ pp_tup) (Prover.Map_name.to_list r.both)
      (pp_hvlist_ (pp_one "left")) (Prover.Map_name.to_list r.left)
      (pp_hvlist_ (pp_one "right")) (Prover.Map_name.to_list r.right)

  module StrSet = CCSet.Make(String)

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  let to_table (t:t): table =
    let lazy map = t.analyze in
    let find_cell p pb : result option =
      try Some (MStr.find pb (Prover.Map_name.find p map).Analyze.raw)
      with Not_found -> None
    in
    let line0 =
      Printf.sprintf "(snapshot :date %s)"
        (ISO8601.Permissive.string_of_datetime t.timestamp)
    in
    let provers = Prover.Map_name.to_list map |> List.map fst in
    let all_problems : StrSet.t =
      Prover.Map_name.fold
        (fun _ analyze acc ->
           MStr.fold (fun pb _ acc -> StrSet.add pb acc) analyze.Analyze.raw acc)
        map
        StrSet.empty
    in
    let t_rows =
      StrSet.fold
        (fun file acc ->
           let tr_res =
             List.map
               (fun prover -> match find_cell prover file with
                  | None ->
                    Prover.name prover, Res.Unknown, 0.
                  | Some res ->
                    let time = time_of_res res in
                    let res = Run_event.analyze_p res in
                    Prover.name prover, res, time)
               provers
           in
           {tr_problem=file; tr_res} :: acc)
        all_problems []
    in
    {t_meta=line0; t_provers=List.map Prover.name provers; t_rows}

  let table_to_csv (t:table): Csv.t =
    let time_to_csv (_:Res.t) f = Printf.sprintf "%.2f" f
    and res_to_csv (r:Res.t) = match r with
      | Res.Error -> "error"
      | Res.Timeout -> "timeout"
      | Res.Unknown -> "unknown"
      | Res.Sat -> "sat"
      | Res.Unsat -> "unsat"
    in
    let header_line =
      "problem" ::
        t.t_provers @
        (List.map (fun p -> p ^ ".time") t.t_provers)
    in
    let lines =
      List.map
        (fun r ->
           r.tr_problem
           :: List.map (fun (_,res,_) ->  res_to_csv res) r.tr_res
           @ List.map (fun (_,res,t) -> time_to_csv res t) r.tr_res)
        t.t_rows
    in
    header_line :: lines

  let to_csv t : Csv.t =
    table_to_csv (to_table t)

  let to_csv_chan oc t =
    let chan = Csv.to_channel oc in
    Csv.output_all chan (to_csv t)

  let to_csv_file file t =
    let oc = open_out file in
    to_csv_chan oc t;
    close_out oc

  let to_csv_string t =
    let buf = Buffer.create 256 in
    let ch = Csv.to_buffer buf in
    Csv.output_all ch (to_csv t);
    Buffer.contents buf

  let encode (self:t) =
    let open J.Encode in
    let {timestamp; events; total_wall_time; raw=_; analyze=_} = self in
    obj [
      "timestamp", float timestamp;
      "total_wall_time", float total_wall_time;
      "events", list Run_event.encode events;
    ]

  let decode : t J.Decode.t =
    let open J.Decode in
    field "timestamp" float >>= fun timestamp ->
    field_opt "total_wall_time" float >>= fun total_wall_time ->
    let total_wall_time = CCOpt.get_or ~default:0. total_wall_time in
    field "events" (list Run_event.decode) >>= fun events ->
    succeed (make ~timestamp ~total_wall_time events)
end

(** {2 Benchmark, within one Top Result} *)
module Bench = struct
  type per_prover = {
    stat: Raw.stat;
    sat: (string * float) list;
    unsat: (string * float) list;
  }

  type t = {
    from: top_result;
    per_prover: per_prover Prover.Map_name.t;
  }

  let make (r:top_result): t =
    let per_prover =
      Prover.Map_name.map
        (fun raw ->
           let stat = Raw.stat raw in
           let sat =
             MStr.fold
               (fun file res acc -> match Run_event.analyze_p res with
                  | Res.Sat -> (file, time_of_res res) :: acc
                  | _ -> acc)
               raw []
           and unsat =
             MStr.fold
               (fun file res acc -> match Run_event.analyze_p res with
                  | Res.Unsat -> (file, time_of_res res) :: acc
                  | _ -> acc)
               raw []
           in
           {stat; sat; unsat})
        (Lazy.force r.raw)
    in
    {from=r; per_prover}

  let pp out (r:t): unit =
    let pp_stat out (p,per_prover) =
      Format.fprintf out "@[<h2>%a:@ %a@]"
        Prover.pp_name p Raw.pp_stat per_prover.stat
    and pp_full _out (_p,_res) =
      () (* TODO *)
    in
    let l = Prover.Map_name.to_list r.per_prover in
    Format.fprintf out "(@[<v2>bench@ @[%a@]@ @[<v>%a@]@])"
      (pp_hvlist_ pp_stat) l
      (pp_hvlist_ pp_full) l
end
