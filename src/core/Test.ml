(* This file is free software. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

module E = CCResult
module MStr = Misc.Str_map
module J = Misc.Json
module PB = PrintBox
module Db = Sqlite3_utils
module Fmt = CCFormat

type result = Prover.name Run_result.t

type 'a or_error = ('a, string) CCResult.t

let fpf = Format.fprintf

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

let pp_list_ p =
  Fmt.within "(" ")"
    (Fmt.hovbox
       (Fmt.(list ~sep:(return "@ ") p)))

let pp_hvlist_ p =
  Fmt.within "(" ")"
    (Fmt.hvbox
       (Fmt.(list ~sep:(return "@ ") p)))

let time_of_res e = e.Run_result.raw.rtime

let pb_v_record ?bars l =
  PB.grid_l ?bars
    (List.map (fun (field,value) -> [PB.text field; value]) l)

let pb_int_color c n =
  let open PB in
  if n=0 then int n
  else text_with_style (Style.set_bold true c) (string_of_int n)

module Stat = struct
  type t = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
    timeout: int;
    total_time: float; (* for sat+unsat *)
  }

  let empty : t =
    {unsat=0; sat=0; errors=0; unknown=0; timeout=0; total_time=0.; }

  let as_printbox_record s : _ list =
    let open PB in
    [ "sat", int s.sat; "unsat", int s.unsat; "errors", int s.errors;
      "unknown", int s.unknown; "timeout", int s.timeout;
      "total_time", line (Misc.human_time s.total_time) ]

  let printbox (s:t) : PrintBox.t =
    pb_v_record @@ as_printbox_record s

  let add_sat_ t s = {s with sat=s.sat+1; total_time=s.total_time+. t; }
  let add_unsat_ t s = {s with unsat=s.unsat+1; total_time=s.total_time+. t; }
  let add_unknown_ s = {s with unknown=s.unknown+1}
  let add_error_ s = {s with errors=s.errors+1}
  let add_timeout_ s = {s with timeout=s.timeout+1}

  let pp out (s:t) : unit =
    fpf out
      "(@[<hv>:unsat %d@ :sat %d@ :solved %d@ :errors %d@ :unknown %d@ \
       :timeout %d@ :total %d@ :total_time %.2f@])"
      s.unsat s.sat (s.sat + s.unsat) s.errors s.unknown s.timeout
      (s.unsat + s.sat + s.errors + s.unknown + s.timeout)
      s.total_time
end

module Raw : sig
  type t = result MStr.t

  val empty: t

  val add : result -> t -> t

  val merge : t -> t -> t

  val stat : t -> Stat.t
end = struct
  type t = result MStr.t
  let empty = MStr.empty

  let add (r:result) raw =
    let pb = r.Run_result.problem.Problem.name in
    MStr.add pb r raw

  let merge =
    MStr.merge
      (fun _ a b -> if a=None then b else a)

  let of_list l = List.fold_left (fun acc r -> add r acc) empty l

  let stat r =
    (* stats *)
    let stat = ref Stat.empty in
    let add_res time res =
      stat := Stat.(match res with
          | Res.Unsat -> add_unsat_ time | Res.Sat -> add_sat_ time
          | Res.Unknown -> add_unknown_ | Res.Error -> add_error_
          | Res.Timeout -> add_timeout_
        ) !stat
    in
    MStr.iter (fun _ r -> add_res (time_of_res r) r.res) r;
    !stat
end

module Analyze : sig
  type t = {
    raw: Raw.t;
    stat: Stat.t;
    improved  : result list;
    ok        : result list;
    disappoint: result list;
    errors    : result list;
    bad       : result list; (* mismatch *)
  }

  val make : Raw.t -> t

  val to_printbox : t -> PrintBox.t
  val to_printbox_bad : t -> PrintBox.t

  val is_ok : t -> bool

  val num_failed : t -> int

  val pp : t Fmt.printer
  val pp_compact : t Fmt.printer
  val pp_bad : t Fmt.printer
end = struct
  type t = {
    raw: Raw.t;
    stat: Stat.t;
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
        (fun r -> Problem.compare_res r.Run_result.problem r.res)
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
    let open PB in
    let fields = [
      "improved", pb_int_color Style.(fg_color Green) @@ List.length r.improved;
      "ok", pb_int_color Style.(fg_color Green) @@ List.length r.ok;
      "disappoint", pb_int_color Style.(fg_color Blue) @@ List.length r.disappoint;
      "errors", pb_int_color Style.(fg_color Cyan) @@ List.length r.errors;
      "bad", pb_int_color Style.(fg_color Red) @@ List.length r.bad;
    ] @ Stat.as_printbox_record r.stat in
    pb_v_record ~bars:true fields

  let to_printbox_bad r : PrintBox.t =
    let open PB in
    if r.bad <> [] then (
      let l =
        List.map
          (fun r -> 
             [ text (Problem.name r.Run_result.problem);
               text (Res.to_string r.res);
               text (Res.to_string r.problem.Problem.expected);
             ])
          r.bad
      in
      let header =
        let tb = text_with_style Style.bold in
        [tb "problem"; tb "res"; tb "expected"] in
      grid_l (header :: l)
    ) else empty

  let pp_raw_res_ ?(color="reset") out r =
    fpf out "(@[<h>:problem %a@ :expected %a@ :result %a@ :time %.2f@])"
      Fmt.(with_color color string) r.Run_result.problem.Problem.name
      (Fmt.with_color color Res.pp) r.Run_result.problem.Problem.expected
      (Fmt.with_color color Res.pp) r.res
      r.Run_result.raw.rtime

  let pp_bad out t =
    if t.bad <> [] then (
      Format.fprintf out "(@[<hv1>bad@ %a@])"
        (pp_list_ (pp_raw_res_ ~color:"red")) t.bad
    )

  let pp_summary out t: unit =
    let pp_z_or_err out d =
      if d=0 then Fmt.int out d
      else Fmt.(with_color "Red" int) out d
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
      Stat.pp stat
      "ok" pp_l ok
      "improved" pp_l improved
      "disappoint" pp_l disappoint
      "bad" pp_l_red bad
      "errors" pp_l_red errors

  let pp_compact out ({stat; _} as r) =
    fpf out
      "(@[<hv2>:summary %a@ :stat %a@])"
      pp_summary r Stat.pp stat
end

module ResultsComparison : sig
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t * float * float) list; (* same result *)
  }

  val compare : Raw.t -> Raw.t -> t

  val pp : t Fmt.printer
  (** Display comparison in a readable way *)

  val pp_short : t Fmt.printer
  (** Display comparison in a compact way *)

  val to_printbox : t -> PrintBox.t
  val to_printbox_short : t -> PrintBox.t
end = struct
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
    let open Run_result in
    let module M = OLinq.AdaptMap(Map.Make(String)) in
    let a = M.of_map a |> OLinq.map snd in
    let b = M.of_map b |> OLinq.map snd in
    let j =
      OLinq.join ~eq:Problem.same_name ~hash:(Problem.hash_name)
        (fun r -> r.problem) (fun r -> r.problem) a b
        ~merge:(fun pb r1 r2 ->
            assert (r1.problem.Problem.name = r2.problem.Problem.name);
            Some (pb, r1.res, r2.res, time_of_res r1, time_of_res r2))
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
        ~eq:(CCFun.compose_binop Run_result.problem Problem.same_name)
        ~hash:(CCFun.compose Run_result.problem Problem.hash_name)
      |> OLinq.map (fun r -> r.problem, r.res)
      |> OLinq.run_list
    and appeared =
      OLinq.diff b a
        ~eq:(CCFun.compose_binop Run_result.problem Problem.same_name)
        ~hash:(CCFun.compose Run_result.problem Problem.hash_name)
      |> OLinq.map (fun r -> r.problem, r.res)
      |> OLinq.run_list
    in
    { appeared; disappeared; mismatch; same; regressed; improved; }

  let fpf = Format.fprintf
  let pp_pb_res out (pb,res) = fpf out "@[<h>%s: %a@]" pb.Problem.name Res.pp res
  let pp_pb_same out (pb,res,t1,t2) =
    fpf out "@[<h>%s: %a (%.2f vs %.2f)@]" pb.Problem.name Res.pp res t1 t2
  let pp_pb_res2 ?(color="reset") ~bold out (pb,res1,res2) =
    let module F = Fmt in
    fpf out "@[<h>%s: %a@]" pb.Problem.name
      ((if bold then F.with_color (CCString.capitalize_ascii color) else F.with_color color)
         (fun out () -> fpf out "%a -> %a" Res.pp res1 Res.pp res2))
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

  let to_pb_res1 (pb,res) =
    let open PB in
    [ text (Problem.name pb); text (Res.to_string res); ]

  let to_pb_res2 (pb,res1,res2) =
    let open PB in
    [ text (Problem.name pb);
      text (Res.to_string res1);
      text (Res.to_string res2)]

  let to_printbox (self:t) : PB.t =
    let open PB in
    pb_v_record [
      "mismatch", grid_l @@ List.map to_pb_res2 self.mismatch;
      "improved", grid_l @@ List.map to_pb_res2 self.improved;
      "regressed", grid_l @@ List.map to_pb_res2 self.regressed;
      "appeared", grid_l @@ List.map to_pb_res1 self.appeared;
      "disappeared", grid_l @@ List.map to_pb_res1 self.disappeared;
      "same", grid_l @@ List.map (fun (pb,res,_,_) -> to_pb_res1 (pb,res)) self.same;
    ]

  let to_printbox_short (self:t) : PB.t =
    let open PB in
    pb_v_record [
      "appeared", int (List.length self.appeared);
      "disappeared", int (List.length self.disappeared);
      "same", int (List.length self.same);
      "mismatch", pb_int_color Style.(fg_color Red) (List.length self.mismatch);
      "improved", pb_int_color Style.(fg_color Green) (List.length self.improved);
      "regressed", pb_int_color Style.(fg_color Cyan) (List.length self.regressed);
      "mismatch-list", grid_l @@ List.map to_pb_res2 self.mismatch;
      "improved-list", grid_l @@ List.map to_pb_res2 self.improved;
      "regressed-list", grid_l @@ List.map to_pb_res2 self.regressed;
    ]
end


(** {2 Top Result}

    Main result of testing: a snapshot of the work done, + the analysis
    per prover *)

type top_result = {
  timestamp: float; (* timestamp *)
  events: Run_event.t list;
  total_wall_time: float;
  raw: Raw.t MStr.t lazy_t;
  analyze: Analyze.t MStr.t lazy_t;
}

module Top_result : sig 
  type t = top_result

  val pp : t Fmt.printer
  (** Full printer, including results *)

  val pp_header : t Fmt.printer
  (** Print only meta-information: UUID and timestamp *)

  val pp_compact : t Fmt.printer
  (** Print meta-informations + compact results *)

  val pp_bad : t Fmt.printer

  (* FIXME:
     request a Uuid as a unique name (along with timestamp), provided
     from main. *)

  val make :
    ?total_wall_time:float -> ?timestamp:float ->
    Prover.name Run_result.t list ->
    t

  type comparison_result = {
    both: ResultsComparison.t MStr.t;
    left: Analyze.t MStr.t;
    right: Analyze.t MStr.t;
  }

  val compare : t -> t -> comparison_result

  val pp_comparison : comparison_result Fmt.printer
  val comparison_to_printbox : ?short:bool -> comparison_result -> PrintBox.t

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  val to_table : t -> table

  val table_to_csv : table -> Csv.t

  val table_to_printbox : table -> PrintBox.t

  val to_printbox_summary : t -> (string * PrintBox.t) list
  val to_printbox_table : t -> PrintBox.t
  val to_printbox_bad : t -> (string * PrintBox.t) list

  val to_csv : t -> Csv.t

  val to_csv_chan : out_channel -> t -> unit

  val to_csv_string : t -> string

  val to_csv_file : string -> t -> unit
  (** Write as CSV into given file *)

  val decode : t J.Decode.t

  val to_db : Db.t -> t -> unit or_error

  val of_db : Db.t -> t or_error
end = struct
  type t = top_result

  let make ?total_wall_time ?timestamp (l:Prover.name Run_result.t list) : t =
    let timestamp = match timestamp with
      | None -> Unix.gettimeofday()
      | Some t -> t
    in
    let total_wall_time = match total_wall_time with
      | Some f -> f
      | None -> Unix.gettimeofday() -. timestamp in
    let raw = lazy (
      List.fold_left
        (fun map (r:Prover.name Run_result.t) ->
             let p = r.program in
             let raw =
               try MStr.find p map with Not_found -> Raw.empty
             in
             let analyze_raw = Raw.add r raw in
             MStr.add p analyze_raw map)
        MStr.empty l
    ) in
    let analyze = lazy (
      MStr.map Analyze.make (Lazy.force raw)
    ) in
    let l = List.rev_map Run_event.mk_prover l in
    { timestamp; events=l; raw; analyze; total_wall_time; }

  let pp_header out t =
    Format.fprintf out "(@[(date %a)@])"
      ISO8601.Permissive.pp_datetime t.timestamp

  let pp_compact out (r:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp_compact res
    in
    let {analyze=lazy a; _} = r in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header r (pp_list_ pp_tup) (MStr.to_list a)

  let pp_bad out (r:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp_bad res
    in
    let {analyze=lazy a; _} = r in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header r (pp_list_ pp_tup) (MStr.to_list a)

  let pp out (r:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp res
    in
    let {analyze=lazy a; _} = r in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header r (pp_list_ pp_tup) (MStr.to_list a)

  type comparison_result = {
    both: ResultsComparison.t MStr.t;
    left: Analyze.t MStr.t;
    right: Analyze.t MStr.t;
  }

  (* TODO: use that in web UI *)
  (* any common problem? *)
  let should_compare (a:t)(b:t): bool =
    let {raw=lazy a; _} = a in
    let {raw=lazy b; _} = b in
    MStr.exists
      (fun p r1 -> match MStr.get p b with
         | Some r2 -> MStr.exists (fun k _ -> MStr.mem k r2) r1
         | None -> false)
      a

  let compare (a:t) (b:t): comparison_result =
    let {analyze=lazy a; _} = a in
    let {analyze=lazy b; _} = b in
    let both, left =
      MStr.fold
        (fun p r_left (both,left) ->
           try
             (* find same (problem,dir) in [b], and compare *)
             let r_right = MStr.find p b in
             let cmp =
               ResultsComparison.compare r_left.Analyze.raw r_right.Analyze.raw
             in
             (p, cmp) :: both, left
           with Not_found ->
             both, (p,r_left)::left)
        a ([],[])
    in
    let right =
      MStr.filter
        (fun p _ -> not (MStr.mem p a))
        b
    in
    let both = MStr.of_list both in
    let left = MStr.of_list left in
    { both; left; right; }

  let pp_comparison out (r:comparison_result) =
    let pp_tup out (p,cmp) =
      Format.fprintf out "@[<2>%s:@ @[%a@]@]"
        p ResultsComparison.pp cmp
    and pp_one which out (p,res) =
      Format.fprintf out "@[<2>%s (only on %s):@ @[%a@]@]"
        p which Analyze.pp res
    in
    Format.fprintf out "(@[<hv>%a@ %a@ %a@])@."
      (pp_hvlist_ pp_tup) (MStr.to_list r.both)
      (pp_hvlist_ (pp_one "left")) (MStr.to_list r.left)
      (pp_hvlist_ (pp_one "right")) (MStr.to_list r.right)

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
      try Some (MStr.find pb (MStr.find p map).Analyze.raw)
      with Not_found -> None
    in
    let line0 =
      Printf.sprintf "(snapshot :date %s)"
        (ISO8601.Permissive.string_of_datetime t.timestamp)
    in
    let provers = MStr.to_list map |> List.map fst in
    let all_problems : StrSet.t =
      MStr.fold
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
                  | None -> prover, Res.Unknown, 0.
                  | Some res ->
                    let time = time_of_res res in
                    prover, res.res, time)
               provers
           in
           {tr_problem=file; tr_res} :: acc)
        all_problems []
    in
    {t_meta=line0; t_provers=provers; t_rows}

  let time_to_csv (_:Res.t) f = Printf.sprintf "%.2f" f
  let res_to_csv (r:Res.t) = match r with
    | Res.Error -> "error"
    | Res.Timeout -> "timeout"
    | Res.Unknown -> "unknown"
    | Res.Sat -> "sat"
    | Res.Unsat -> "unsat"

  let table_to_csv (t:table): Csv.t =
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

  let table_to_printbox (self:table) : PB.t =
    let header_line =
      List.map PB.(text_with_style Style.bold) @@
      "problem" ::
        self.t_provers @
        (List.map (fun p -> p ^ ".time") self.t_provers)
    in
    let lines =
      List.map
        (fun r ->
           PB.text r.tr_problem
           :: List.map (fun (_,res,_) -> PB.text @@ res_to_csv res) r.tr_res
           @ List.map (fun (_,res,t) -> PB.text @@ time_to_csv res t) r.tr_res)
        self.t_rows
    in
    PB.grid_l (header_line::lines)

  let to_printbox_summary (self:t) : (_ * PB.t) list =
    let {analyze=lazy a; _} = self in
    MStr.to_list a
    |> List.map (fun (p, a) -> p, Analyze.to_printbox a)

  let to_printbox_table self = table_to_printbox @@ to_table self
  let to_printbox_bad self =
    let {analyze=lazy a; _} = self in
    MStr.to_list a
    |> CCList.filter_map
      (fun (p, a) ->
         if a.Analyze.bad =[] then None
         else Some (p, Analyze.to_printbox_bad a))

  let comparison_to_printbox ?(short=true) (self:comparison_result) : PB.t =
    let open PB in
    let pm side m =
      MStr.to_list m
      |> List.map
        (fun (p,c) ->
           [text_with_style Style.bold (p ^ " ("^side^")");
            Analyze.to_printbox c])
    in
    grid_l @@ List.flatten [
      [
        MStr.to_list self.both
        |> List.map
          (fun (p,c) ->
             hlist [text_with_style Style.bold (p ^" (both)");
                    if short
                    then ResultsComparison.to_printbox_short c
                    else ResultsComparison.to_printbox c;
                   ])
      ];
      pm "left" self.left;
      pm "right" self.right;
    ]

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

  let decode_events =
    let open J.Decode in
    list Run_event.decode
    >|= CCList.filter_map (function Run_event.Prover_run r -> Some r | _ -> None)

  let decode : t J.Decode.t =
    let open J.Decode in
    field "timestamp" float >>= fun timestamp ->
    field_opt "total_wall_time" float >>= fun total_wall_time ->
    let total_wall_time = CCOpt.get_or ~default:0. total_wall_time in
    field "events" decode_events >>= fun events ->
    succeed (make ~timestamp ~total_wall_time events)

  let db_prepare (db:Db.t) : _ or_error =
    Db.exec0 db {|
        create table if not exists
        meta(
          key text not null unique,
          value blob
          );
        create index if not exists meta_k on meta(key);
        |} |> Misc.db_err ~ctx:"top-res.db-prepare"

  let add_meta (db:Db.t) (self:t) : unit or_error =
    Db.exec_no_cursor db
      "insert into meta values
      ('timestamp', ?), ('total-wall-time', ?);"
      ~ty:Db.Ty.(p2 blob blob)
      (string_of_float self.timestamp)
      (string_of_float self.total_wall_time)
    |> Misc.db_err ~ctx:"inserting metadata"

  let to_db (db:Db.t) (self:t) : unit or_error =
    Logs.info (fun k->k "dump top-result into DB");
    Misc.err_with (fun scope ->
        scope.unwrap @@ db_prepare db;
        scope.unwrap @@ Run_event.db_prepare db;
        (* insert within one transaction, much faster *)
        scope.unwrap @@ add_meta db self;
        Db.transact db (fun _ ->
            List.iter (fun ev -> scope.unwrap @@ Run_event.to_db db ev) self.events);
        ())

  let get_meta db k : _ or_error =
    Db.exec db {|select value from meta where key=? ;|}
      k
      ~ty:Db.Ty.(p1 text, p1 blob,fun x->x) ~f:Db.Cursor.next
      |> Misc.db_err ~ctx:"test.get-meta"
      |> E.flat_map (CCOpt.to_result ("did not found metadata " ^ k))

  let of_db (db:Db.t) : t or_error =
    let open E.Infix in
    try
      get_meta db "timestamp" >>= fun timestamp ->
      get_meta db "total-wall-time" >>= fun total_wall_time ->
      let timestamp = float_of_string timestamp in
      let total_wall_time = float_of_string total_wall_time in
      Run_event.of_db_l db >>= fun events ->
      Ok (make ~total_wall_time ~timestamp events)
    with Db.Type_error d ->
      Error ("type error: unexpected " ^ Db.Data.to_string_debug d)
end

(** {2 Benchmark, within one Top Result} *)

module Bench : sig
  type per_prover = {
    stat: Stat.t;
    sat: (string * float) list;
    unsat: (string * float) list;
  }

  type t = {
    from: top_result;
    per_prover: per_prover MStr.t;
  }

  val make : top_result -> t

  val pp : t Fmt.printer
  (** Full printer that compares provers against one another *)
end = struct
  type per_prover = {
    stat: Stat.t;
    sat: (string * float) list;
    unsat: (string * float) list;
  }

  type t = {
    from: top_result;
    per_prover: per_prover MStr.t;
  }

  let make (r:top_result): t =
    let per_prover =
      MStr.map
        (fun raw ->
           let stat = Raw.stat raw in
           let sat =
             MStr.fold
               (fun file res acc -> match res.Run_result.res with
                  | Res.Sat -> (file, time_of_res res) :: acc
                  | _ -> acc)
               raw []
           and unsat =
             MStr.fold
               (fun file res acc -> match res.Run_result.res with
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
      Format.fprintf out "@[<h2>%s:@ %a@]"
        p Stat.pp per_prover.stat
    and pp_full _out (_p,_res) =
      () (* TODO *)
    in
    let l = MStr.to_list r.per_prover in
    Format.fprintf out "(@[<v2>bench@ @[%a@]@ @[<v>%a@]@])"
      (pp_hvlist_ pp_stat) l
      (pp_hvlist_ pp_full) l
end
