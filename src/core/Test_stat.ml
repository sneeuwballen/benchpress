open Common
open Test
module PB = PrintBox

(** Aggregate function computing the mean and the standard deviation
   of data series using the Welford's algorithm. *)
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

(* obtain stats for this prover *)
let of_db_for ~(prover : Prover.name) (db : Db.t) : t =
  Error.guard (Error.wrapf "reading stat(%s) from DB" prover) @@ fun () ->
  let custom = Prover.tags_of_db db in
  let get_res r =
    Error.guard (Error.wrapf "get-res %S" r) @@ fun () ->
    let count : int option =
      Db.exec db {| select count(*) from prover_res where prover=? and res=?; |}
        prover r
        ~ty:Db.Ty.(p2 text text, p1 (nullable int), Fun.id)
        ~f:Db.Cursor.get_one_exn
      |> Misc.unwrap_db (fun () -> spf "problems with result %s" r)
    in

    let stats =
      let stat = ref Stats.init in
      Db.exec db {| select rtime from prover_res where prover=? and res=?; |}
        prover r
        ~ty:Db.Ty.(p2 text text, p1 float, Fun.id)
        ~f:(fun cursor ->
          Db.Cursor.iter cursor ~f:(fun rtime -> stat := Stats.step !stat rtime))
      |> Misc.unwrap_db (fun () -> spf "problems with result %s" r);
      !stat
    in

    let total = Stats.total stats in
    let mean = Stats.mean stats in
    let sd = Stats.sd stats in
    { n = CCOpt.get_or ~default:0 count; total; mean; sd }
  in

  let get_proof_res r =
    Error.guard (Error.wrapf "get-proof-res %S %S" prover r) @@ fun () ->
    try
      Db.exec db
        {| select count( * ) from proof_check_res where prover=? and res=?; |}
        prover r
        ~ty:Db.Ty.([ text; text ], [ int ], fun i -> i)
        ~f:Db.Cursor.get_one_exn
      |> Misc.unwrap_db (fun () -> spf "problems with result %s" r)
    with
    | Sqlite3.SqliteError msg | Sqlite3.Error msg ->
      Log.err (fun k ->
          k "cannot get proof res %S %S:@ sqlite error:@ %s" prover r msg);
      0
    | Error.E e ->
      Log.err (fun k -> k "cannot get proof res %S %S:@ %a" prover r Error.pp e);
      0
  in
  let sat = get_res "sat" in
  let unsat = get_res "unsat" in
  let unknown = get_res "unknown" in
  let timeout = (get_res "timeout").n in
  let memory = (get_res "memory").n in
  let errors = (get_res "error").n in
  let valid_proof = get_proof_res "valid" in
  let invalid_proof = get_proof_res "invalid" in
  let custom = CCList.map (fun tag -> tag, get_res tag) custom in
  let total =
    sat.n + unsat.n + unknown.n + timeout + memory + errors
    + List.fold_left (fun acc (_, { n; _ }) -> acc + n) 0 custom
  in
  let total_time =
    Db.exec db
      {|
        select sum(rtime) from prover_res where prover=? and res in ('sat', 'unsat');
          |}
      prover
      ~ty:Db.Ty.(p1 text, p1 (nullable float), CCOpt.get_or ~default:0.)
      ~f:Db.Cursor.get_one_exn
    |> Misc.unwrap_db (fun () -> spf "obtaining total time for %s" prover)
  in
  {
    sat;
    unsat;
    timeout;
    memory;
    unknown;
    valid_proof;
    invalid_proof;
    errors;
    custom;
    total;
    total_time;
  }

let of_db (db : Db.t) : (Prover.name * t) list =
  Profile.with_ "stat.of-db" @@ fun () ->
  Error.guard (Error.wrap "reading statistics from DB") @@ fun () ->
  let provers = list_provers db in
  CCList.map (fun p -> p, of_db_for db ~prover:p) provers

let pp out (s : t) : unit =
  Fmt.fprintf out
    "(@[<hv>:unsat %d@ :sat %d@ :solved %d@ :errors %d@ :unknown %d@ \
     :valid-proof %d@ :invalid-proof %d@ :timeout %d@ :total %d@ :total_time \
     %.2f@])"
    s.unsat.n s.sat.n (s.sat.n + s.unsat.n) s.errors s.unknown.n s.valid_proof
    s.invalid_proof s.timeout
    (s.unsat.n + s.sat.n + s.errors + s.unknown.n + s.timeout)
    s.total_time
