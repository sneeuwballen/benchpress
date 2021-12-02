
open Common
open Test

module PB = PrintBox

type t = {
  unsat: int;
  sat: int;
  errors: int;
  unknown: int;
  timeout: int;
  memory: int;
  custom: (string * int) list;
  total: int;
  total_time: float; (* for sat+unsat *)
}

let get_unsat r = r.unsat
let get_sat r = r.sat
let get_errors r = r.errors
let get_unknown r = r.unknown
let get_timeout r = r.timeout
let get_memory r = r.memory
let get_custom s r = try List.assoc s r.custom with Not_found -> 0
let get_total r = r.total
let get_total_time r = r.total_time

let to_printbox_l ?to_link l : PB.t =
  let mk_row ?res lbl get_r mk_box : PB.t list =
    match to_link with
    | Some f ->
      let uri p =
        let res = CCOpt.get_or ~default:lbl res in
        f p res
      in
      PB.text lbl ::
      List.map (fun (p,r) ->
          let n = get_r r in
          if n > 0 then (
            PB.link ~uri:(uri p) (mk_box n)
          ) else (
            mk_box n
          ))
        l
    | _ ->
      PB.text lbl :: List.map (fun (_p,r) -> mk_box (get_r r)) l
  and mk_row1 lbl get_r mk_box =
    PB.text lbl :: List.map (fun (_,r) -> mk_box @@ get_r r) l;
  in
  let r1 =
    [mk_row "sat" get_sat @@ pb_int_color PB.Style.(fg_color Green);
     mk_row "unsat" get_unsat @@ pb_int_color PB.Style.(fg_color Green);
     mk_row1 "sat+unsat" (fun r->r.sat+r.unsat) @@
     pb_int_color PB.Style.(fg_color Green);
     mk_row ~res:"error" "errors" get_errors @@ pb_int_color PB.Style.(fg_color Cyan);
     mk_row "unknown" get_unknown PB.int;
     mk_row "timeout" get_timeout PB.int;
    ]
  and r2 =
    let all_custom =
      CCList.flat_map (fun (_,s) -> List.map fst s.custom) l
      |> CCList.sort_uniq ~cmp:String.compare
    in
    CCList.map
      (fun tag -> mk_row ~res:tag ("tag." ^ tag) (get_custom tag) PB.int)
      all_custom
  and r3 =
    [mk_row1 "memory" get_memory PB.int;
     mk_row1 "total" get_total PB.int;
     mk_row1 "total_time" get_total_time
       (fun s -> PB.line (Misc.human_duration s));
    ]
  in
  let header = List.map PB.text @@ "provers" :: List.map fst l in
  let rows = r1 @ r2 @ r3 in
  PB.grid_l ~bars:true (header::rows)

(* obtain stats for this prover *)
let of_db_for ~(prover:Prover.name) (db:Db.t) : t =
  Error.guard (Error.wrapf "reading stat(%s) from DB" prover) @@ fun () ->
  let f c = Db.Cursor.next c |> Error.unwrap_opt "no result" in
  let custom = Prover.tags_of_db db in
  let get_res r =
    Logs.debug (fun k->k "get-res %s" r);
    Db.exec db
      {| select count(*) from prover_res where prover=? and res=?; |}
      prover r ~ty:Db.Ty.(p2 text text, p1 (nullable int), CCOpt.get_or ~default:0) ~f
    |> Misc.unwrap_db (fun() -> spf "problems with result %s" r)
  in
  let sat = get_res "sat" in
  let unsat = get_res "unsat" in
  let unknown = get_res "unknown" in
  let timeout = get_res "timeout" in
  let memory = get_res "memory" in
  let errors = get_res "error" in
  let custom = CCList.map (fun tag -> tag, get_res tag) custom in
  let total =
    sat + unsat + unknown + timeout + memory + errors
    + List.fold_left (fun n (_,i) -> n+i) 0 custom in
  let total_time =
    Db.exec db {|
        select sum(rtime) from prover_res where prover=? and res in ('sat', 'unsat');
          |} prover
      ~ty:Db.Ty.(p1 text, p1 (nullable float), CCOpt.get_or ~default:0.) ~f
    |> Misc.unwrap_db (fun() -> spf "obtaining total time for %s" prover)
  in
  { sat; unsat; timeout; memory; unknown; errors; custom; total; total_time; }

let of_db (db:Db.t) : (Prover.name * t) list =
  Profile.with_ "stat.of-db" @@ fun () ->
  Error.guard (Error.wrap "reading statistics from DB") @@ fun () ->
  let provers = list_provers db in
  CCList.map
    (fun p -> p, of_db_for db ~prover:p)
    provers

let pp out (s:t) : unit =
  Fmt.fprintf out
    "(@[<hv>:unsat %d@ :sat %d@ :solved %d@ :errors %d@ :unknown %d@ \
     :timeout %d@ :total %d@ :total_time %.2f@])"
    s.unsat s.sat (s.sat + s.unsat) s.errors s.unknown s.timeout
    (s.unsat + s.sat + s.errors + s.unknown + s.timeout)
    s.total_time
