open Common
open Test

module Summarize_dirs : sig
  type path = string list
  type path_opt = path option

  val init : path_opt
  val string_of_path : path -> string
  val merge_path1 : path_opt -> Db.Data.t -> path_opt

  (*   val setup_fun : Db.t -> unit *)
end = struct
  type path = string list
  type path_opt = path option

  (* split path into components *)
  let split_path_ s : path =
    let rec aux acc s =
      let dir = Filename.dirname s in
      if s = "." then
        acc
      else if s <> dir then
        aux (Filename.basename s :: acc) dir
      else
        s :: acc
    in
    aux [] s

  let rec merge_paths (p1 : path) (p2 : path) : path =
    match p1, p2 with
    | [], _ | _, [] -> []
    | d1 :: tl1, d2 :: tl2 ->
      if d1 = d2 then
        d1 :: merge_paths tl1 tl2
      else
        []

  let nopath_ = None
  let init = nopath_

  let merge_path1 (data : path_opt) (d2 : Db.Data.t) : path_opt =
    let get_str = function
      | Db.Data.TEXT s | Db.Data.BLOB s -> s
      | _ -> raise Exit
    in
    try
      let p2 = split_path_ @@ get_str d2 in
      match data with
      | None -> Some p2
      | Some p1 -> Some (merge_paths p1 p2)
    with Exit -> nopath_

  let string_of_path = String.concat "/"

  (*
  let finalize path : Db.Data.t =
    match path with
    | None -> Db.Data.NULL
    | Some path ->
      let path = string_of_path path in
      Db.Data.TEXT path
     *)

  (* FIXME: https://github.com/mmottl/sqlite3-ocaml/issues/47
     let setup_fun db : unit =
       Sqlite3.Aggregate.create_fun1 db "mergepaths"
         ~init ~final:finalize ~step:merge_path1;
  *)
end

type t = {
  improved: int;
  ok: int;
  disappoint: int;
  bad: int; (* mismatch *)
  bad_full: (Problem.t * Res.t * float) list; (* always materialized *)
  valid_proof: int;
  invalid_proof: int;
  invalid_proof_full: (Problem.t * Proof_check_res.t * float) list;
  errors: int;
  errors_full: (Problem.t * Res.t * float) list;
  total: int;
}

let int1_cursor ~ctx c =
  Db.Cursor.next c
  |> Error.unwrap_opt' (fun () -> spf "expected a result in %s" ctx)

let of_db_n_bad (db : Db.t) : int =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "test.analyze.n-bad" in
  Error.guard (Error.wrap "computing n-bad from DB") @@ fun () ->
  Db.exec db
    ~f:(int1_cursor ~ctx:"extracting n-bad")
    ~ty:Db.Ty.(nil, p1 int, id)
    {| select count(*) from prover_res
              where res in ('sat','unsat')
              and res != file_expect
              and file_expect in ('sat','unsat'); |}
  |> Misc.unwrap_db (fun () -> "counting bad results")

let of_db_dirs (db : Db.t) : string list =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "test.analyze.dirs" in
  Error.guard (Error.wrap "computing dirs from DB") @@ fun () ->
  let r = ref Summarize_dirs.init in
  let () =
    Db.exec db
      ~ty:Db.Ty.(nil, p1 data, id)
      {| select distinct file from prover_res; |}
      ~f:(fun c ->
        Db.Cursor.iter c ~f:(fun d -> r := Summarize_dirs.merge_path1 !r d))
    |> Misc.unwrap_db (fun () -> "listing distinct files")
  in
  match !r with
  | None -> []
  | Some p -> [ Summarize_dirs.string_of_path p ]

(* FIXME:
   Summarize_dirs.setup_fun db;
   Misc.err_with
   ~map_err:(Printf.sprintf "while computing dirs from DB: %s")
   (fun scope ->
     Db.exec db ~f:Db.Cursor.to_list
       ~ty:Db.Ty.(nil, p1 text, id)
       {| select mergepaths(distinct file) from prover_res; |}
     |> scope.unwrap_with Db.Rc.to_string)
*)

(* build statistics and list of mismatch from raw results *)

let is_solved = function
  | Res.Sat | Res.Unsat -> true
  | _ -> false

let of_events_for ~(prover : Prover.name) (events : Run_event.t list) : t =
  let ok = ref 0 and bad = ref 0 and improved = ref 0 and disappoint = ref 0 in
  let errors = ref 0 in
  let bad_full = ref [] and errors_full = ref [] in
  let valid_proof = ref 0 and invalid_proof = ref 0 in
  let invalid_proof_full = ref [] in
  List.iter
    (function
      | Run_event.Prover_run r when r.Run_result.program = prover ->
        let res = r.Run_result.res in
        let expected = r.Run_result.problem.Problem.expected in
        let rtime = r.Run_result.raw.Run_proc_result.rtime in
        let pb = r.Run_result.problem in
        if res = expected then
          incr ok
        else if res = Res.Error then (
          incr errors;
          errors_full := (pb, res, rtime) :: !errors_full
        ) else if is_solved res && is_solved expected && res <> expected then (
          incr bad;
          bad_full := (pb, res, rtime) :: !bad_full
        ) else if is_solved res && not (is_solved expected) then
          incr improved
        else if (not (is_solved res)) && is_solved expected then
          incr disappoint
      | Run_event.Checker_run r when fst r.Run_result.program = prover ->
        let rtime = r.Run_result.raw.Run_proc_result.rtime in
        let pb = r.Run_result.problem in
        (match r.Run_result.res with
        | Proof_check_res.Valid -> incr valid_proof
        | Proof_check_res.Invalid ->
          incr invalid_proof;
          invalid_proof_full :=
            (pb, r.Run_result.res, rtime) :: !invalid_proof_full
        | Proof_check_res.Unknown _ -> ())
      | _ -> ())
    events;
  let total = !ok + !bad + !improved + !disappoint + !errors in
  {
    ok = !ok;
    bad = !bad;
    improved = !improved;
    disappoint = !disappoint;
    bad_full = !bad_full;
    errors = !errors;
    errors_full = !errors_full;
    valid_proof = !valid_proof;
    invalid_proof = !invalid_proof;
    invalid_proof_full = !invalid_proof_full;
    total;
  }

let of_events ?(full = false) (events : Run_event.t list) :
    (Prover.name * t) list =
  let provers =
    List.filter_map
      (function
        | Run_event.Prover_run r -> Some r.Run_result.program
        | _ -> None)
      events
    |> List.sort_uniq String.compare
  in
  let results = List.map (fun p -> p, of_events_for ~prover:p events) provers in
  if full then
    results
  else
    List.map
      (fun (p, r) ->
        p, { r with bad_full = []; errors_full = []; invalid_proof_full = [] })
      results

let of_events_n_bad (events : Run_event.t list) : int =
  List.fold_left
    (fun acc -> function
      | Run_event.Prover_run r ->
        let res = r.Run_result.res in
        let expected = r.Run_result.problem.Problem.expected in
        if is_solved res && is_solved expected && res <> expected then
          acc + 1
        else
          acc
      | _ -> acc)
    0 events

let of_events_dirs (events : Run_event.t list) : string list =
  let paths =
    List.filter_map
      (function
        | Run_event.Prover_run r ->
          Some (Filename.dirname r.Run_result.problem.Problem.name)
        | _ -> None)
      events
    |> List.sort_uniq String.compare
  in
  match paths with
  | [] -> []
  | _ -> paths

let is_ok r = r.bad = 0
let num_bad r = r.bad
let get_improved r = r.improved
let get_ok r = r.ok
let get_disappoint r = r.disappoint
let get_bad r = r.bad
let get_valid_proof r = r.valid_proof
let get_invalid_proof r = r.invalid_proof
let get_errors r = r.errors

let to_printbox_ ~header ?link:to_link (l : (Prover.name * t) list) : PrintBox.t
    =
  let open PrintBox in
  let mk_row ?ex lbl get_r mk_box : PrintBox.t list =
    match to_link with
    | Some f ->
      let uri p : string =
        let ex = CCOpt.get_or ~default:lbl ex in
        f p ex
      in
      text lbl
      :: List.map
           (fun (p, r) ->
             let n = get_r r in
             if n > 0 then
               link ~uri:(uri p) (mk_box n)
             else
               mk_box n)
           l
    | _ -> text lbl :: List.map (fun (_p, r) -> mk_box (get_r r)) l
  in
  let rows =
    [
      mk_row "improved" get_improved @@ pb_int_color Style.(fg_color Blue);
      mk_row "ok" get_ok @@ pb_int_color Style.(fg_color Green);
      mk_row "disappoint" get_disappoint @@ pb_int_color Style.(fg_color Yellow);
      mk_row "bad" get_bad @@ pb_int_color Style.(fg_color Red);
      mk_row "valid proof" get_valid_proof
      @@ pb_int_color Style.(fg_color Green);
      mk_row "invalid proof" get_invalid_proof
      @@ pb_int_color Style.(fg_color Red);
      mk_row ~ex:"error" "errors" get_errors
      @@ pb_int_color Style.(fg_color Cyan);
      text "total" :: List.map (fun (_, r) -> int r.total) l;
    ]
  in
  grid_l ~bars:true (header :: rows)

let to_printbox_l ?link l =
  let header = List.map PrintBox.text @@ ("provers" :: List.map fst l) in
  to_printbox_ ~header ?link l

let to_printbox_bad ?link:(mk_link = default_linker) r : PrintBox.t =
  let open PrintBox in
  if r.bad <> 0 then (
    let l =
      CCList.map
        (fun (pb, res, t) ->
          [
            mk_link pb.Problem.name;
            text (Res.to_string res);
            text (Res.to_string pb.Problem.expected);
            text (Misc.human_duration t);
          ])
        r.bad_full
    in
    let header =
      let tb = text_with_style Style.bold in
      [ tb "problem"; tb "res"; tb "expected"; tb "time" ]
    in
    grid_l (header :: l)
  ) else
    empty

let to_printbox_bad_l ?(link = default_pp_linker) =
  CCList.filter_map (fun ((p : string), a) ->
      if a.bad = 0 then
        None
      else
        Some
          ( p,
            CCList.map (fun (pb, _, _) -> pb.Problem.name) a.bad_full,
            to_printbox_bad ~link:(link p) a ))

let to_printbox_invalid_proof ?link:(mk_link = default_linker) r : PrintBox.t =
  let open PrintBox in
  if r.invalid_proof <> 0 then (
    let l =
      r.invalid_proof_full
      |> CCList.map (fun (pb, res, t) ->
             [
               mk_link pb.Problem.name;
               text (Proof_check_res.to_string res);
               text (Misc.human_duration t);
             ])
    in
    let header =
      let tb = text_with_style Style.bold in
      [ tb "problem"; tb "proof check res"; tb "time" ]
    in
    grid_l (header :: l)
  ) else
    empty

let to_printbox_invalid_proof_l ?(link = default_pp_linker) =
  CCList.filter_map (fun ((p : string), a) ->
      if a.invalid_proof = 0 then
        None
      else
        Some
          ( p,
            CCList.map (fun (pb, _, _) -> pb.Problem.name) a.invalid_proof_full,
            to_printbox_invalid_proof ~link:(link p) a ))

let to_printbox_errors ?link:(mk_link = default_linker) r : PrintBox.t =
  let open PrintBox in
  if r.errors <> 0 then (
    let l =
      CCList.map
        (fun (pb, res, t) ->
          [
            mk_link pb.Problem.name;
            text (Res.to_string res);
            text (Res.to_string pb.Problem.expected);
            text (Misc.human_duration t);
          ])
        r.errors_full
    in
    let header =
      let tb = text_with_style Style.bold in
      [ tb "problem"; tb "res"; tb "expected"; tb "time" ]
    in
    grid_l (header :: l)
  ) else
    empty

let to_printbox_errors_l ?(link = default_pp_linker) =
  CCList.filter_map (fun ((p : string), a) ->
      if a.errors = 0 then
        None
      else
        Some
          ( p,
            CCList.map (fun (pb, _, _) -> pb.Problem.name) a.errors_full,
            to_printbox_errors ~link:(link p) a ))

let pp_bad out self =
  if self.bad <> 0 then
    Format.fprintf out "@[<hv1>bad@ %a@]" PrintBox_text.pp
      (to_printbox_bad self)

let pp out self : unit =
  let pp_z_or_err out d =
    if d = 0 then
      Fmt.int out d
    else
      Fmt.(with_color "Red" int) out d
  in
  Format.fprintf out
    "(@[<hv>:ok %d@ :improved %d@ :disappoint %d@ :bad %a@ :total %d@])%a"
    self.ok self.improved self.disappoint pp_z_or_err self.bad self.total pp_bad
    self
