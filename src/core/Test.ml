(* This file is free software. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

module E = CCResult
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
    (CCList.map (fun (field,value) -> [PB.text field; value]) l)

let pb_int_color c n =
  let open PB in
  if n=0 then int n
  else text_with_style (Style.set_bold true c) (string_of_int n)

(* list provers from the main table *)
let list_provers db : string list or_error =
  Db.exec_no_params db
    {| select distinct prover from prover_res ; |}
    ~ty:Db.Ty.(p1 text, id) ~f:Db.Cursor.to_list_rev
  |> Misc.db_err ~ctx:"listing provers"

(** URL providers *)

type string_linker = string -> string
type prover_string_linker = Prover.name -> string_linker
type path_linker = Problem.path -> PrintBox.t
type prover_path_linker = Prover.name -> Problem.path -> PrintBox.t
type prover_path_res_linker = Prover.name -> Problem.path -> res:string -> PrintBox.t

let default_linker path = PB.text path
let default_pp_linker _ path = default_linker path
let default_ppr_linker _ _ ~res = default_linker res

(** {2 Basic stats on results} *)

module Stat : sig
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

  val to_printbox_l :
    ?to_link:prover_string_linker ->
    (string*t) list ->
    PB.t

  val of_db_for : prover:Prover.name -> Db.t -> t or_error
  val of_db : Db.t -> (Prover.name * t) list or_error

  val pp : t Fmt.printer
end = struct
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
  let of_db_for ~(prover:Prover.name) (db:Db.t) : t or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading stat(%s) from DB: %s" prover)
      (fun scope ->
         let f c = Db.Cursor.next c |> CCOpt.to_result "no result" |> scope.unwrap in
         let custom = Prover.tags_of_db db in
         let get_res r =
           Logs.debug (fun k->k "get-res %s" r);
           Db.exec db
             {| select count(*) from prover_res where prover=? and res=?; |}
             prover r ~ty:Db.Ty.(p2 text text, p1 (nullable int), CCOpt.get_or ~default:0) ~f
           |> scope.unwrap_with Db.Rc.to_string
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
           |> scope.unwrap_with Db.Rc.to_string
         in
         { sat; unsat; timeout; memory; unknown; errors; custom; total; total_time; }
      )

  let of_db (db:Db.t) : (Prover.name * t) list or_error =
    Profile.with_ "stat.of-db" @@ fun () ->
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading stats from DB: %s")
      (fun scope ->
         let provers = list_provers db |> scope.unwrap in
         CCList.map
           (fun p -> p, of_db_for db ~prover:p |> scope.unwrap)
           provers)

  let pp out (s:t) : unit =
    fpf out
      "(@[<hv>:unsat %d@ :sat %d@ :solved %d@ :errors %d@ :unknown %d@ \
       :timeout %d@ :total %d@ :total_time %.2f@])"
      s.unsat s.sat (s.sat + s.unsat) s.errors s.unknown s.timeout
      (s.unsat + s.sat + s.errors + s.unknown + s.timeout)
      s.total_time
end

(** {2 Basic analysis of results} *)

module Summarize_dirs : sig
  type path = string list
  type path_opt = path option
  val init : path_opt
  val string_of_path : path -> string
  val merge_path1 : path_opt -> Db.Data.t -> path_opt
  val setup_fun : Db.t -> unit
end = struct
  type path = string list
  type path_opt = path option

  (* split path into components *)
  let split_path_ s : path =
    let rec aux acc s =
      let dir = Filename.dirname s in
      if s="." then acc
      else if s<>dir then aux (Filename.basename s::acc) dir
      else s::acc
    in
    aux [] s

  let rec merge_paths (p1:path) (p2:path) : path =
    match p1, p2 with
    | [], _ | _, [] -> []
    | d1 :: tl1, d2 :: tl2 ->
      if d1=d2 then d1 :: merge_paths tl1 tl2 else []

  let nopath_ = None
  let init = nopath_

  let merge_path1 (data:path_opt) (d2:Db.Data.t) : path_opt =
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

  let finalize path : Db.Data.t =
    match path with
    | None -> Db.Data.NULL
    | Some path ->
      let path = string_of_path path in
      Db.Data.TEXT path

  let setup_fun db : unit =
    Sqlite3.Aggregate.create_fun1 db "mergepaths"
      ~init ~final:finalize ~step:merge_path1;
end

module Analyze : sig
  type t = {
    improved  : int;
    ok        : int;
    disappoint: int;
    bad       : int; (* mismatch *)
    bad_full  : (Problem.t * Res.t * float) list; (* always materialized *)
    errors    : int;
    errors_full : (Problem.t * Res.t * float) list;
    total     : int;
  }

  val of_db_for : ?full:bool -> Db.t -> prover:Prover.name -> t or_error
  val of_db : ?full:bool -> Db.t -> (Prover.name * t) list or_error

  val of_db_dirs : Db.t -> string list or_error
  val of_db_n_bad : Db.t -> int or_error
  (** Compute number of bad results *)

  val to_printbox_l : ?link:prover_string_linker ->
    (Prover.name * t) list -> PrintBox.t
  val to_printbox_bad : ?link:path_linker -> t -> PrintBox.t
  val to_printbox_bad_l :
    ?link:prover_path_linker ->
    (Prover.name * t) list -> (string*string list*PrintBox.t) list
  val to_printbox_errors : ?link:path_linker -> t -> PrintBox.t
  val to_printbox_errors_l :
    ?link:prover_path_linker ->
    (Prover.name * t) list -> (string*string list*PrintBox.t) list

  val is_ok : t -> bool

  val num_bad : t -> int

  val pp : t Fmt.printer
  val pp_bad : t Fmt.printer
end = struct
  type t = {
    improved  : int;
    ok        : int;
    disappoint: int;
    bad       : int; (* mismatch *)
    bad_full  : (Problem.t * Res.t * float) list; (* always materialized *)
    errors    : int;
    errors_full : (Problem.t * Res.t * float) list;
    total     : int;
  }

  let int1_cursor ~ctx (scope: _ Misc.try_scope) c =
    Db.Cursor.next c
    |> CCOpt.to_result ("expected a result in "^ctx)
    |> scope.unwrap

  let get1_int (scope: _ Misc.try_scope) db ~ctx q ~ty p =
    Db.exec db q ~ty p ~f:(int1_cursor ~ctx scope)
    |> scope.unwrap_with Db.Rc.to_string

  let of_db_for ?(full=false) (db:Db.t) ~prover : t or_error =
    let tags = Prover.tags_of_db db in
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading analyze(%s) from DB: %s" prover)
      (fun scope ->
         let get1_int = get1_int scope db in
         let ok =
           get1_int ~ctx:"get ok results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and res=file_expect; |}
             prover
         and disappoint =
           get1_int ~ctx:"get disappoint results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and not (res in ('sat','unsat'))
                and file_expect in ('sat','unsat'); |}
             prover
         and improved =
           get1_int ~ctx:"get improved results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and res in ('sat','unsat')
                and not (file_expect in ('sat','unsat')); |}
             prover
         and total =
           get1_int ~ctx:"get total results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?;|} prover
         and bad =
           get1_int ~ctx:"get bad results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and res in ('sat','unsat')
                and res != file_expect
                and file_expect in ('sat','unsat'); |}
             prover
         and bad_full =
           if not full then []
           else Db.exec db
             {| select file, res, file_expect, rtime from prover_res
              where prover=? and res != file_expect and res in ('sat','unsat')
              and file_expect in ('sat','unsat'); |}
             prover
             ~ty:Db.Ty.(p1 text, p4 text text text float,
                        (fun file res expected t ->
                           Problem.make file (Res.of_string ~tags expected),
                           Res.of_string ~tags res, t))
             ~f:Db.Cursor.to_list_rev
           |> scope.unwrap_with Db.Rc.to_string
         and errors =
           get1_int ~ctx:"get errors results"
             ~ty:Db.Ty.(p1 text, p1 int, id)
             {| select count(*) from prover_res where prover=?
                and res = 'error' ; |}
             prover
         and errors_full =
           if not full then []
           else Db.exec db
             ~ty:Db.Ty.(p1 text, p4 text text text float,
                        (fun file res expected t ->
                           Problem.make file (Res.of_string ~tags expected),
                           Res.of_string ~tags res, t))
             {| select file, res, file_expect, rtime from prover_res where prover=?
                and res = 'error' ; |}
             prover ~f:Db.Cursor.to_list_rev
           |> scope.unwrap_with Db.Rc.to_string
         in
         { ok; disappoint; improved; bad; bad_full; errors; errors_full; total; })

  (* TODO: create a function for "better"
        Sqlite3.create_fun2
  *)

  let of_db ?(full=false) db : _ list or_error =
    Profile.with_ "test.analyze" @@ fun () ->
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading top-res from DB: %s")
      (fun scope ->
         let provers = list_provers db |> scope.unwrap in
         CCList.map (fun p -> p, of_db_for ~full db ~prover:p |> scope.unwrap) provers)

  let of_db_n_bad (db:Db.t) : int or_error =
    Profile.with_ "test.analyze.n-bad" @@ fun () ->
    Misc.err_with
      ~map_err:(Printf.sprintf "while computing n-bad from DB: %s")
      (fun scope ->
         Db.exec db ~f:(int1_cursor ~ctx:"extracting n-bad" scope)
           ~ty:Db.Ty.(nil, p1 int, id)
           {| select count(*) from prover_res
                where res in ('sat','unsat')
                and res != file_expect
                and file_expect in ('sat','unsat'); |}
         |> scope.unwrap_with Db.Rc.to_string)

  let of_db_dirs (db:Db.t) : string list or_error =
    Profile.with_ "test.analyze.dirs" @@ fun () ->
    (* use ocaml function *)
    (*
    Misc.err_with
      ~map_err:(Printf.sprintf "while computing dirs from DB: %s")
      (fun scope ->
         let r = ref Summarize_dirs.init in
         let() = Db.exec db
           ~ty:Db.Ty.(nil, p1 data, id)
           {| select distinct file from prover_res; |}
           ~f:(fun c ->
               Db.Cursor.iter c ~f:(fun d -> r := Summarize_dirs.merge_path1 !r d))
                 |> scope.unwrap_with Db.Rc.to_string
         in
         match !r with
           | None -> []
           | Some p -> [Summarize_dirs.string_of_path p]
      )
       *)
    Summarize_dirs.setup_fun db;
    Misc.err_with
      ~map_err:(Printf.sprintf "while computing dirs from DB: %s")
      (fun scope ->
         Db.exec db ~f:Db.Cursor.to_list
           ~ty:Db.Ty.(nil, p1 text, id)
           {| select mergepaths(distinct file) from prover_res; |}
         |> scope.unwrap_with Db.Rc.to_string)

  (* build statistics and list of mismatch from raw results *)

  let is_ok r = r.bad = 0
  let num_bad r = r.bad

  let get_improved r = r.improved
  let get_ok r = r.ok
  let get_disappoint r = r.disappoint
  let get_bad r = r.bad
  let get_errors r = r.errors

  let to_printbox_ ~header ?link:to_link (l:(Prover.name * t) list) : PrintBox.t =
    let open PB in
    let mk_row ?ex lbl get_r mk_box : PB.t list =
      match to_link with
      | Some f ->
        let uri p : string =
          let ex = CCOpt.get_or ~default:lbl ex in
          f p ex
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
    in
    let rows = [
      mk_row "improved" get_improved @@ pb_int_color Style.(fg_color Blue);
      mk_row "ok" get_ok @@ pb_int_color Style.(fg_color Green);
      mk_row "disappoint" get_disappoint @@ pb_int_color Style.(fg_color Yellow);
      mk_row "bad" get_bad @@ pb_int_color Style.(fg_color Red);
      mk_row ~ex:"error" "errors" get_errors @@ pb_int_color Style.(fg_color Cyan);
      PB.text "total" :: List.map (fun (_,r) -> int r.total) l;
    ] in
    PB.grid_l ~bars:true (header :: rows)

  let to_printbox_l ?link l =
    let header = List.map PB.text @@ ("provers" :: List.map fst l) in
    to_printbox_ ~header ?link l

  let to_printbox_bad ?link:(mk_link=default_linker) r : PrintBox.t =
    let open PB in
    if r.bad <> 0 then (
      let l =
        CCList.map
          (fun (pb, res, t) ->
             [ mk_link pb.Problem.name;
               text (Res.to_string res);
               text (Res.to_string pb.Problem.expected);
               text (Misc.human_duration t);
             ])
          r.bad_full
      in
      let header =
        let tb = text_with_style Style.bold in
        [tb "problem"; tb "res"; tb "expected"; tb "time"] in
      grid_l (header :: l)
    ) else empty

  let to_printbox_bad_l ?(link=default_pp_linker) =
    CCList.filter_map
      (fun ((p:string), a) ->
         if a.bad = 0 then None
         else Some (p, CCList.map (fun (pb,_,_) -> pb.Problem.name) a.bad_full,
                    to_printbox_bad ~link:(link p) a))

  let to_printbox_errors ?link:(mk_link=default_linker) r : PrintBox.t =
    let open PB in
    if r.errors <> 0 then (
      let l =
        CCList.map
          (fun (pb, res, t) ->
             [ mk_link pb.Problem.name;
               text (Res.to_string res);
               text (Res.to_string pb.Problem.expected);
               text (Misc.human_duration t);
             ])
          r.errors_full
      in
      let header =
        let tb = text_with_style Style.bold in
        [tb "problem"; tb "res"; tb "expected"; tb "time"] in
      grid_l (header :: l)
    ) else empty

  let to_printbox_errors_l ?(link=default_pp_linker) =
    CCList.filter_map
      (fun ((p:string), a) ->
         if a.errors = 0 then None
         else Some (p, CCList.map (fun (pb,_,_) -> pb.Problem.name) a.errors_full,
                    to_printbox_errors ~link:(link p) a))

  let pp_bad out self =
    if self.bad <> 0 then (
      Format.fprintf out "@[<hv1>bad@ %a@]"
        PrintBox_text.pp (to_printbox_bad self)
    )

  let pp out self : unit =
    let pp_z_or_err out d =
      if d=0 then Fmt.int out d
      else Fmt.(with_color "Red" int) out d
    in
    Format.fprintf out
      "(@[<hv>:ok %d@ :improved %d@ :disappoint %d@ :bad %a@ :total %d@])%a"
      self.ok self.improved self.disappoint
      pp_z_or_err self.bad
      self.total
      pp_bad self
end

(** {2 Lightweight Comparison between bench runs} *)

module Comparison_short : sig
  type single = {
    better: int;
    worse: int;
    same: int;
  }

  type t = {
    provers: Prover.name list;
    tbl: (Prover.name * Prover.name * single) list;
  }

  val of_db : Db.t -> t or_error

  val to_printbox_l : t -> PrintBox.t
end = struct
  type single = {
    better: int;
    worse: int;
    same: int;
  }

  type t = {
    provers: Prover.name list;
    tbl: (Prover.name * Prover.name * single) list;
  }

  let of_db db : t or_error =
    (* get a single integer *)
    let db_get db s x1 x2 =
      Db.exec db s x1 x2 ~ty:Db.Ty.(p2 text text, p1 int, id) ~f:Db.Cursor.to_list_rev
      |> Misc.db_err ~ctx:"extract comparison"
      |> E.flat_map (function [x] -> Ok x | _ -> Error "expected a single integer")
    in
    Misc.err_with
      ~map_err:(Printf.sprintf "comparison-short.of_db %s")
    @@ fun scope ->
    let provers = list_provers db |> scope.unwrap in
    (* TODO: make a single query and group-by? *)
    let tbl =
      CCList.diagonal provers
      |> List.rev_map
        (fun (p1,p2) ->
           assert (p1 <> p2);
           let better =
             db_get db
               {|select count(r1.file) from prover_res r1, prover_res r2
                    where r1.prover=? and r2.prover=? and r1.file=r2.file
                    and r1.res in ('sat','unsat') and not (r2.res in ('sat','unsat')); |}
               p1 p2 |> scope.unwrap
           and worse =
             db_get db
               {|select count(r1.file) from prover_res r1, prover_res r2
                    where r1.prover=? and r2.prover=? and r1.file=r2.file
                    and not (r1.res in ('sat','unsat')) and (r2.res in ('sat','unsat')); |}
               p1 p2 |> scope.unwrap
           and same =
             db_get db
               {|select count(r1.file) from prover_res r1, prover_res r2
                    where r1.prover=? and r2.prover=? and r1.file=r2.file
                    and r1.res in ('sat','unsat') and (r2.res in ('sat','unsat')); |}
               p1 p2 |> scope.unwrap
           in
           p1, p2, {better; worse; same}
        )
    in
    {provers; tbl}

  exception Find_int of (int * int * int)

  let to_printbox_l_ (self:t) =
    let get_pair p1 p2 =
      try
        List.iter (fun (p1',p2',r) ->
            if p1=p1' && p2=p2' then raise (Find_int (r.better, r.worse, r.same))
            else if p1=p2' && p2=p1' then raise (Find_int (r.worse, r.better, r.same))
          ) self.tbl;
        assert false
      with Find_int x -> x
    in
    let headers =
      PB.text "better:" ::
      List.map (PB.text_with_style PB.Style.bold) self.provers in
    let tbl =
      List.map
        (fun p2 ->
           PB.text p2 ::
           List.map
             (fun p1 ->
                if p1=p2 then (
                  PB.center_hv @@ PB.text "×"
                ) else (
                 let bet, worse, same = get_pair p1 p2 in
                 (* TODO: URL for detailed comparison p1 vs p2 *)
                 let tsf s x = PB.text @@ Printf.sprintf s x in
                 PB.center_hv @@ PB.vlist ~bars:false [
                   tsf "same: %d" same;
                   tsf "better: %d" bet;
                   tsf "worse: %d" worse;
                 ]
               ))
             self.provers)
        self.provers
    in
    PB.grid_l ~bars:true (headers :: tbl)
  (* TODO: grid display (-> to array, then by index + reverse when i<j?)
     let to_printbox_grid l : PB.t =
  *)

  let to_printbox_l self =
    if List.length self.provers >= 2 then (
      to_printbox_l_ self
    ) else PB.empty (* nothing interesting *)
end

type metadata = {
  uuid: Uuidm.t; (* unique ID *)
  timestamp: float option; (* timestamp *)
  total_wall_time: float option;
  n_results: int;
  n_bad: int;
  dirs: string list;
  provers: Prover.name list;
}

module Metadata = struct
  type t = metadata

  let is_complete self = CCOpt.is_some self.total_wall_time

  let to_printbox ?link:(mk_link=default_linker) self : PB.t =
    let open PB in
    pb_v_record @@ List.flatten [
      ["provers", vlist_map mk_link self.provers;
       "n_results", int self.n_results; ];
      (if self.n_bad>0 then ["bad", int self.n_bad] else []);
      ["uuid", text @@ Uuidm.to_string self.uuid;
       "dirs", hlist_map text self.dirs;
       "timestamp", (match self.timestamp with
           | None -> text "none"
           | Some f -> text @@ Misc.human_datetime f);
       "total_wall_time", (match self.total_wall_time with
           | None -> text "none" | Some f -> text @@ Misc.human_duration f);
      ]
    ]

  let db_prepare (db:Db.t) : _ or_error =
    Db.exec0 db {|
        create table if not exists
        meta(
          key text not null unique,
          value blob
          );
        create index if not exists meta_k on meta(key);
        |} |> Misc.db_err ~ctx:"top-res.db-prepare"

  let get_meta db k : _ =
    Db.exec_exn db {|select value from meta where key=? ;|}
      k
      ~ty:Db.Ty.(p1 text, p1 (nullable any_str), id)
      ~f:Db.Cursor.next
    |> CCOpt.to_result ("did not find metadata " ^ k)

  let to_db (db:Db.t) (self:t) : unit or_error =
    Db.exec_no_cursor db
      "insert or replace into meta values
      ('timestamp', ?), ('total-wall-time', ?), ('uuid', ?);"
      ~ty:Db.Ty.(p3 (nullable blob) (nullable blob) text)
      (CCOpt.map string_of_float self.timestamp)
      (CCOpt.map string_of_float self.total_wall_time)
      (Uuidm.to_string self.uuid)
    |> Misc.db_err ~ctx:"inserting metadata"

  let of_db db : t or_error =
    Profile.with_ "metadata.of-db" @@ fun () ->
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading metadata: %s")
      (fun scope ->
         let timestamp = get_meta db "timestamp" |> scope.unwrap in
         let total_wall_time = get_meta db "total-wall-time" |> scope.unwrap in
         let timestamp = CCOpt.map float_of_string timestamp in
         let uuid = get_meta db "uuid" |> scope.unwrap
                    |> CCOpt.flat_map Uuidm.of_string
                    |> CCOpt.to_result "no uuid found in DB"
                    |> scope.unwrap in
         let total_wall_time = CCOpt.map float_of_string total_wall_time in
         let n_results =
           Db.exec_no_params_exn db "select count(*) from prover_res;"
             ~ty:Db.Ty.(p1 int,id) ~f:Db.Cursor.next
           |> CCOpt.to_result "no prover results" |> scope.unwrap
         in
         let n_bad = Analyze.of_db_n_bad db |> scope.unwrap in
         let dirs = Analyze.of_db_dirs db |> scope.unwrap in
         Logs.debug (fun k->k "dirs: [%s]" (String.concat "," dirs));
         let provers =
           Db.exec_no_params_exn db "select distinct name from prover;"
             ~f:Db.Cursor.to_list_rev ~ty:Db.Ty.(p1 any_str, id)
         in
         { timestamp; total_wall_time; uuid; n_results; n_bad; dirs; provers; })

  let pp_l out (self:t) : unit =
    Fmt.fprintf out
      "@[<v>n-results: %d%s@ provers: [%s]@ timestamp: \
       %s@ total-time: %s@ uuid: %a@]"
      self.n_results
      (if self.n_bad>0 then Printf.sprintf " bad: %d" self.n_bad else "")
      (String.concat ";" self.provers)
      (CCOpt.map_or ~default:"<no time>" Misc.human_datetime self.timestamp)
      (CCOpt.map_or ~default:"<no wall time>" Misc.human_duration self.total_wall_time)
      Uuidm.pp self.uuid

  let to_string self = Fmt.asprintf "%a" pp_l self
end

(** {2 Lightweight Results} *)

(** A kind of lightweight result *)
type compact_result = {
  cr_meta: metadata;
  cr_stat: (Prover.name * Stat.t) list;
  cr_analyze: (Prover.name * Analyze.t) list;
  cr_comparison: Comparison_short.t;
}

module Compact_result = struct
  type t = compact_result

  let of_db ?full db : t or_error =
    Profile.with_ "compact-res.of-db" @@ fun () ->
    let open E.Infix in
    Db.transact db (fun _ ->
      Metadata.of_db db >>= fun cr_meta ->
      Stat.of_db db >>= fun cr_stat ->
      Analyze.of_db ?full db >>= fun cr_analyze ->
      Comparison_short.of_db db >>= fun cr_comparison ->
      Ok {cr_stat; cr_analyze; cr_comparison; cr_meta; })

  let pp out _self = Fmt.fprintf out "<compact result>"
end

(** {2 Cactus plots} *)

module Cactus_plot : sig
  type t

  val of_db : Db.t -> t or_error
  val of_file : string -> t or_error

  val show : t -> unit
  val save_to_file : t -> string -> unit
  val to_png : t -> string
end = struct
  module Gp = Gnuplot

  type t = {
    lines: (Prover.name * float list) list;
  }

  let of_db db =
    Profile.with_ "plot.of-db" @@ fun () ->
    Misc.err_with
      ~map_err:(Printf.sprintf "while plotting DB: %s")
      (fun scope ->
         let provers = list_provers db |> scope.unwrap in
         Logs.debug (fun k->k "provers: [%s]" (String.concat ";" provers));
         let has_custom_tags =
           try ignore (Db.exec0_exn db "select 1 from custom_tags;"); true
           with _ -> false
         in
         let get_prover prover =
           Db.exec db
             (Printf.sprintf {| select rtime from prover_res
              where prover=? and
              (res in ('sat','unsat')
               %s
               )
              order by rtime|}
                (if has_custom_tags then "or exists (select 1 from custom_tags where tag=res)" else ""))
             prover
             ~ty:Db.Ty.(p1 text, p1 float, id) ~f:Db.Cursor.to_list
           |> scope.unwrap_with Db.Rc.to_string
         in
         let lines = CCList.map (fun p -> p, get_prover p) provers in
         { lines }
      )

  let of_file file : t or_error =
    try
      Db.with_db ~timeout:500 ~mode:`READONLY file of_db
    with e -> E.of_exn_trace e

  let to_gp ~output self =
    Profile.with_ "plot.gnuplot" @@ fun () ->
    Gp.with_ (fun gp ->
        let series =
          self.lines
          |> CCList.map
            (fun (prover,l) ->
               let l =
                 let sum = ref 0. in
                 CCList.mapi
                   (fun i rtime ->
                      sum := !sum +. rtime;
                      (!sum, float i))
                   l
               in
               Gp.Series.linespoints_xy ~title:prover l)
        in
        Gp.plot_many
          ~labels:(Gp.Labels.create ~x:"time (s)" ~y:"problems solved (accumulated)" ())
          ~title:"cumulative time for n° of problems solved" gp series ~output);
    ()

  let show (self:t) =
    to_gp self ~output:(Gp.Output.create `X11)

  let save_to_file (self:t) file =
    to_gp self ~output:(Gp.Output.create ~size:(1800,1024) @@ `Png file)

  let to_png (self:t) : string =
    Profile.with_ "plot.to-png" @@ fun () ->
    CCIO.File.with_temp ~prefix:"benchpress_plot" ~suffix:".png"
      (fun file ->
         Logs.debug (fun k->k "plot into file %s" file);
         save_to_file self file;
         let s = CCIO.with_in file CCIO.read_all in
         Logs.debug (fun k->k "read %d bytes from file" (String.length s));
         s)
end

(** {2 Utils} *)

(** Cleanup a wildcard query for sqlite filtering *)
let clean_s wildcard s =
  let s = String.trim s in
  if s="" then "%" else if wildcard then "%"^s^"%" else s

(** {2 Top Result}

    Main result of testing: a snapshot of the work done, + the analysis
    per prover *)

type top_result = {
  meta: metadata;
  provers: Prover.t list;
  events: Run_event.t list;
  stats: (Prover.name * Stat.t) list;
  analyze: (Prover.name * Analyze.t) list;
  db: Db.t; (* in-memory database *)
}

(** Filter on the list of all results *)
type tr_filter =
  | TRF_all
  | TRF_different (** different results *)
  | TRF_bad (** inconsistent results *)

module Top_result : sig
  type t = top_result

  val pp : t Fmt.printer
  (** Full printer, including results *)

  val pp_header : t Fmt.printer
  (** Print only meta-information: UUID and timestamp *)

  val pp_compact : t Fmt.printer
  (** Print meta-informations + compact results *)

  val pp_bad : t Fmt.printer

  val is_ok : t -> bool

  val make :
    meta:metadata ->
    provers:Prover.t list ->
    Prover.name Run_result.t list ->
    t or_error
  (** Make from a list of results *)

  val of_db : Db.t -> t or_error
  (** Parse from a DB *)

  val db_prepare : Db.t -> unit or_error

  val to_db : Db.t -> t -> unit or_error
  (** Dump into the DB *)

  val stat : t -> (Prover.name * Stat.t) list
  (** Compute or retrieve stats *)

  val analyze : t -> (Prover.name * Analyze.t) list

  val to_compact_result : t -> compact_result or_error

  (* TODO: move to another file
     type comparison_result = {
     both: ResultsComparison.t MStr.t;
     left: Analyze.t MStr.t;
     right: Analyze.t MStr.t;
     }

     val compare : t -> t -> comparison_result

     val pp_comparison : comparison_result Fmt.printer
     val comparison_to_printbox : ?short:bool -> comparison_result -> PrintBox.t
  *)

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  val db_to_table :
    ?offset:int -> ?page_size:int ->
    ?provers:string list ->
    ?filter_pb:string ->
    ?filter_res:tr_filter ->
    Db.t -> table
  val to_table :
    ?offset:int -> ?page_size:int ->
    ?provers:string list ->
    t -> table

  val table_to_csv : table -> Csv.t

  val table_to_printbox :
    ?link_pb:path_linker -> ?link_res:prover_path_res_linker ->
    table -> PrintBox.t

  val to_printbox_summary : t -> PrintBox.t
  val to_printbox_stat : t -> PrintBox.t

  val to_printbox_table :
    ?offset:int -> ?page_size:int ->
    ?link_pb:path_linker -> ?link_res:prover_path_res_linker ->
    t -> PrintBox.t

  val db_to_printbox_table :
    ?offset:int -> ?page_size:int ->
    ?link_pb:path_linker -> ?link_res:prover_path_res_linker ->
    ?filter_pb:string ->
    ?filter_res:tr_filter ->
    Db.t -> PrintBox.t

  val to_printbox_bad : t -> (string * PrintBox.t) list
  val to_printbox_errors : t -> (string * PrintBox.t) list

  val db_to_csv : ?provers:string list -> Db.t -> Csv.t
  val to_csv : ?provers:string list -> t -> Csv.t

  val to_csv_chan : ?provers:string list -> out_channel -> t -> unit

  val db_to_csv_string : ?provers:string list -> Db.t -> string
  val to_csv_string : ?provers:string list -> t -> string

  val to_csv_file : ?provers:string list -> string -> t -> unit
  (** Write as CSV into given file *)
end = struct
  type t = top_result

  let analyze self = self.analyze
  let stat self = self.stats

  let is_ok self = List.for_all (fun (_,a) -> Analyze.is_ok a) @@ analyze self

  let pp_header out (self:t) : unit =
    Format.fprintf out "(@[(uuid %s)(date %a)@])"
      (Uuidm.to_string self.meta.uuid)
      (Misc.Pp.pp_opt "timestamp" Misc.pp_human_datetime) self.meta.timestamp

  let pp_compact out (self:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp res
    in
    let {analyze=a; _} = self in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header self (pp_list_ pp_tup) a

  let pp_bad out (r:t) : unit =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp_bad res
    in
    let a = analyze r in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header r (pp_list_ pp_tup) a

  let pp out (self:t) : unit =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Fmt.string p Analyze.pp res
    in
    let a = analyze self in
    Format.fprintf out "(@[<2>%a@ %a@])"
      pp_header self (pp_list_ pp_tup) a

  let to_compact_result (self:t) : compact_result or_error =
    let open E.Infix in
    Comparison_short.of_db self.db >>= fun cr_comparison ->
    let cr_analyze = analyze self in
    let cr_stat = stat self in
    E.return {cr_analyze; cr_meta=self.meta; cr_stat; cr_comparison; }

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  let db_to_table ?(offset=0) ?(page_size=max_int) ?provers
      ?(filter_pb="") ?(filter_res=TRF_all)
      (db:Db.t): table =
    let c = Misc.Chrono.start() in
    Misc.err_with
      ~map_err:(Printf.sprintf "while converting to table: %s")
      (fun scope ->
         Db.transact db @@ fun _ ->
         let meta = Metadata.of_db db |> scope.unwrap in
         let line0 =
           Printf.sprintf "(snapshot :uuid %s :date %s)"
             (Uuidm.to_string meta.uuid)
             (CCOpt.map_or ~default:"<none>" Misc.human_datetime meta.timestamp)
         in
         let provers =
           match provers with
           | Some l -> l
           | None -> list_provers db |> scope.unwrap
         in
         let filter_pb = clean_s true filter_pb in
         let filter_res_clause = match filter_res with
           | TRF_all -> ""
           | TRF_different ->
             " and exists (select p1.prover, p2.prover \
              from prover_res p1, prover_res p2 \
              where p1.prover != p2.prover and p1.res != p2.res \
              and p1.file = r.file and p2.file = r.file)"
           | TRF_bad ->
             " and exists (select p1.prover, p2.prover \
              from prover_res p1, prover_res p2 \
              where p1.prover != p2.prover and p1.res != p2.res \
              and p1.res in ('sat','unsat') and p2.res in ('sat','unsat') \
              and p1.file = r.file and p2.file = r.file)"
         in
         let files =
           Db.exec db
             (Printf.sprintf {| select distinct file from prover_res r
               where file like ? %s limit ? offset ?
             ; |} filter_res_clause)
             filter_pb page_size offset
             ~ty:Db.Ty.(p3 text int int, p1 text, id) ~f:Db.Cursor.to_list_rev
                     |> scope.unwrap_with Db.Rc.to_string
         in
         Logs.info (fun k->k"to_table: found %d files in %.3fs" (List.length files)
                   (Misc.Chrono.since_last c));
         let tags = Prover.tags_of_db db in
         Logs.info (fun k->k"to_table: found tags [%s]" (String.concat "," tags));
         let t_rows =
           List.rev_map
             (fun file ->
                let tr_res =
                  Db.exec db
                    {| select prover, res, rtime from
                   prover_res where file=? order by prover; |}
                    file
                    ~ty:Db.Ty.(p1 text, p3 text text float,
                               fun prover res t ->
                                 prover, Res.of_string ~tags res, t)
                    ~f:Db.Cursor.to_list_rev
                  |> scope.unwrap_with Db.Rc.to_string
                  |> List.filter (fun (p,_,_) -> List.mem p provers)
                in
                {tr_problem=file; tr_res})
             files
         in
         Logs.info (fun k->k"to_table: gathered lines in %.3fs" (Misc.Chrono.since_last c));
         {t_meta=line0; t_provers=provers; t_rows}
      )
    |> (function
        | Ok x -> x
        | Error e ->
          Logs.err (fun k->k "conversion to CSV failed: %s" e);
          failwith ("error while converting to CSV: " ^ e))

  let to_table ?offset ?page_size ?provers (self:t): table =
    db_to_table ?offset ?page_size ?provers self.db

  let time_to_csv (_:Res.t) f = Printf.sprintf "%.2f" f
  let res_to_csv (r:Res.t) = match r with
    | Res.Error -> "error"
    | Res.Timeout -> "timeout"
    | Res.Unknown -> "unknown"
    | Res.Sat -> "sat"
    | Res.Unsat -> "unsat"
    | Res.Tag s -> s

  let table_to_csv (t:table): Csv.t =
    let header_line =
      "problem" ::
      t.t_provers @
      (CCList.map (fun p -> p ^ ".time") t.t_provers)
    in
    let lines =
      CCList.map
        (fun r ->
           r.tr_problem
           :: CCList.map (fun (_,res,_) ->  res_to_csv res) r.tr_res
           @ CCList.map (fun (_,res,t) -> time_to_csv res t) r.tr_res)
        t.t_rows
    in
    header_line :: lines

  let db_to_csv ?provers db : Csv.t = db_to_table ?provers db |> table_to_csv
  let to_csv ?provers t : Csv.t = to_table ?provers t |> table_to_csv

  let table_to_printbox
      ?(link_pb=default_linker)
      ?(link_res=default_ppr_linker)
      (self:table) : PB.t =
    let header_line =
      CCList.map PB.(text_with_style Style.bold) @@
      "problem" ::
      self.t_provers @
      (CCList.map (fun p -> p ^ ".time") self.t_provers)
    in
    let lines =
      CCList.map
        (fun r ->
           link_pb r.tr_problem
           :: CCList.map (fun (prover,res,_) ->
               link_res prover r.tr_problem ~res:(res_to_csv res)) r.tr_res
           @ CCList.map (fun (_,res,t) -> PB.text @@ time_to_csv res t) r.tr_res)
        self.t_rows
    in
    PB.grid_l (header_line::lines)

  let to_printbox_stat (self:t) : PB.t =
    let a = stat self in
    Stat.to_printbox_l a

  let to_printbox_summary (self:t) : PB.t =
    let a = analyze self in
    Analyze.to_printbox_l a

  let db_to_printbox_table ?offset ?page_size ?link_pb
      ?link_res ?filter_pb ?filter_res db =
    table_to_printbox ?link_pb ?link_res @@
    db_to_table ?offset ?page_size ?filter_pb ?filter_res db

  let to_printbox_table ?offset ?page_size ?link_pb ?link_res self =
    table_to_printbox ?link_pb ?link_res @@ to_table ?offset ?page_size self

  let to_printbox_bad self =
    let a = analyze self in
    CCList.filter_map
      (fun (p, a) ->
         if a.Analyze.bad = 0 then None
         else Some (p, Analyze.to_printbox_bad a))
      a

  let to_printbox_errors self =
    let a = analyze self in
    CCList.map
      (fun (p, a) -> (p, Analyze.to_printbox_errors a))
      a

  (* TODO
     let comparison_to_printbox ?(short=true) (self:comparison_result) : PB.t =
     let open PB in
     let pm side m =
      MStr.to_list m
      |> CCList.map
        (fun (p,c) ->
           [text_with_style Style.bold (p ^ " ("^side^")");
            Analyze.to_printbox c])
     in
     grid_l @@ List.flatten [
      [
        MStr.to_list self.both
        |> CCList.map
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
  *)

  let to_csv_chan ?provers oc t =
    let chan = Csv.to_channel oc in
    Csv.output_all chan (to_csv ?provers t)

  let to_csv_file ?provers file t =
    let oc = open_out file in
    to_csv_chan ?provers oc t;
    close_out oc

  let str_of_csv_ csv =
    let buf = Buffer.create 256 in
    let ch = Csv.to_buffer buf in
    Csv.output_all ch csv;
    Buffer.contents buf

  let db_to_csv_string ?provers db =
    str_of_csv_ @@ db_to_csv ?provers db

  let to_csv_string ?provers t =
    str_of_csv_ @@ to_csv ?provers t

  let db_prepare (db:Db.t) : _ or_error =
    let open E.Infix in
    Metadata.db_prepare db >>= fun () ->
    Run_event.db_prepare db >>= fun () ->
    Prover.db_prepare db >>= fun () ->
    Ok ()

  let to_db (db:Db.t) (self:t) : unit or_error =
    Logs.info (fun k->k "dump top-result into DB");
    Misc.err_with ~map_err:(Printf.sprintf "while dumping top-res to DB: %s")
      (fun scope ->
         scope.unwrap @@ db_prepare db;
         scope.unwrap @@ Metadata.to_db db self.meta;
         scope.unwrap @@ Prover.db_prepare db;
         (* insert within one transaction, much faster *)
         Db.transact db (fun _ ->
             List.iter (fun p -> Prover.to_db db p |> scope.unwrap) self.provers;
             List.iter (fun ev -> scope.unwrap @@ Run_event.to_db db ev) self.events);
         ())

  let make ~meta ~provers
      (l:Prover.name Run_result.t list) : t or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading top-res from DB: %s")
      (fun scope ->
         let events = List.rev_map Run_event.mk_prover l in
         (* create a temporary in-memory DB *)
         let db = Sqlite3.db_open ":memory:" in
         db_prepare db |> scope.unwrap;
         Metadata.to_db db meta |> scope.unwrap;
         Db.transact db (fun _ ->
             List.iter
               (fun p ->
                  Prover.to_db db p
                  |> scope.unwrap_with
                    (Printf.sprintf "while adding prover %s: %s" p.Prover.name))
               provers;
             List.iter (fun ev -> scope.unwrap @@ Run_event.to_db db ev) events);
         Logs.debug (fun k->k "computing stats");
         let stats = Stat.of_db db |> scope.unwrap in
         Logs.debug (fun k->k "computing analyze");
         let analyze = Analyze.of_db ~full:false db |> scope.unwrap in
         Logs.debug (fun k->k "done");
         { db; events; meta; provers; stats; analyze; })

  let of_db (db:Db.t) : t or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while reading top-res from DB: %s")
      (fun scope ->
         Logs.debug (fun k->k "loading metadata from DB");
         let meta = Metadata.of_db db |> scope.unwrap in
         let prover_names = Prover.db_names db |> scope.unwrap in
         let provers =
           CCList.map (fun p -> Prover.of_db db p |> scope.unwrap) prover_names in
         Logs.debug (fun k->k "loading events from DB");
         let events =
           Run_event.of_db_l db |> scope.unwrap
         in
         make ~meta ~provers events |> scope.unwrap)
end

(** Filter on detailed results *)
type expect_filter =
  | TD_expect_improved
  | TD_expect_ok
  | TD_expect_disappoint
  | TD_expect_bad
  | TD_expect_error

module Detailed_res : sig
  type key = {
    prover: Prover.name;
    file: string;
    res: Res.t;
    file_expect: Res.t;
    rtime: float;
  }
  (** Summary of a result *)

  val list_keys :
    ?offset:int -> ?page_size:int ->
    ?filter_prover:string ->
    ?filter_pb:string ->
    ?filter_res:string ->
    ?filter_expect:expect_filter ->
    Db.t -> (key list * int * bool) or_error
  (** List available results.
      @returns tuple [l, n, is_done], where [is_done] is true if there are
      no more results, and [n] is the total number of results (not just
      those in [l]). *)

  type t = Prover.t Run_result.t
  (** Detailed result *)

  val to_printbox : ?link:prover_path_linker -> t -> PrintBox.t * PrintBox.t * string * string
  (** Display an individual result + prover descr + stdout + stderr *)

  val get_res : Db.t -> Prover.name -> string -> t or_error
  (** Get an individual result *)
end = struct
  type key = {
    prover: Prover.name;
    file: string;
    res: Res.t;
    file_expect: Res.t;
    rtime: float;
  }
  type t = Prover.t Run_result.t

  let list_keys ?(offset=0) ?(page_size=500)
      ?(filter_prover="") ?(filter_pb="") ?(filter_res="") ?filter_expect
      db : (key list*_*_) or_error =
    let filter_prover = clean_s true filter_prover in
    let filter_pb = clean_s true filter_pb in
    (* no wildcard here, as "sat" would match "unsat"… *)
    let filter_res = clean_s false filter_res in
    let filter_expect = match filter_expect with
      | None -> ""
      | Some TD_expect_error -> " and res = 'error'"
      | Some TD_expect_ok ->
        " and res = file_expect "
      | Some TD_expect_disappoint ->
        " and not (res in ('sat','unsat')) and file_expect in ('sat','unsat')"
      | Some TD_expect_bad ->
        " and res in ('sat','unsat') \
        and res != file_expect
        and file_expect in ('sat','unsat')"
      | Some TD_expect_improved ->
        " and res in ('sat', 'unsat') and not (file_expect in ('sat','unsat'))"
    in
    Misc.err_with
      ~map_err:(Printf.sprintf "when listing detailed results: %s")
      (fun scope ->
         let tags = Prover.tags_of_db db in
         (* count total number of results *)
         let n =
           Db.exec db
             (Printf.sprintf {|select count(*)
               from prover_res
               where prover like ? and res like ? %s and file like ?|}
                filter_expect)
             ~ty:Db.Ty.(p3 text text text, p1 int, id) ~f:Db.Cursor.get_one_exn
             filter_prover filter_res filter_pb
           |> E.map_err
             (fun e -> Printf.sprintf "sqlite error: %s" (Db.Rc.to_string e))
           |> scope.unwrap
         in
         (* ask for [limit+1] entries *)
         let l =
           Db.exec db
             (Printf.sprintf {|select distinct prover, file, res, file_expect, rtime
               from prover_res
               where prover like ? and res like ? %s and file like ?
               order by file, prover desc
                limit ? offset ?
              ; |} filter_expect)
             ~ty:Db.Ty.(p5 text text text int int,
                        p5 text any_str text text float,
                        fun prover file res file_expect rtime ->
                          let res=Res.of_string ~tags res in
                          let file_expect=Res.of_string ~tags file_expect in
                          {prover;file;res;file_expect;rtime})
             ~f:Db.Cursor.to_list_rev
             filter_prover filter_res filter_pb (page_size+1) offset
           |> E.map_err
             (fun e -> Printf.sprintf "sqlite error: %s" (Db.Rc.to_string e))
           |> scope.unwrap
         in
         if List.length l > page_size then (
           (* there are more result, cut the last one *)
           CCList.take page_size l, n, false
         ) else (
           l, n, true
         )
      )

  let get_res db prover file : _ or_error =
    Profile.with_ "detailed-res" ~args:["prover",prover] @@ fun () ->
    Misc.err_with
      ~map_err:(Printf.sprintf "in get_res for '%s' on '%s':\n%s" prover file)
      (fun scope ->
         let tags = Prover.tags_of_db db in
         let res: Prover.name Run_result.t =
           Db.exec db
             {|select
                  res, file_expect, timeout, errcode, stdout, stderr,
                  rtime, utime, stime
               from prover_res where prover=? and file=? ; |}
             prover file
             ~f:Db.Cursor.next
             ~ty:Db.Ty.(p2 text text,
                        p2 any_str any_str @>>
                        p4 int int (nullable blob) (nullable blob)
                        @>> p3 float float float,
                        fun x1 x2 x3 x4 x5 x6 x7 x8 x9 ->
                          Logs.info (fun k->k "got results");
                          x1,x2,x3,x4,x5,x6,x7,x8,x9)
           |> E.map_err
             (fun e -> Printf.sprintf "sqlite error: %s" (Db.Rc.to_string e))
           |> scope.unwrap
           |> CCOpt.to_result_lazy
             (fun () ->
                Printf.sprintf
                  "expected a non-empty result for prover='%s', file='%s'"
                  prover file)
           |> scope.unwrap
           |> (fun (res, file_expect, timeout, errcode, stdout, stderr, rtime, utime, stime) ->
               (* Still needed ?! *)
               Gc.compact();
               Gc.full_major();
               Logs.info (fun k->k "got results 2");
               let stdout=CCOpt.get_or ~default:"" stdout in
               let stderr=CCOpt.get_or ~default:"" stderr in
               Logs.debug (fun k->k"res.of_string tags=[%s]" (String.concat "," tags));
               let expected = Res.of_string ~tags file_expect in
               Logs.debug (fun k->k"got expected %a" Res.pp expected);
               let res = Res.of_string ~tags res in
               Logs.debug (fun k->k"got res %a" Res.pp res);
               let timeout = Limit.Time.mk ~s:timeout () in
               Run_result.make prover ~timeout ~res
                 {Problem.name=file; expected}
                 { Proc_run_result.errcode;stdout;stderr;rtime;utime;stime})
         in
         Logs.info (fun k->k "try to get prover");
         let prover = Prover.of_db db prover |> scope.unwrap in
         Logs.info (fun k->k "got prover");
         Run_result.map ~f:(fun _ -> prover) res
      )

  let to_printbox ?link:(mk_link=default_pp_linker) (self:t) : PB.t*PB.t*string*string =
    let open PB in
    Logs.debug (fun k->k "coucou");
    let pp_res r =
      match r with
      | Res.Sat | Res.Unsat -> text_with_style Style.(set_fg_color Green@@bold) @@ Res.to_string r
      | Res.Error -> text_with_style Style.(set_fg_color Red@@bold) @@ Res.to_string r
      | _ -> text @@ Res.to_string r
    in
    Logs.debug (fun k->k "prover is %a" Prover.pp self.program);
    v_record @@ [
      "problem.path", mk_link self.program.Prover.name self.problem.Problem.name;
      "problem.expected_res", pp_res self.problem.Problem.expected;
      "res", pp_res self.res;
      "rtime", text (Misc.human_duration self.raw.rtime);
      "stime", text (Misc.human_duration self.raw.stime);
      "utime", text (Misc.human_duration self.raw.utime);
      "errcode", int self.raw.errcode;
    ],
    v_record @@ List.flatten [
      ["prover.name", text self.program.Prover.name;
       "prover.cmd", text self.program.Prover.cmd;
       "prover.version",
       (text @@ Format.asprintf "%a" Prover.Version.pp self.program.Prover.version);
       "prover.sat", text (CCOpt.get_or ~default:"<none>" self.program.Prover.sat);
       "prover.unsat", text (CCOpt.get_or ~default:"<none>" self.program.Prover.unsat);
       "prover.unknown", text (CCOpt.get_or ~default:"<none>" self.program.Prover.unknown);
       "prover.timeout", text (CCOpt.get_or ~default:"<none>" self.program.Prover.timeout);];
      CCList.map (fun (tag,re) -> "prover.tag." ^ tag, text re) self.program.Prover.custom;
      ["prover.memory", text (CCOpt.get_or ~default:"<none>" self.program.Prover.memory);
      ]
    ],
    self.raw.stdout, self.raw.stderr
end

