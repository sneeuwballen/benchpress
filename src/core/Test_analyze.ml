
open Misc
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
  improved  : int;
  ok        : int;
  disappoint: int;
  bad       : int; (* mismatch *)
  bad_full  : (Problem.t * Res.t * float) list; (* always materialized *)
  invalid_proof: int;
  invalid_proof_full: (Problem.t * Proof_check_res.t * float) list;
  errors    : int;
  errors_full : (Problem.t * Res.t * float) list;
  total     : int;
}

let int1_cursor ~ctx (scope: _ Misc.try_scope) c =
  Db.Cursor.next c
  |> CCOpt.to_result (Error.makef "expected a result in %s" ctx)
  |> scope.unwrap

let get1_int (scope: _ Misc.try_scope) db ~ctx q ~ty p =
  Db.exec db q ~ty p ~f:(int1_cursor ~ctx scope)
  |> scope.unwrap_with Misc.err_of_db

let of_db_for ?(full=false) (db:Db.t) ~prover : t or_error =
  let tags = Prover.tags_of_db db in
  Misc.err_with
    ~map_err:(Error.wrapf "reading analyze(%s) from DB" prover)
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
         |> scope.unwrap_with Misc.err_of_db
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
         |> scope.unwrap_with Misc.err_of_db
       and invalid_proof_full =
         match
           Db.exec db prover
             {| select r.file, r.file_expect, p.rtime, p.res
                from prover_res r, proof_check_res p
                where r.prover=? and r.prover = p.prover and r.file = p.file
                  and p.res = 'invalid';
             |}
             ~ty:Db.Ty.([text], [text;text;float;text],
                        fun file expected rtime pres ->
                          Problem.make file (Res.of_string ~tags expected),
                          Proof_check_res.of_string pres |> scope.unwrap,
                          rtime)
             ~f:Db.Cursor.to_list_rev
         with
         | Ok l -> l
         | Error db ->
           Log.err (fun k->k"cannot list invalid-proofs: %a"
                       Error.pp (Misc.err_of_db db));
           []
         | exception e ->
           Log.err
             (fun k->k"cannot list invalid-proofs: %s" (Printexc.to_string e));
           []
       in
       let invalid_proof = List.length invalid_proof_full in
       { ok; disappoint; improved; bad; bad_full;
         invalid_proof; invalid_proof_full;
         errors; errors_full; total; })

(* TODO: create a function for "better"
      Sqlite3.create_fun2
*)

let of_db ?(full=false) db : _ list or_error =
  Profile.with_ "test.analyze" @@ fun () ->
  Misc.err_with
    ~map_err:(Error.wrap "reading top-res from DB")
    (fun scope ->
       let provers = Test.list_provers db |> scope.unwrap in
       CCList.map (fun p -> p, of_db_for ~full db ~prover:p |> scope.unwrap) provers)

let of_db_n_bad (db:Db.t) : int or_error =
  Profile.with_ "test.analyze.n-bad" @@ fun () ->
  Misc.err_with
    ~map_err:(Error.wrap "computing n-bad from DB")
    (fun scope ->
       Db.exec db ~f:(int1_cursor ~ctx:"extracting n-bad" scope)
         ~ty:Db.Ty.(nil, p1 int, id)
         {| select count(*) from prover_res
              where res in ('sat','unsat')
              and res != file_expect
              and file_expect in ('sat','unsat'); |}
       |> scope.unwrap_with Misc.err_of_db)

let of_db_dirs (db:Db.t) : string list or_error =
  Profile.with_ "test.analyze.dirs" @@ fun () ->
  (* use ocaml function *)
  Misc.err_with
    ~map_err:(Error.wrap "computing dirs from DB")
    (fun scope ->
       let r = ref Summarize_dirs.init in
       let() = Db.exec db
         ~ty:Db.Ty.(nil, p1 data, id)
         {| select distinct file from prover_res; |}
         ~f:(fun c ->
             Db.Cursor.iter c ~f:(fun d -> r := Summarize_dirs.merge_path1 !r d))
               |> scope.unwrap_with Misc.err_of_db
       in
       match !r with
         | None -> []
         | Some p -> [Summarize_dirs.string_of_path p]
    )
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
