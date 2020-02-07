
(** {1 Compare two result files} *)

module Fmt = CCFormat
module E = CCResult
module Db = Sqlite3_utils
module PB = PrintBox
type 'a or_error = ('a, string) CCResult.t
type filename = string

let pb_v_record = Test.pb_v_record
let pb_int_color = Test.pb_int_color

module Short = struct
  type t = {
    appeared: int;  (* new problems *)
    disappeared: int; (* problems that disappeared *)
    improved: int;
    regressed: int;
    mismatch: int;
    same: int; (* same result *)
  }

  let to_printbox (self:t) =
    let open PB in
    pb_v_record [
      "appeared", int self.appeared;
      "disappeared", int self.disappeared;
      "same", int self.same;
      "mismatch", pb_int_color Style.(fg_color Red) self.mismatch;
      "improved", pb_int_color Style.(fg_color Green) self.improved;
      "regressed", pb_int_color Style.(fg_color Cyan) self.regressed;
    ]


  let make f1 f2 : (_ * t) list or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while comparing.short %S and %S: %s" f1 f2)
      (fun scope ->
         let db = Sqlite3.db_open ":memory:" in
         Db.exec_no_cursor db "attach database ? as db1;" 
           ~ty:Db.Ty.(p1 text) f1
         |> scope.unwrap_with Db.Rc.to_string;
         Db.exec_no_cursor db "attach database ? as db2;" 
           ~ty:Db.Ty.(p1 text) f2
         |> scope.unwrap_with Db.Rc.to_string;
         let provers =
           Db.exec_no_params db
             {| select distinct * from (select prover from db1.prover_res UNION
              select prover from db2.prover_res) ;|}
             ~ty:Db.Ty.(p1 text,id) ~f:Db.Cursor.to_list_rev
           |> scope.unwrap_with Db.Rc.to_string
         in
         Logs.debug (fun k->k "provers: [%s]" (String.concat ";" provers));
         List.map
           (fun prover ->
             let get_n q = 
                Db.exec db q prover
                  ~ty:Db.Ty.(p1 text, p1 int, fun x->x)
                  ~f:(fun c -> scope.unwrap @@
                       match Db.Cursor.next c with
                       | None -> E.fail "expected result" | Some x -> Ok x)
                |> scope.unwrap_with Db.Rc.to_string
             in
             let appeared = get_n
                 {| select count(r2.file) from db2.prover_res r2
                   where r2.prover = ?
                    and not exists (select file from db1.prover_res where
                    db1.prover_res.prover=r2.prover
                    and file = r2.file); |}
             and disappeared = get_n
                 {| select count(r1.file) from db1.prover_res r1
                   where r1.prover = ?
                    and not exists (select file from db2.prover_res where
                    db2.prover_res.prover=r1.prover
                    and file = r1.file); |}
             and same = get_n
                 {| select count(r1.file) from db1.prover_res r1, db2.prover_res r2
                   where
                      r1.prover = ? and r1.prover = r2.prover
                      and r1.file = r2.file
                      and (
                         (r1.res in ('sat', 'unsat') and r1.res = r2.res)
                         or
                         (not (r1.res in ('sat','unsat')) and not (r2.res in ('sat','unsat')))
                      ) ; |}
             and mismatch = get_n
                 {| select count(r1.file) from db1.prover_res r1, db2.prover_res r2
                   where
                      (r1.res in ('sat', 'unsat') or r2.res in ('sat', 'unsat'))
                      and r1.prover = ? and r1.prover = r2.prover
                      and r1.file = r2.file and r1.res != r2.res; |}
             and improved = get_n
                 {| select count(r1.file) from db1.prover_res r1, db2.prover_res r2
                   where not (r1.res in ('sat', 'unsat'))
                      and r2.res in ('sat', 'unsat')
                      and r1.prover = ? and r1.prover = r2.prover
                      and r1.file = r2.file ; |}
             and regressed = get_n
                 {| select count(r1.file) from db1.prover_res r1, db2.prover_res r2
                   where r1.res in ('sat', 'unsat')
                      and not (r2.res in ('sat', 'unsat'))
                      and r1.prover = ? and r1.prover = r2.prover
                      and r1.file = r2.file ; |}
             in
             let c = { appeared; disappeared; same; mismatch; improved; regressed } in
             prover, c)
           provers
      )
end

(* TODO
module Full = struct
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t * float * float) list; (* same result *)
  }

  val to_printbox : t -> PrintBox.t
  val to_printbox_l : (Prover.name * t) list -> PrintBox.t

  val make : filename -> filename -> (Prover.name * t) list or_error
end
*)

(*
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
   *)
