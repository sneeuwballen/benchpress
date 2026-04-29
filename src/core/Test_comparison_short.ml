open Common
open Test
module PB = PrintBox

type single = { better: int; worse: int; same: int }

type t = {
  provers: Prover.name list;
  tbl: (Prover.name * Prover.name * single) list;
}

let of_events (events : Run_event.t list) : t =
  let provers =
    List.filter_map
      (function
        | Run_event.Prover_run r -> Some r.Run_result.program
        | _ -> None)
      events
    |> List.sort_uniq String.compare
  in
  (* problem_name -> prover_name -> Res.t *)
  let by_problem : (string, (string * Res.t) list) Hashtbl.t =
    Hashtbl.create 64
  in
  List.iter
    (function
      | Run_event.Prover_run r ->
        let pb = r.Run_result.problem.Problem.name in
        let prev = Option.value ~default:[] (Hashtbl.find_opt by_problem pb) in
        Hashtbl.replace by_problem pb
          ((r.Run_result.program, r.Run_result.res) :: prev)
      | _ -> ())
    events;
  let is_solved = function
    | Res.Sat | Res.Unsat -> true
    | _ -> false
  in
  let tbl =
    CCList.diagonal provers
    |> List.rev_map (fun (p1, p2) ->
           let better = ref 0 and worse = ref 0 and same = ref 0 in
           Hashtbl.iter
             (fun _pb results ->
               let r1 = List.assoc_opt p1 results in
               let r2 = List.assoc_opt p2 results in
               match r1, r2 with
               | Some res1, Some res2 ->
                 let s1 = is_solved res1 and s2 = is_solved res2 in
                 if s1 && s2 then
                   incr same
                 else if s1 && not s2 then
                   incr better
                 else if (not s1) && s2 then
                   incr worse
               | _ -> ())
             by_problem;
           p1, p2, { better = !better; worse = !worse; same = !same })
  in
  { provers; tbl }

exception Find_int of (int * int * int)

let to_printbox_l_ (self : t) =
  let get_pair p1 p2 =
    try
      List.iter
        (fun (p1', p2', r) ->
          if p1 = p1' && p2 = p2' then
            raise (Find_int (r.better, r.worse, r.same))
          else if p1 = p2' && p2 = p1' then
            raise (Find_int (r.worse, r.better, r.same)))
        self.tbl;
      assert false
    with Find_int x -> x
  in
  let headers =
    PB.text "better:"
    :: List.map (PB.text_with_style PB.Style.bold) self.provers
  in
  let tbl =
    List.map
      (fun p2 ->
        PB.text p2
        :: List.map
             (fun p1 ->
               if p1 = p2 then
                 PB.center_hv @@ PB.text "×"
               else (
                 let bet, worse, same = get_pair p1 p2 in
                 (* TODO: URL for detailed comparison p1 vs p2 *)
                 let tsf s x = PB.text @@ Printf.sprintf s x in
                 PB.center_hv
                 @@ PB.vlist ~bars:false
                      [
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
  if List.length self.provers >= 2 then
    to_printbox_l_ self
  else
    PB.empty
(* nothing interesting *)
