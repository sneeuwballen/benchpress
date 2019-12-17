
module Fmt = CCFormat
module E = CCResult
module T = Test
type 'a or_error = ('a, string) E.t
type path = string

(** {1 Actions} *)

type run_provers = {
  j: int option; (* number of concurrent processes *)
  dirs: Subdir.t list; (* list of directories to examine *)
  provers: Prover.t list;
  pattern: string option;
  timeout: int option;
  memory: int option;
}

(** An action to perform *)
type t =
  | Act_run_provers of run_provers

let pp_run_provers out (self:run_provers) =
  let open Misc.Pp in
  let {dirs; provers; timeout; memory; j; pattern; } = self in
  Fmt.fprintf out "(@[<v1>run_provers%a%a%a%a%a%a@])"
    (pp_f "dirs" (pp_l Subdir.pp)) dirs
    (pp_f "provers" (pp_l Prover.pp_name)) provers
    (pp_opt "pattern" pp_regex) pattern
    (pp_opt "timeout" Fmt.int) timeout
    (pp_opt "memory" Fmt.int) memory
    (pp_opt "j" Fmt.int) j

let pp out (self:t) : unit =
  match self with
  | Act_run_provers a -> pp_run_provers out a

(** {2 Run Provers on Problems} *)

module Exec_run_provers : sig
  type t = run_provers

  type expanded = {
    j: int;
    problems: Problem.t list;
    provers: Prover.t list;
    timeout: int;
    memory: int;
  }

  val expand : 
    ?j:int ->
    ?timeout:int ->
    ?memory:int ->
    t -> expanded or_error

  val run :
    ?timestamp:float ->
    ?uuid:Uuidm.t ->
    ?on_start:(expanded -> unit) ->
    ?on_solve:(Test.result -> unit) ->
    ?on_done:(Test.top_result -> unit) ->
    expanded ->
    Test.top_result or_error
  (** Run the given prover(s) on the given problem set, obtaining results
      after all the problems have been dealt with.
      @param on_solve called whenever a single problem is solved
      @param on_done called when the whole process is done
  *)
end = struct
  open E.Infix

  type t = run_provers
  let (>?) a b = match a with None -> b | Some x -> x
  let (>??) a b = match a with None -> b | Some _ as x -> x

  type expanded = {
    j: int;
    problems: Problem.t list;
    provers: Prover.t list;
    timeout: int;
    memory: int;
  }

  let filter_regex_ = function
    | None -> (fun _ -> true)
    | Some re ->
      let re = Re.Perl.compile_pat re in
      (fun path -> Re.execp re path)

  (* turn a subdir into a list of problems *)
  let expand_subdir ?pattern (s:Subdir.t) : Problem.t list or_error =
    try
      let filter1 = filter_regex_ s.Subdir.inside.pattern in
      let filter2 = filter_regex_ pattern in
      let filter s = filter1 s && filter2 s in
      CCIO.File.walk_l s.Subdir.path
      |> CCList.filter_map
        (fun (kind,f) -> match kind with
           | `File when filter f -> Some f
           | _ -> None)
      |> Misc.Par_map.map_p ~j:3
        (fun path ->
           Problem.make_find_expect path ~expect:s.Subdir.inside.expect)
      |> E.flatten_l
    with e ->
      E.of_exn_trace e |> E.add_ctxf "expand_subdir of_dir %a" Subdir.pp s

  (* Expand options into concrete choices *)
  let expand ?j ?timeout ?memory (self:t) : expanded or_error =
    let j = j >?? self.j >? Misc.guess_cpu_count () in
    let timeout = timeout >?? self.timeout >? 60 in
    let memory = memory >?? self.memory >? 1_000 in
    E.map_l expand_subdir self.dirs >>= fun problems ->
    let problems = CCList.flatten problems in
    Ok { j; memory; timeout; problems; provers=self.provers; }

  let _nop _ = ()

  let run ?(timestamp=Unix.gettimeofday())
      ?(uuid=Misc.mk_uuid())
      ?(on_start=_nop) ?(on_solve = _nop) ?(on_done = _nop)
      (self:expanded) : _ E.t =
    let open E.Infix in
    let start = Unix.gettimeofday() in
    on_start self;
    (* build list of tasks *)
    let jobs =
      CCList.flat_map
        (fun pb -> List.map (fun prover -> prover,pb) self.provers)
        self.problems
    in
    (* run provers *)
    begin
      Misc.Par_map.map_p ~j:self.j
        (fun (prover,pb) ->
          begin
            Run_prover_problem.run
              ~timeout:self.timeout ~memory:self.memory
              prover pb >|= fun result ->
            on_solve result; (* callback *)
            result
          end
          |> E.add_ctxf "(@[running :prover %a :on %a@])"
            Prover.pp_name prover Problem.pp pb)
        jobs
      |> E.flatten_l
    end
    >>= fun res_l ->
    let total_wall_time =  Unix.gettimeofday() -. start in
    let r = T.Top_result.make ~timestamp ~total_wall_time ~uuid res_l in
    E.iter on_done r;
    r
end

