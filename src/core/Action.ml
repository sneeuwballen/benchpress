open Common
module T = Test

type path = string

(** {1 Actions} *)

type run_provers = {
  j: int option; (* number of concurrent processes *)
  dirs: Subdir.t list; (* list of directories to examine *)
  provers: Prover.t list;
  pattern: string option;
  limits: Limit.All.t;
  loc: Loc.t option;
}

type run_provers_slurm_submission = {
  partition: string option;
  (* The partition to which the allocated nodes should belong. *)
  nodes: int;
  (* the maximum number of nodes that can be allocated for the job.
     One worker will run per node *)
  addr: Unix.inet_addr;
  (* IP address of the server on the control node.
     Needs to be reachable by the workers which will run on the allocated calculation nodes. *)
  port: int;
  (* port of the server in the control node. *)
  j: int option;
  (* number of parallel threads that will be launched by the workers.
     Default is the number of processors on the node. *)
  ntasks: int;
  (* The number of tasks to give the workers at a time.
     Default is equal to the number of threads. *)
  dirs: Subdir.t list; (* list of problems *)
  provers: Prover.t list;
  pattern: string option;
  limits: Limit.All.t;
  loc: Loc.t option;
}

type git_fetch_action = Git_fetch | Git_pull

type git_checkout = {
  dir: string;
  ref: string;
  fetch_first: git_fetch_action option;
  loc: Loc.t;
}

(** An action to perform *)
type t =
  | Act_run_provers of run_provers
  | Act_run_slurm_submission of run_provers_slurm_submission
  | Act_git_checkout of git_checkout
  | Act_run_cmd of { cmd: string; loc: Loc.t }
  | Act_progn of t list

let pp_run_provers out (self : run_provers) =
  let open Misc.Pp in
  let ({ dirs; provers; limits; j; pattern; loc = _ } : run_provers) = self in
  Fmt.fprintf out "(@[<v1>run_provers%a%a%a%a%a%a%a@])"
    (pp_f "dirs" (pp_l Subdir.pp))
    dirs
    (pp_f "provers" (pp_l Prover.pp_name))
    provers
    (pp_opt "pattern" pp_regex)
    pattern
    (pp_opt "timeout" Limit.Time.pp)
    limits.time
    (pp_opt "memory" Limit.Memory.pp)
    limits.memory
    (pp_opt "stack" Limit.Stack.pp)
    limits.stack (pp_opt "j" Fmt.int) j

let pp_run_provers_slurm out (self : run_provers_slurm_submission) =
  let open Misc.Pp in
  let {
    partition;
    nodes;
    j;
    dirs;
    provers;
    pattern;
    limits;
    addr;
    port;
    ntasks;
    loc = _;
  } =
    self
  in
  Fmt.fprintf out "(@[<v1>run_provers.Slurm%a%a%a%a%a%a%a%a%a%a%a%a@])"
    (pp_opt "partition" Fmt.string)
    partition (pp_f "nodes" Fmt.int) nodes
    (pp_f "addr" Misc.pp_inet_addr)
    addr (pp_f "port" Fmt.int) port (pp_f "ntasks" Fmt.int) ntasks
    (pp_opt "j" Fmt.int) j
    (pp_f "dirs" (pp_l Subdir.pp))
    dirs
    (pp_f "provers" (pp_l Prover.pp_name))
    provers
    (pp_opt "pattern" pp_regex)
    pattern
    (pp_opt "timeout" Limit.Time.pp)
    limits.time
    (pp_opt "memory" Limit.Memory.pp)
    limits.memory
    (pp_opt "stack" Limit.Stack.pp)
    limits.stack

let pp_git_fetch out = function
  | Git_fetch -> Fmt.string out "fetch"
  | Git_pull -> Fmt.string out "pull"

let pp_git_checkout out (self : git_checkout) =
  let open Misc.Pp in
  let { dir; ref; fetch_first; loc = _ } = self in
  Fmt.fprintf out "(@[<v1>git-checkout%a%a%a@])" (pp_f "dir" pp_regex) dir
    (pp_f "ref" pp_regex) ref
    (pp_opt "fetch-first" pp_git_fetch)
    fetch_first

let rec pp out (self : t) : unit =
  match self with
  | Act_run_provers a -> pp_run_provers out a
  | Act_run_slurm_submission a -> pp_run_provers_slurm out a
  | Act_git_checkout g -> pp_git_checkout out g
  | Act_run_cmd { cmd = s; loc = _ } -> Fmt.fprintf out "(run-cmd %S)" s
  | Act_progn l -> Fmt.fprintf out "(@[%a@])" (Misc.Pp.pp_l pp) l
