
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

