(* This file is free software. See file "license" for more details. *)

(** {1 Definitions} *)

open Common
module Str_map = Misc.Str_map
module Log = (val Logs.src_log (Logs.Src.create "benchpress.definition"))

type path = string
type 'a with_loc = 'a With_loc.t

type def =
  | D_prover of Prover.t with_loc
  | D_task of Task.t with_loc
  | D_proof_checker of Proof_checker.t with_loc

type t = {
  defs: def Str_map.t;
  dirs: Dir.t list; (* list of directories *)
  dir_vars: path Str_map.t; (* named directories *)
  errors: Error.t list;
  cur_dir: string; (* for relative paths *)
  config_file: string option;
  tags: string list;
  option_j: int option;
  option_progress: bool option;
}
(** All known definitions *)

let empty : t =
  {
    defs = Str_map.empty;
    dirs = [];
    dir_vars = Str_map.empty;
    cur_dir = Sys.getcwd ();
    tags = [];
    option_j = None;
    option_progress = None;
    config_file = None;
    errors = [];
  }

let add_prover (p : Prover.t with_loc) self : t =
  { self with defs = Str_map.add (Prover.name p.view) (D_prover p) self.defs }

let add_proof_checker (p : Proof_checker.t with_loc) self : t =
  { self with defs = Str_map.add p.view.name (D_proof_checker p) self.defs }

let add_task (t : Task.t with_loc) self : t =
  { self with defs = Str_map.add t.view.Task.name (D_task t) self.defs }

let add_dir (d : Dir.t) self : t =
  let dir_vars =
    match d.name with
    | Some name -> Str_map.add ("dir:" ^ name) d.path self.dir_vars
    | None -> self.dir_vars
  in
  { self with dirs = d :: self.dirs; dir_vars }

let errors self = self.errors
let option_j self = self.option_j
let option_progress self = self.option_progress
let custom_tags self = self.tags

module Def = struct
  type t = def

  let loc = function
    | D_prover p -> p.loc
    | D_task t -> t.loc
    | D_proof_checker p -> p.loc

  let pp out = function
    | D_prover p -> Prover.pp out p.view
    | D_task t -> Task.pp out t.view
    | D_proof_checker p -> Proof_checker.pp out p.view

  let show = Fmt.to_string pp
end

let to_iter self : _ Iter.t = Str_map.to_iter self.defs

let all_provers self : _ list =
  Str_map.values self.defs
  |> Iter.filter_map (function
       | D_prover p -> Some p
       | _ -> None)
  |> Iter.to_rev_list

let all_checkers self : _ list =
  Str_map.values self.defs
  |> Iter.filter_map (function
       | D_proof_checker c -> Some c
       | _ -> None)
  |> Iter.to_rev_list

let all_tasks self : _ list =
  Str_map.values self.defs
  |> Iter.filter_map (function
       | D_task t -> Some t
       | _ -> None)
  |> Iter.to_rev_list

let all_dirs self : Dir.t list = self.dirs
let find self name = Str_map.get name self.defs

let find_prover self name : Prover.t with_loc =
  match Str_map.get name self.defs with
  | Some (D_prover p) -> p
  | Some _ -> Error.failf "%S is not a prover" name
  | _ -> Error.failf "prover %S is not defined" name

let find_checker self name : Proof_checker.t with_loc =
  match Str_map.get name self.defs with
  | Some (D_proof_checker p) -> p
  | Some _ -> Error.failf "%S is not a proof checker" name
  | _ -> Error.failf "proof checker %S is not defined" name

let find_task self name : Task.t with_loc =
  match Str_map.get name self.defs with
  | Some (D_task p) -> p
  | Some _ -> Error.failf "%S is not a task" name
  | _ -> Error.failf "task %S is not defined" name

let find_prover' self name = With_loc.view @@ find_prover self name
let find_task' self name = With_loc.view @@ find_task self name

let norm_path ?(dir_vars = Str_map.empty) ~cur_dir s =
  let f s =
    match s with
    | "cur_dir" -> Some cur_dir
    | _ -> Str_map.find_opt s dir_vars
  in
  s |> Xdg.interpolate_home ~f |> Misc.mk_abs_path

(* find a known directory for [path] *)
let mk_subdir self path : Subdir.t =
  let path = norm_path ~dir_vars:self.dir_vars ~cur_dir:self.cur_dir path in
  (* helper *)
  let is_parent (dir : string) (f : string) : bool =
    let dir_st = Unix.(stat dir) in
    let same_file f =
      try
        let f_st = Unix.(stat f) in
        dir_st.st_ino = f_st.st_ino && dir_st.st_dev = f_st.st_dev
      with Unix.Unix_error _ -> false
    in
    (* check f and its parents *)
    let rec check f =
      same_file f
      ||
      let parent = Filename.dirname f in
      parent <> f && check parent
    in
    check f
  in
  CCList.find_map
    (fun dir ->
      Logs.debug (fun k -> k "check prefix dir=%S for %S" dir.Dir.path path);
      if is_parent dir.Dir.path path then
        Some { Subdir.path; loc = dir.loc; inside = dir }
      else
        None)
    self.dirs
  |> Error.unwrap_opt' (fun () ->
         spf "no known directory contains path %S" path)

let mk_limits ?timeout ?memory () =
  let time = CCOpt.map (fun s -> Limit.Time.mk ~s ()) timeout in
  let memory = CCOpt.map (fun m -> Limit.Memory.mk ~m ()) memory in
  Limit.All.mk ?time ?memory ()

(* Add lines from [files] to [dirs] *)
let mk_paths ?(dir_files = []) dirs =
  List.fold_left
    (fun dirs f ->
      let f_lines = CCIO.with_in f CCIO.read_lines_l in
      List.rev_append f_lines dirs)
    dirs dir_files

let mk_run_provers ?j ?timeout ?memory ?pattern ~paths ~provers ~loc (self : t)
    : Action.run_provers =
  let provers = CCList.map (find_prover' self) provers in
  let dirs = CCList.map (mk_subdir self) paths in
  let limits = mk_limits ?timeout ?memory () in
  Action.{ j; limits; dirs; provers; pattern; loc }

let mk_run_provers_slurm_submission ?j ~paths ?timeout ?memory ?pattern ~provers
    ?loc ?partition ?nodes ?addr ?port ?ntasks (self : t) :
    Action.run_provers_slurm_submission =
  let ge_val opt min def =
    match opt with
    | Some v when v >= min -> v
    | _ -> def
  in
  let provers = CCList.map (find_prover' self) provers in
  let dirs = CCList.map (mk_subdir self) paths in
  let limits = mk_limits ?timeout ?memory () in
  let nodes = ge_val nodes 1 1 in
  let addr = CCOpt.value addr ~default:(Misc.localhost_addr ()) in
  let port = ge_val port 0 0 in
  let j =
    match j with
    | Some v when v > 0 -> j
    | _ -> None
  in
  let ntasks = ge_val ntasks 1 10 in
  {
    partition;
    nodes;
    j;
    addr;
    port;
    ntasks;
    provers;
    dirs;
    pattern;
    limits;
    loc;
  }

let completions (self : t) ?before_pos (str : string) : def list =
  to_iter self
  |> Iter.filter_map (fun (name, d) ->
         if CCString.prefix ~pre:str name then
           Some d
         else
           None)
  |> (match before_pos with
     | None -> fun i -> i
     | Some query_pos ->
       Iter.filter (fun d ->
           (* keep [d] if it comes before [query_pos] *)
           let loc = Def.loc d in
           Loc.Pos.le loc.input loc.stop query_pos))
  |> Iter.to_rev_list

let pp_def out = function
  | D_prover { view = p; _ } -> Fmt.fprintf out "%a" Prover.pp p
  | D_task { view = t; _ } -> Fmt.fprintf out "%a" Task.pp t
  | D_proof_checker { view = pc; _ } -> Fmt.fprintf out "%a" Proof_checker.pp pc

let pp out
    {
      defs;
      dirs;
      dir_vars;
      errors;
      cur_dir;
      config_file;
      tags;
      option_j;
      option_progress;
    } =
  let open Misc.Pp in
  Fmt.fprintf out "(@[<v1>Definitions%a%a%a%a%a%a%a%a%a@])"
    (pp_f "def" (Str_map.pp Fmt.string pp_def))
    defs
    (pp_f "dirs" (pp_l Dir.pp))
    dirs
    (pp_f "dir_vars" (Str_map.pp Fmt.string Fmt.string))
    dir_vars
    (pp_f "dirs" (pp_l Error.pp))
    errors
    (pp_f "cur_dir" Fmt.string)
    cur_dir
    (pp_opt "config_file" Fmt.string)
    config_file
    (pp_f "tags" (pp_l Fmt.string))
    tags
    (pp_opt "option_j" Fmt.int)
    option_j
    (pp_opt "option_progress" Fmt.bool)
    option_progress
