(* This file is free software. See file "license" for more details. *)

(** {1 Definitions} *)

module E = Or_error
module Fmt = CCFormat
module Str_map = Misc.Str_map

type path = string
type 'a or_error = 'a Or_error.t
type 'a with_loc = 'a With_loc.t

open E.Infix

type def =
  | D_prover of Prover.t with_loc
  | D_task of Task.t with_loc

(** All known definitions *)
type t = {
  defs: def Str_map.t;
  dirs: Dir.t list; (* list of directories *)
  cur_dir: string; (* for relative paths *)
  config_file: string option;
  tags: string list;
  option_j : int option;
  option_progress : bool option;
}

let empty : t =
  { defs= Str_map.empty; dirs=[]; cur_dir=Sys.getcwd(); tags=[];
    option_j=None; option_progress=None; config_file=None;
  }

let add_prover (p:Prover.t with_loc) self : t =
  { self with defs=Str_map.add (Prover.name p.view) (D_prover p) self.defs}

let add_task (t:Task.t with_loc) self : t =
  { self with defs=Str_map.add t.view.Task.name (D_task t) self.defs}

let add_dir (d:Dir.t) self : t =
  { self with dirs=d::self.dirs }

let option_j self = self.option_j
let option_progress self = self.option_progress
let custom_tags self = self.tags

module Def = struct
  type t = def
  let loc = function
    | D_prover p -> p.loc
    | D_task t -> t.loc
  let pp out = function
    | D_prover p -> Prover.pp out p.view
    | D_task t -> Task.pp out t.view
  let show = Fmt.to_string pp
end

let to_iter self : _ Iter.t = Str_map.to_iter self.defs
let all_provers self : _ list =
  Str_map.values self.defs
  |> Iter.filter_map (function D_prover p -> Some p | _ -> None)
  |> Iter.to_rev_list

let all_tasks self : _ list =
  Str_map.values self.defs
  |> Iter.filter_map (function D_task t -> Some t | _ -> None)
  |> Iter.to_rev_list

(* compute a version for the prover *)
let get_version ?(binary="") (v:Stanza.version_field) : Prover.version =
  let open Prover in
  begin match v with
    | Stanza.Version_git {dir} ->
      Git {branch=Misc.Git.get_branch dir; commit=Misc.Git.get_commit dir}
    | Stanza.Version_cmd {cmd} ->
      begin try
          Tag (Misc.get_cmd_out @@ Prover.interpolate_cmd cmd
              ~subst:(Prover.subst ~binary ()))
        with Prover.Subst_not_found s ->
          Tag (Printf.sprintf "command `%s` failed: cannot find field %s" cmd s)
      end
    | Stanza.Version_exact v -> v
  end

let find self name = Str_map.get name self.defs

let find_prover self name : Prover.t with_loc or_error =
  match Str_map.get name self.defs with
  | Some (D_prover p) -> Ok p
  | Some _ -> E.failf "%S is not a prover" name
  | _ -> E.failf "prover %S not defined" name

let find_task self name : Task.t with_loc or_error =
  match Str_map.get name self.defs with
  | Some (D_task p) -> Ok p
  | Some _ -> E.failf "%S is not a task" name
  | _ -> E.failf "task %S is not defined" name

let find_prover' self name = E.map With_loc.view @@ find_prover self name
let find_task' self name = E.map With_loc.view @@ find_task self name

let norm_path ~cur_dir s =
  let f s = match s with
    | "cur_dir" -> Some cur_dir
    | _ -> None
  in
  s |> Xdg.interpolate_home ~f |> Misc.mk_abs_path

(* find a known directory for [path] *)
let mk_subdir self path : Subdir.t or_error =
  let path = norm_path ~cur_dir:self.cur_dir path in
  (* helper *)
  let is_parent (dir:string) (f:string) : bool =
    let fd_dir = (Unix.stat dir).Unix.st_dev in
    let same_file f = try (Unix.stat f).Unix.st_dev = fd_dir with _ -> false in
    (* check f and its parents *)
    let rec check f =
      same_file f ||
      (let parent = Filename.dirname f in
       parent <> f && check parent)
    in
    check f
  in
  CCList.find_map
    (fun dir ->
       Logs.debug (fun k->k"check prefix dir=%S for %S" dir.Dir.path path);
       if is_parent dir.Dir.path path
       then Some {Subdir.path; loc=dir.loc; inside=dir}
       else None)
    self.dirs
  |> CCOpt.to_result_lazy
    (fun () -> Error.makef "no known directory contains path %S" path)

let rec conv_expect self = function
  | Stanza.E_const r -> Ok (Dir.E_const r)
  | Stanza.E_program {prover} ->
    let+ p = find_prover self prover in
    Dir.E_program {prover=p.view}
  | Stanza.E_try l ->
    let+ l = E.map_l (conv_expect self) l in
    Dir.E_try l

let mk_limits ?timeout ?memory ?stack () =
  (* Timeouts are expressed in seconds in the config files *)
  let time = CCOpt.map (fun s -> Limit.Time.mk ~s ()) timeout in
  (* Memory limits are expressed in Megabytes in the config files *)
  let memory = CCOpt.map (fun m -> Limit.Memory.mk ~m ()) memory in
  (* Stack sizes are also given in Megabytes *)
  let stack = CCOpt.map (function
    | Stanza.Unlimited -> Limit.Stack.Unlimited
    | Stanza.Limited m -> Limit.Stack.Limited (Limit.Memory.mk ~m ())
    ) stack in
  Limit.All.mk ?time ?memory ?stack ()


let mk_run_provers
    ?j ?timeout ?memory ?stack ?pattern ~paths ~provers ~loc
    (self:t) : _ or_error =
  E.map_l (find_prover' self) provers >>= fun provers ->
  E.map_l (mk_subdir self) paths >>= fun dirs ->
  let limits = mk_limits ?timeout ?memory ?stack () in
  let act = { Action.j; limits; dirs; provers; pattern; loc } in
  Ok act

let rec mk_action (self:t) (a:Stanza.action) : _ or_error =
  match a with
  | Stanza.A_run_provers {provers; memory; dirs; timeout; stack; pattern; loc } ->
    let+ a =
      mk_run_provers
        ?timeout ?memory ?stack ?pattern ~loc:(Some loc) ~paths:dirs ~provers self
    in
    Action.Act_run_provers a
  | Stanza.A_progn l ->
    let+ l = E.map_l (mk_action self) l in
    Action.Act_progn l
  | Stanza.A_git_checkout {dir;ref;fetch_first;loc} ->
    let dir = norm_path ~cur_dir:self.cur_dir dir in
    if Sys.file_exists dir && Sys.is_directory dir then (
      let fetch_first =
        CCOpt.map (function
            | Stanza.GF_fetch -> Action.Git_fetch
            | GF_pull -> Action.Git_pull) fetch_first
      in
      E.return @@ Action.Act_git_checkout {dir; ref; loc; fetch_first}
    ) else (
      E.failf "%s is not an existing directory" (Filename.quote dir)
    )
  | Stanza.A_run_cmd {cmd; loc} ->
    E.return @@ Action.Act_run_cmd {cmd; loc}

(* conversion from stanzas *)
let add_stanza (st:Stanza.t) self : t or_error =
  Logs.info (fun k->k "add-stanza %a" Stanza.pp st);
  let open Stanza in
  match st with
  | St_enter_file file ->
    Ok { self with
         cur_dir=Misc.mk_abs_path (Filename.dirname file);
         config_file=Some file; }
  | St_dir {path;expect;pattern;loc} ->
    let path = norm_path ~cur_dir:self.cur_dir path in
    (if Sys.file_exists path && Sys.is_directory path then Ok ()
     else E.failf ~loc "%S is not a directory" path) >>= fun () ->
    begin match expect with
      | None -> Ok Dir.E_comment
      | Some e -> conv_expect self e
    end >>= fun expect ->
    let d = {Dir.path; expect; loc; pattern} in
    Ok (add_dir d self)
  | St_prover {
      name; cmd; sat; unsat; timeout; unknown; memory;
      version; binary; binary_deps; custom; ulimits; loc;
    } ->
    (* add prover *)
    let cmd = Misc.str_replace ["cur_dir", self.cur_dir] cmd in
    let binary =
      match binary with
      | Some b -> b
      | None ->
        let cmd = String.trim cmd in
        (try fst @@ CCString.Split.left_exn ~by:" " cmd
         with Not_found -> cmd)
    in
    let binary = Misc.str_replace ["cur_dir", self.cur_dir] binary in
    let version = match version with
      | Some v -> v
      | None -> Version_exact (Prover.Tag "<unknown>")
    in
    let ulimits =
      match ulimits with
      | Some l -> l
      | None -> Ulimit.mk ~time:true ~memory:true ~stack:false
    in
    let p = {
      Prover.
      name; cmd; sat; unsat; timeout; unknown; memory; ulimits;
      binary; binary_deps; version=get_version ~binary version;
      custom; defined_in=self.config_file;
    } in
    Ok (add_prover (With_loc.make ~loc p) self)
  | St_task {name; synopsis; action; loc;} ->
    mk_action self action >>= fun action ->
    let t = {Task.name; synopsis; action; defined_in=self.config_file;} in
    Ok (add_task (With_loc.make ~loc t) self)
  | St_set_options {progress; j; loc=_} ->
    let open CCOpt.Infix in
    Ok {self with
        option_j = j <+> self.option_j;
        option_progress = progress <+> self.option_progress;
       }
  | St_declare_custom_tag {tag=t;loc} ->
    if List.mem t self.tags then (
      E.failf ~loc "tag %s already declared" t
    ) else (
      Ok { self with tags = t :: self.tags }
    )

let add_stanza_l (l:Stanza.t list) self : t or_error =
  E.fold_l (fun self st -> add_stanza st self) self l

let of_stanza_l l = add_stanza_l l empty

let completions (self:t) ?before_pos (str:string) : def list =
  to_iter self
  |> Iter.filter_map
    (fun (name,d) ->
       if CCString.prefix ~pre:str name then Some d else None)
  |> (match before_pos with
      | None -> (fun i->i)
      | Some query_pos ->
        Iter.filter
          (fun d ->
             (* keep [d] if it comes before [query_pos] *)
             let loc = Def.loc d in
             Loc.Pos.(loc.stop <= query_pos))
    )
  |> Iter.to_rev_list
