(* This file is free software. See file "license" for more details. *)

(** {1 Run Prover} *)

module Fmt = CCFormat

type version =
  | Tag of string
  | Git of {
      branch: string;
      commit: string;  (* branch & commit hash *)
    }

type t = {
  (* Prover identification *)
  name : string;
  version : version;

  (* Pover execution *)
  binary: string;       (* name of the program itself *)
  binary_deps: (string list [@default []]); (* additional list of binaries this depends on *)
  cmd: string;          (* the command line to run.
                           possibly contains $binary, $file, $memory and $timeout *)

  (* Result analysis *)
  unsat   : string option;  (* regex for "unsat" *)
  sat     : string option;  (* regex for "sat" *)
  unknown : string option;  (* regex for "unknown" *)
  timeout : string option;  (* regex for "timeout" *)
  memory  : string option;  (* regex for "out of memory" *)
}

type t_ = t

let equal p1 p2 = p1.name = p2.name

let version_to_string = function
  | Tag s -> s
  | Git {branch=b; commit=c} -> Printf.sprintf "%s#%s" b c

let name p = p.name

let pp_name out p = Fmt.string out p.name
let pp_version out =
  let open Misc.Pp in
  function
  | Tag s -> Fmt.fprintf out "(tag %a)" pp_str s
  | Git {branch=b; commit=c} ->
    Fmt.fprintf out "(@[git@ branch=%a@ commit=%a@])" pp_str b pp_str c

let pp out self =
  let open Misc.Pp in
  let {name; version; cmd; unsat; sat; timeout; unknown; memory;
       binary; binary_deps=_;} = self in
  Fmt.fprintf out
    "(@[<hv1>prover%a%a%a%a%a%a%a%a%a@])"
    (pp_f "name" pp_str) name
    (pp_f "version" pp_version) version
    (pp_f "cmd" pp_str) cmd
    (pp_f "binary" pp_str) binary
    (pp_opt "sat" pp_regex) sat
    (pp_opt "unsat" pp_regex) unsat
    (pp_opt "memory" pp_regex) memory
    (pp_opt "timeout" pp_regex) timeout
    (pp_opt "unknown" pp_regex) unknown

exception Subst_not_found of string

let interpolate_cmd
    ?(env=[||])
    ?(binary="")
    ?(timeout=1)
    ?(memory=5_000_000)
    ?(file="")
    ?(f=fun _->None)
    cmd =
  let buf = Buffer.create 32 in
  let add_str s =
    Buffer.add_substitute buf
      (function
        | "memory" -> string_of_int memory
        | "timeout" | "time" -> string_of_int timeout
        | "file" -> file
        | "binary" -> binary
        | s ->
          match f s with
          | Some u -> u
          | None -> raise (Subst_not_found s))
      s
  in
  add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1200 * $memory )); ";
  Array.iter
    (fun (key,value) -> add_str (key ^ "=" ^ value ^ " "))
    env;
  add_str cmd;
  Buffer.contents buf

let make_command ?env prover ~timeout ~memory ~file =
  let binary = prover.binary in
  try interpolate_cmd ?env ~binary ~timeout ~memory ~file prover.cmd
  with Subst_not_found s ->
    failwith (Printf.sprintf
        "cannot make command for prover %s: cannot find field %s" prover.name s)

module Map_name = CCMap.Make(struct
    type t = t_
    let compare a b = String.compare a.name b.name
  end)

module As_key = struct
  type t = t_

  let compare p1 p2 =
    let c = String.compare p1.name p2.name in
    if c<>0 then c else Pervasives.compare p1.version p2.version
end

module Map = CCMap.Make(As_key)
module Set = CCSet.Make(As_key)
    
module J = Misc.Json

let encode_version v =
  let open J.Encode in
  match v with
  | Tag s -> list string ["tag"; s]
  | Git {branch;commit} ->
    list string ["git"; branch; commit]

let decode_version =
  let open J.Decode in
  string >>:: function
  | "tag" -> (list1 string >|= fun s -> Tag s)
  | "git" ->
    (list string >>= function
    | [branch;commit] -> succeed (Git {branch;commit})
    | _ -> fail "need 2 arguments")
  | _ -> fail "unknown constructor, expect tag/git"

let encode p =
  let open J.Encode in
  let {
    name;version;binary;binary_deps;cmd;
    sat; unsat; unknown; timeout; memory;
  } = p in
  obj [
    "name", string name;
    "version", encode_version version;
    "binary", string binary;
    "binary_deps", list string binary_deps;
    "cmd", string cmd;
    "unsat", option string unsat;
    "sat", option string sat;
    "unknown", option string unknown;
    "timeout", option string timeout;
    "memory", option string memory;
  ]

let decode =
  let open J.Decode in
  let f_opt_null f_name =
    field_opt f_name (nullable string) >|= CCOpt.flatten
  in
  field "name" string >>= fun name ->
  field "cmd" string >>= fun cmd ->
  field "binary" string >>= fun binary ->
  field "binary_deps" (list string) >>= fun binary_deps ->
  field "version" decode_version >>= fun version ->
  f_opt_null "unsat" >>= fun unsat ->
  f_opt_null "sat" >>= fun sat ->
  f_opt_null "unknown" >>= fun unknown ->
  f_opt_null "timeout" >>= fun timeout ->
  f_opt_null "memory" >>= fun memory ->
  succeed {
    name; cmd; binary; binary_deps; version;
    sat; unsat; unknown; timeout; memory;
  }

let run_proc cmd =
  let start = Unix.gettimeofday () in
  (* call process and block *)
  let p = try CCUnix.call_full "%s" cmd
    with e ->
      object
        method stdout=""
        method stderr="<process died: " ^ Printexc.to_string e
        method errcode=1
        method status = Unix.WEXITED 1
      end
  in
  let errcode = p#errcode in
  Misc.Debug.debugf 5
    (fun k->k "(@[prover.run.done errcode: %d@ cmd %a@]" errcode Misc.Pp.pp_str cmd);
  (* Compute time used by the prover *)
  let rtime = Unix.gettimeofday () -. start in
  let utime = 0. in
  let stime = 0. in
  let stdout = p#stdout in
  let stderr = p#stderr in
  Misc.Debug.debugf 10
    (fun k->k "stdout:\n%s\nstderr:\n%s" stdout stderr);
  { Proc_run_result. stdout; stderr; errcode; rtime; utime; stime; }

let run ?env ~timeout ~memory ~file (self:t) : Proc_run_result.t =
  Misc.Debug.debugf 5
    (fun k->k "(@[Prover.run %s timeout: %d, memory: %d@])" self.name timeout memory);
  (* limit time and memory ('v' is virtual memory, needed because 'm' ignored on linux) *)
  let memory' = memory * 1000 in
  let memory_v = memory * 1000 * 8 in (* give 8 times more virtual mem, for JVM *)
  let prefix =
    Printf.sprintf "ulimit -t %d -m %d -Sv %d; " timeout memory' memory_v
  in
  let cmd = make_command ?env self ~timeout ~memory ~file in
  let cmd = prefix ^ cmd in
  run_proc cmd

let analyze_p_opt (self:t) (r:Proc_run_result.t) : Res.t option =
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re ->
      let re = Re.Perl.compile_pat ~opts:[`Multiline] re in
      Re.execp re r.stdout ||
      Re.execp re r.stderr
  in
  if find_opt_ self.sat then Some Res.Sat
  else if find_opt_ self.unsat then Some Res.Unsat
  else if find_opt_ self.timeout then Some Res.Timeout
  else if find_opt_ self.unknown then Some Res.Unknown
  else None
