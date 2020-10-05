(* This file is free software. See file "license" for more details. *)

(* A type to state which limits should be enforced by ulimit *)
type conf = {
  time : bool;
  memory : bool;
  stack : bool;
}

(* Creation *)
let mk ~time ~memory ~stack =
  { time; memory; stack; }

(* Usual functions *)
let hash = Hashtbl.hash
let compare (x : conf) y = compare x y
let equal x y = compare x y = 0

(* Printing *)
let pp out t =
  if not t.time && not t.memory && not t.stack then
    CCFormat.fprintf out "none"
  else (
    CCFormat.fprintf out "(%a%a%a)"
      CCFormat.string (if t.time then "time " else "")
      CCFormat.string (if t.memory then "memory " else "")
      CCFormat.string (if t.stack then "stack" else "")
  )

(* Make a command to enforce a set of limits *)
let cmd ~conf ~limits =
  if not conf.time && not conf.memory && not conf.stack then (
    None
  ) else (
    let buf = Buffer.create 32 in
    let subst s =
      (* this should be safe as we only use pattern recognized
         by the Limit.All.substitute, hence it should never return
         None *)
      match Limit.All.substitute
        ~memory_as:Megabytes
        ~time_as:Seconds
        ~stack_as:Megabytes
        limits s
      with
      | Some x -> x
      | None -> failwith (Printf.sprintf "cannot substitute in %S" s)
    in
    let add_str s = Buffer.add_substitute buf subst s in
    Buffer.add_string buf "ulimit ";
    if conf.time && CCOpt.is_some limits.time then add_str "-t $timeout ";
    if conf.memory && CCOpt.is_some limits.memory then add_str "-Sm $memory ";
    if conf.stack && CCOpt.is_some limits.stack then add_str "-s $stack ";
    Some (Buffer.contents buf)
  )

let prefix_cmd ?prefix ~cmd =
  match prefix with
  | None -> cmd
  | Some s -> s ^ "; " ^ cmd

