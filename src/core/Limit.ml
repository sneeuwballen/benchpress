(* This file is free software. See file "license" for more details. *)


(* Time limits aka Timeouts *)
module Time = struct

  (* Timeout are stored in **seconds** *)
  type t = int

  (* The view of a time *)
  type view =
    | Seconds
    | Minutes
    | Hours (**)

  (* useful constants *)
  let t_min = 60
  let t_hour = 60 * t_min

  (* Usual functions *)
  let hash = Hashtbl.hash
  let compare (x : t) y = compare x y
  let equal x y = compare x y = 0

  (* printing *)
  let pp out t =
    CCFormat.fprintf out "%ds" t

  (* Creation *)
  let mk ?(s=0) ?(m=0) ?(h=0) () =
    s + m * t_min + h * t_hour

  let add x y = x + y

  (* Int (i.e. partial) View *)
  let as_int v t =
    match v with
    | Seconds -> t
    | Minutes -> t / t_min
    | Hours -> t / t_hour

  (* Float view (more precise) *)
  let as_float v t =
    match v with
    | Seconds -> float t
    | Minutes -> float t /. float t_min
    | Hours -> float t /. float t_hour

end



(* Memory limits *)
module Memory = struct

  (* Memory limits are stored in number of bytes *)
  type t = int

  (* View of a memory limits *)
  type view =
    | Bytes
    | Kilobytes
    | Megabytes
    | Gigabytes
    | Terabytes

  (* useful constants *)
  let s_k = 1_000
  let s_m = 1_000 * s_k
  let s_g = 1_000 * s_m
  let s_t = 1_000 * s_g

  (* Usual functions *)
  let hash = Hashtbl.hash
  let compare (x : t) y = compare x y
  let equal x y = compare x y = 0

  (* printing *)
  let pp out n =
    let aux n div = n / div, n mod div in
    let n_tera, n = aux n s_t in
    let n_giga, n = aux n s_g in
    let n_mega, n = aux n s_m in
    let n_kilo, n_bytes = aux n s_k in
    let print_aux s n =
      if n = 0 then ()
      else CCFormat.fprintf out "%d%s" n s
    in
    print_aux "T" n_tera;
    print_aux "G" n_giga;
    print_aux "M" n_mega;
    print_aux "k" n_kilo;
    print_aux "" n_bytes

  (* Creation *)
  let mk ?(b=0) ?(k=0) ?(m=0) ?(g=0) ?(t=0) () =
    b + k * s_k + m * s_m + g * s_g + t * s_t

  (* Int View *)
  let as_int v t =
    match v with
    | Bytes -> t
    | Kilobytes -> t / s_k
    | Megabytes -> t / s_m
    | Gigabytes -> t / s_g
    | Terabytes -> t / s_t

  (* Access as floats *)
  let as_float v t =
    match v with
    | Bytes -> float t
    | Kilobytes -> float t /. float s_k
    | Megabytes -> float t /. float s_m
    | Gigabytes -> float t /. float s_g
    | Terabytes -> float t /. float s_t


end

(* Stack size limit *)
module Stack = struct

  type t =
    | Unlimited
    | Limited of Memory.t

  (* Usual functions *)
  let hash = Hashtbl.hash
  let compare (x : t) y = compare x y
  let equal x y = compare x y = 0

  (* Printing *)
  let pp out = function
    | Unlimited -> CCFormat.fprintf out "unlimited"
    | Limited m -> Memory.pp out m

end

(* Structure to combine all currently supported limits *)
module All = struct

  (* Useful record to represent a complet set of limits for one run *)
  type t = {
    time : Time.t option;
    memory : Memory.t option;
    stack : Stack.t option;
  }

  (* Usual functions *)
  let hash = Hashtbl.hash
  let compare (x : t) y = compare x y
  let equal x y = compare x y = 0

  (* Printing *)
  let pp out t =
    CCFormat.fprintf out "(@[<v 1>limits%a%a%a@])"
      (Misc.Pp.pp_opt "timeout" Time.pp) t.time
      (Misc.Pp.pp_opt "memory" Memory.pp) t.memory
      (Misc.Pp.pp_opt "stack" Stack.pp) t.stack

  (* Creation *)
  let mk ?time ?memory ?stack () =
    { time; memory; stack; }

  let update_time f t = { t with time = f t.time }
  let update_memory f t = { t with memory = f t.memory }
  let update_stack f t = { t with stack = f t.stack }


  (* Combination of limits *)
  let with_default ~default = function
    | None -> default
    | Some _ as res -> res

  let with_defaults ~defaults t =
    update_time (with_default ~default:defaults.time) @@
    update_memory (with_default ~default:defaults.memory) @@
    update_stack (with_default ~default:defaults.stack) @@ t

  (* Exception for a missing limit *)
  exception Limit_missing of string

  (* Substitution of limits in a buffer *)
  let subst_aux s to_string = function
    | Some x -> Some (to_string x)
    | None -> raise (Limit_missing s)

  let substitute ~time_as ~memory_as ~stack_as t s =
    match s with
    | "memory" ->
      subst_aux s CCFun.(Memory.as_int memory_as %> string_of_int) t.memory
    | "timeout" | "time" ->
      subst_aux s CCFun.(Time.as_int time_as %> string_of_int) t.time
    | "stack" ->
      subst_aux s (function
          | Stack.Unlimited -> "unlimited"
          | Stack.Limited m -> string_of_int @@ Memory.as_int stack_as m
        ) t.stack
    | _ -> None

end


