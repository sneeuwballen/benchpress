open Common

module S = Sexp_loc
type sexp = Sexp_loc.t

type err = Err of {
    msg: (unit -> string);
    value: sexp;
    ctx_of: err option;
  }

type +'a t = {
  run: sexp -> ('a, err) result;
} [@@unboxed]
type 'a m = 'a t

let[@inline] const x = fun _ -> x
let spf = Printf.sprintf

let return x = {run=fun _ -> Ok x}

let fail_ ?ctx_of str sexp = Error (Err {msg=str; value=sexp; ctx_of})

let fail str = {
  run=fun s -> fail_ (const str) s
}

let failf k = {
  run=fun s ->
    fail_ (fun () -> k (fun fmt -> Fmt.asprintf fmt)) s
}

let value = {run=fun s->Ok s}
let value_loc = {run=fun s->Ok s.S.loc}
let atom = {run=fun s ->
    match s.S.view with
    | S.Atom x -> Ok x
    | S.List _ -> fail_ (const "expected an atom") s
  }
let string = atom

let int = {
  run=fun s ->
    match s.S.view with
    | Atom x -> (try Ok (int_of_string x) with _ -> fail_ (const "expected an integer") s)
    | _ -> fail_ (const "expected an integer") s
}

let bool = {
  run=fun s ->
    match s.S.view with
    | Atom x -> (try Ok (bool_of_string x) with _ -> fail_ (const "expected a bool") s)
    | _ -> fail_ (const "expected a bool") s
}

let list = {run=fun s ->
    match s.S.view with
    | S.List l -> Ok l
    | S.Atom _ -> fail_ (const "expected a list") s
  }

let list_of ?what d =
  let exception E of err in
  { run=
      fun s ->
        match s.view with
        | List l ->
          begin
            try
              let l =
                l
                |> List.map (fun u ->
                    match d.run u with
                    | Ok x -> x
                    | Error e -> raise (E e))
              in
              Ok l
            with E e ->
              Error e
          end
        | Atom _ ->
          let msg() = match what with
            | None -> "expected list"
            | Some w -> spf "expected list of %s" w in
          fail_ msg s
  }

let ( let+ ) x f = {
  run=fun s ->
    match x.run s with
    | Error _ as e -> e
    | Ok x2 -> Ok (f x2)
}

let ( let* ) x f = {
  run=fun s ->
    match x.run s with
    | Error _ as e -> e
    | Ok x2 -> (f x2).run s
}

module Infix = struct
  let ( let+ ) = ( let+ )
  let ( >|= ) = ( let+ )
  let ( let* ) = ( let* )
  let ( >>= ) = ( let* )
                end
include Infix

let fix f =
  let rec self = lazy (f (Lazy.force d))
  and d = lazy ({
    run=fun s -> (Lazy.force self).run s
  })
  in
  Lazy.force self

let is_atom = {run=fun s -> match s.S.view with Atom _ -> Ok true | _ -> Ok false}
let is_list = {run=fun s -> match s.S.view with Atom _ -> Ok false | List _ -> Ok true}
let succeeds d = {
  run=fun s -> match d.run s with
    | Ok _ -> Ok true
    | Error _ -> Ok false
}
let is_applied foo = {
  run=fun s ->
    match s.S.view with
    | List ({S.view=Atom x;_} :: _) -> Ok (x = foo)
    | _ -> Ok false
}

let try_succeed d = succeeds d, d

let try_l (type x) ~msg decs = {
  run=fun s ->
    let exception E of x t in
    try
      List.iter
        (fun (check,dec) ->
           match check.run s with
           | Ok false | Error _ -> ()
           | Ok true -> raise_notrace (E dec))
        decs;
      (* no decoder found *)
      fail_ (const msg) s
    with E dec ->
      begin match dec.run s with
        | Ok _ as x -> x
        | Error e ->
          (* wrap error *)
          fail_ (const msg) s ~ctx_of:e
      end
}

let with_msg ~msg d = {
  run=fun s ->
    match d.run s with
    | Ok _ as x -> x
    | Error (Err r) -> Error (Err {r with msg=const msg})
}

let sub self s = {
  run=fun _ -> self.run s
}

let pair a b =
  let* l = list_of value in
  match l with
  | [x; y] ->
    let* x = sub a x in
    let+ y = sub b y in
    x,y
  | _ -> fail "expected a pair"

let try_apply name f else_ = {
  run=fun s ->
    match s.S.view with
    | S.List ({S.view=S.Atom name2;_} :: args) when name=name2 ->
      (f args).run s
    | _ -> else_.run s
}

let applied name d = {
  run=fun s ->
    match s.S.view with
    | List ({S.view=S.Atom name';_} :: l) ->
      if name=name' then (
        let exception E of err in
        try
          Ok (List.map
                (fun u -> match d.run u with Ok x -> x | Error e -> raise (E e))
                l)
        with E err ->
          Error err
      ) else (
        fail_ (fun() -> spf "expected (%s …) but got (%s …)" name name') s
      )
    | _ ->
        fail_ (fun() -> spf "expected (%s …)" name) s
}

let applied0 name =
  let* l = applied name value in
  match l with
  | [] -> return ()
  | _ -> failf (fun k -> k"expected (%s) but got argument(s)" name)

let applied1 name d = {
  run=fun s ->
    match s.S.view with
    | List [{S.view=S.Atom name';_}; x] ->
      if name=name' then (
        d.run x
      ) else (
        fail_ (fun() -> spf "expected (%s _) but got (%s _)" name name') s
      )
    | _ ->
        fail_ (fun() -> spf "expected (%s _)" name) s
}

let applied2 name d1 d2 =
  let* l = applied name value in
  match l with
  | [x;y] ->
    let* x = sub d1 x in
    let+ y = sub d2 y in
    x,y
  | _ -> failf (fun k->k"expected (%s _ _)" name)

let applied3 name d1 d2 d3 =
  let* l = applied name value in
  match l with
  | [x;y;z] ->
    let* x = sub d1 x in
    let* y = sub d2 y in
    let+ z = sub d3 z in
    x,y,z
  | _ -> failf (fun k->k"expected (%s _ _)" name)

let atom_or_atom_list = {
  run=fun s ->
    let exception E of sexp in
    match s.S.view with
    | Atom x -> Ok [x]
    | List l ->
      try
        Ok (List.map (function {S.view=Atom u;_} -> u | u -> raise (E u)) l)
      with E u ->
        fail_
          (fun () -> spf "expected a list of atoms,@ but the list contains a non-atom")
          u
}

let keyword ~msg l = {
  run=fun s ->
    match s.S.view with
    | Atom x ->
      begin
        try Ok (List.assoc x l)
        with Not_found ->
          fail_ (const msg) s
      end
    | List _ -> fail_ (const msg) s
}

let rec map_l f l = match l with
  | [] -> return []
  | x :: tl ->
    let* x = f x in
    let+ l = map_l f tl in
    x::l

let rec fold_l f acc l = match l with
  | [] -> return acc
  | x :: tl ->
    let* acc = f acc x in
    fold_l f acc tl

let run (self:_ t) sexp : _ result = self.run sexp

module Err = struct
  type t = err
  let pp out (self:t) =
    let rec loop out (Err {msg;value;ctx_of}) =
      let pp_self out () =
        Fmt.fprintf out "%a@,%s" Loc.pp value.S.loc (msg());
      in
      begin match ctx_of with
        | None -> pp_self out ()
        | Some sub ->
          Fmt.fprintf out "%a@,@[<2>@{<Blue>Context:@ %a@]" loop sub pp_self ()
      end
    in
    Fmt.fprintf out "@[<v>%a@]" loop self
  let to_string = Fmt.to_string pp
  let sexp (Err{value;_}) = value
  let loc self = (sexp self).S.loc

  let rec to_error (Err {msg;value;ctx_of}) : Error.t =
    let loc = value.S.loc in
    match ctx_of with
    | None -> Error.make ~loc @@ msg()
    | Some e ->
      Error.wrap ~loc (msg()) @@ to_error e
end

let run' self sexp =
  run self sexp |> CCResult.map_err Err.to_string

module Fields = struct
  module Str_map = CCMap.Make(CCString)
  type t = {
    mutable m: (sexp * sexp) Str_map.t;
    value: sexp;
  }

  let get_map_of_list_ l =
    let exception E of sexp in
    try
      Ok
        (List.fold_left
           (fun m kv -> match kv.S.view with
              | S.List [{S.view=S.Atom k;_} as k_val;v] ->
                Str_map.add k (k_val,v) m
              | _ -> raise_notrace (E kv))
           Str_map.empty l)
    with E s ->
      fail_ (const "expected a pair") s

  let get_map_ s : (_ Str_map.t,_) result =
    match s.S.view with
    | S.List l -> get_map_of_list_ l
    | S.Atom _ -> fail_ (const "expected a list of pairs") s

  let get_applied name : t m = {
    run=fun s -> match s.S.view with
      | S.List ({S.view=S.Atom name';_} :: l) when name = name' ->
        begin match get_map_of_list_ l with
          | Ok m -> Ok {m; value=s}
          | Error _ as e -> e
        end
      | _ -> fail_ (fun ()->spf "expected (%s (_ _) (_ _) …)" name) s
  }

  let get : t m = {
    run=fun s ->
      match get_map_ s with
      | Ok m -> Ok {m; value=s}
      | Error _ as e -> e
  }

  let check_no_field_left (self:t) : unit m = {
    run=fun _s ->
      match Str_map.choose_opt self.m with
      | None -> Ok ()
      | Some (k, (k_val,_)) ->
        let msg() = Printf.sprintf "unknown key '%s'" k in
        fail_ msg k_val
  }

  let field (self:t) key d : _ m = {
    run=fun _ ->
      match Str_map.get key self.m with
      | None ->
        let msg() = Printf.sprintf "key not found: '%s'" key in
        fail_ msg self.value
      | Some (_,v) ->
        self.m <- Str_map.remove key self.m;
        d.run v
  }

  let field_opt (self:t) key d : _ m = {
    run=fun _ ->
      match Str_map.get key self.m with
      | None -> Ok None
      | Some (_,v) ->
        self.m <- Str_map.remove key self.m;
        d.run v |> CCResult.map (fun x -> Some x)
  }

  let field_opt_or (self:t) key ~default d : _ m =
    let+ x = field_opt self key d in
    match x with
    | None -> default
    | Some y -> y
end

let fields = Fields.get

let applied_fields = Fields.get_applied
