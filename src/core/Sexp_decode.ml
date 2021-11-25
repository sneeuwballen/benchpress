open Common

module S = Sexp_loc
type sexp = Sexp_loc.t

type err = Err of string * sexp

type +'a t = {
  run: sexp -> ('a, err) result;
} [@@unboxed]
type 'a m = 'a t

let return x = {run=fun _ -> Ok x}

let fail_ str sexp = Error (Err (str, sexp))

let fail str = {
  run=fun s -> fail_ str s
}
let failf fmt = Fmt.kasprintf fail fmt

let value = {run=fun s->Ok s}
let atom = {run=fun s ->
    match s.S.view with
    | S.Atom x -> Ok x
    | S.List _ -> fail_ "expected an atom" s
  }

let list = {run=fun s ->
    match s.S.view with
    | S.List l -> Ok l
    | S.Atom _ -> fail_ "expected a list" s
  }

let list_of (type x) d =
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
        | Atom _ -> fail_ "expected list" s
  }

let applied s = ()

module Pat = struct
  type ('a,'b) t =
    | [] : ('a,'a) t
    | (::) : 'a m * ('b, 'k) t -> ('a * 'b, 'a -> 'k) t

  let rec match_
    : type a b. (a, b) t -> sexp list -> b -> (a, err) result
    = fun pat l f res -> match pat, l with
      | [], [] -> Ok res
      | p1 :: ps, x1 :: xs ->
        match p1.run x1 with
        | Error _ as e -> e
        | Ok x -> 
end

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

let any_of (type x) ~msg decs = {
  run=fun s ->
    let exception E of x in
    try
      List.iter
        (fun d ->
           match d.run s with
           | Ok x -> raise_notrace (E x)
           | Error _ -> ())
        decs;
      fail_ msg s
    with E x -> Ok x
}

let sub self s = {
  run=fun _ -> self.run s
}

let try_apply name f else_ = {
  run=fun s ->
    match s.S.view with
    | S.List ({S.view=S.Atom name2;_} :: args) when name=name2 ->
      (f args).run s
    | _ -> else_.run s
}


let run (self:_ t) sexp : _ result = self.run sexp

module Err = struct
  type t = err
  let pp out (Err (s,sexp)) =
    Fmt.fprintf out "@[<v>%a@,%s@]" Loc.pp sexp.S.loc s
  let to_string = Fmt.to_string pp
  let sexp (Err(_,sexp)) = sexp
  let loc self = (sexp self).S.loc
end

let run' self sexp =
  run self sexp |> CCResult.map_err Err.to_string

module Fields = struct
  module Str_map = CCMap.Make(CCString)
  type t = {
    mutable m: (sexp * sexp) Str_map.t;
    value: sexp;
  }

  let get_map_ s : (_ Str_map.t,_) result =
    let exception E of sexp in
    match s.S.view with
    | S.List l ->
      (try
         Ok
           (List.fold_left
              (fun m kv -> match kv.S.view with
                 | S.List [{S.view=S.Atom k;_} as k_val;v] ->
                   Str_map.add k (k_val,v) m
                 | _ -> raise_notrace (E kv))
              Str_map.empty l)
       with E s ->
         fail_ "expected a pair" s
      )
    | S.Atom _ -> fail_ "expected a list of pairs" s

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
        let msg = Printf.sprintf "unknown key '%s'" k in
        fail_ msg k_val
  }

  let field (self:t) key d : _ m = {
    run=fun _ ->
      match Str_map.get key self.m with
      | None ->
        let msg = Printf.sprintf "key not found: '%s'" key in
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
end

let fields = Fields.get
