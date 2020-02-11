
type 'a mlist = Nil | Cons of {f: 'a; mutable tl: 'a mlist}
type 'a callback =
  | CB of ('a -> bool)
  | CB_map : ('a -> 'b) * 'b callback -> 'a callback
type 'a t = {
  mutable fs: 'a callback mlist;
}

let create() : _ t = { fs=Nil }

module Source = struct
  type nonrec 'a t = 'a t
  let on_ self f = self.fs <- Cons {f; tl=self.fs}
  let on self f = on_ self (CB f)
  let on_always self f = on_ self (CB (fun x -> f x; true))
  let once self f = on_ self (CB (fun x -> f x; false))
end

module Sink = struct
  type nonrec 'a t = 'a t

  let rec eval_cb
    : type a. a callback -> a -> bool
    = fun cb x -> match cb with
      | CB f -> f x
      | CB_map (f, cb') -> eval_cb cb' (f x)

  let send (self:'a t) (x:'a) : unit =
    let rec loop l : _ mlist =
      match l with
      | Nil -> Nil
      | Cons r ->
        (* call the callback *)
        let keep =
          try eval_cb r.f x
          with e ->
            (* log and drop the callback *)
            Log.err
              (fun k->k "error when calling callback: %s" (Printexc.to_string e));
            false
        in
        if keep then (
          let tl' = loop r.tl in
          if r.tl != tl' then (
            r.tl <- tl';
          );
          l
        ) else loop r.tl
    in
    self.fs <- loop self.fs
end

include (Source : module type of struct include Source end with type 'a t := 'a t)
include (Sink : module type of struct include Sink end with type 'a t := 'a t)

let source self = self
let sink self = self

let chain a b : unit = on_always a (send b)
let chain_map ~f a b : unit = on_ a (CB_map (f, CB (fun x-> send b x; true)))
