(** Lightweight observer pattern — multiple subscribers, called immediately on
    {!publish}. Each subscriber is uniquely identified by an [int] handle. *)

module IM = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type handle = int

type 'a callback_state = {
  f: 'a -> unit;
  sw: Eio.Switch.t option; (* None = sync subscriber *)
}

type 'a t = { next_handle: int Atomic.t; subs: 'a callback_state IM.t Atomic.t }

let create () = { next_handle = Atomic.make 0; subs = Atomic.make IM.empty }

let unsubscribe self h =
  CCAtomic.update_cas self.subs (fun m -> (), IM.remove h m)

let subscribe self ~sw (f : 'a -> unit) : int =
  let h = Atomic.fetch_and_add self.next_handle 1 in
  CCAtomic.update_cas self.subs (fun old ->
      (), IM.add h { f; sw = Some sw } old);
  Eio.Switch.on_release sw (fun () -> unsubscribe self h);
  h

let subscribe_sync self (f : 'a -> unit) : int =
  let h = Atomic.fetch_and_add self.next_handle 1 in
  CCAtomic.update_cas self.subs (fun m -> (), IM.add h { f; sw = None } m);
  h

let publish self v =
  (* Snapshot before iteration to avoid reentrancy issues *)
  let snapshot = Atomic.get self.subs in
  IM.iter
    (fun h s ->
      match s.sw with
      | Some sw ->
        (try Eio.Fiber.fork ~sw (fun () -> s.f v)
         with Eio.Cancel.Cancelled _ -> unsubscribe self h)
      | None -> (try s.f v with _ -> ()))
    snapshot
