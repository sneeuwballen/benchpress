
(** {1 API context} *)

module P = Protocol_t

type t = {
  on_push_notification : P.push_notification Signal.t;
}

let create() : t =
  { on_push_notification=Signal.create();
  }

let push_notification self = self.on_push_notification
