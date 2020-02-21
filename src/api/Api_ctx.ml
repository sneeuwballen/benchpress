
(** {1 API context} *)

module P = Protocol_t

type t = {
  list_tasks:(unit -> P.task_descr option * int);
  on_push_notification : P.push_notification Signal.t;
}

let create ~list_tasks () : t =
  { list_tasks;
    on_push_notification=Signal.create();
  }

let list_tasks self = self.list_tasks()
let push_notification self = self.on_push_notification
