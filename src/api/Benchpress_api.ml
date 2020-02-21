
type json = Protocol_j.json

module Protocol_j = Protocol_j
module Protocol_t = Protocol_t

module P = Protocol_t
module Signal = Signal

(** Main module *)
module Ctx = Api_ctx


module As_json : sig
  val call :
    Ctx.t ->
    query:string ->
    (string,string) result
end = struct
  let call ctx ~query : _ result =
    try
      let q = Protocol_j.query_of_string query in
      let r = match q with
        | `Q_task_list ->
          let tasks = Ctx.list_tasks ctx in
          Protocol_j.string_of_response (`R_task_list tasks)
      in
      Ok r
    with e ->
      Error ("cannot parse query: " ^ Printexc.to_string e)
end

(* TODO: client based on curly *)

(* TODO: simple TCP server? *)
(* TODO: simple TCP client? *)
