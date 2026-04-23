val bearer_of_req : _ Tiny_httpd.Request.t -> string option
(** Extract the Bearer token from the Authorization header, if present. *)

val authenticate : Auth.t -> _ Tiny_httpd.Request.t -> (string, string) result
(** Validate Bearer token. Returns [Ok user_id] or [Error reason]. *)

val require_auth :
  Auth.t ->
  _ Tiny_httpd.Request.t ->
  send_resp:(Tiny_httpd.Response.t -> unit) ->
  string option
(** [require_auth auth req ~send_resp] checks Bearer token. Returns
    [Some user_id] if valid. If invalid/missing, calls [send_resp] with a 401
    response and returns [None]. *)

val middleware : Auth.t -> Tiny_httpd.Middleware.t
(** Middleware that authenticates requests via Bearer token and stores the
    user_id in [req.meta] under [user_meta_key]. Returns 401 on failure. *)

val user_of_req : _ Tiny_httpd.Request.t -> string option
(** Retrieve the authenticated user_id stored by [middleware], if present. *)
