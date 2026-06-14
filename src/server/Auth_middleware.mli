val bearer_of_req : _ Tiny_httpd.Request.t -> string option
(** Extract the Bearer token from the Authorization header, if present. *)

val is_localhost : _ Tiny_httpd.Request.t -> bool
(** [is_localhost req] is [true] if the client address is 127.0.0.1 or ::1. *)

val authenticate :
  allow_localhost:bool ->
  Auth.t ->
  _ Tiny_httpd.Request.t ->
  (string, string) result
(** Validate Bearer token. Returns [Ok user_id] or [Error reason]. If
    [allow_localhost] is [true] and the request comes from localhost, returns
    [Ok "localhost"] without checking the API key. *)

val require_auth :
  allow_localhost:bool ->
  Auth.t ->
  _ Tiny_httpd.Request.t ->
  send_resp:(Tiny_httpd.Response.t -> unit) ->
  string option
(** [require_auth auth req ~send_resp] checks Bearer token. Returns
    [Some user_id] if valid. If invalid/missing, calls [send_resp] with a 401
    response and returns [None]. *)

val middleware : allow_localhost:bool -> Auth.t -> Tiny_httpd.Middleware.t
(** Middleware that authenticates requests via Bearer token and stores the
    user_id in [req.meta] under [user_meta_key]. Returns 401 on failure. If
    [allow_localhost] is [true], requests from localhost bypass auth. *)

val user_of_req : _ Tiny_httpd.Request.t -> string option
(** Retrieve the authenticated user_id stored by [middleware], if present. *)
