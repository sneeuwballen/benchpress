type t
(** Auth DB handle *)

val create : string -> t
(** [create path] opens (or creates) the auth sqlite DB at [path] and runs migrations. *)

val default_path : unit -> string
(** Default path: XDG data dir / "benchpress" / "auth.db" *)

val create_user : t -> email:string -> (string, string) result
(** Returns the new user's UUID, or Error if email already exists. *)

val create_api_key : t -> user_id:string -> (string, string) result
(** Returns a fresh hex API key for the given user, or Error if user not found. *)

val revoke_api_key : t -> key:string -> unit
(** Delete the given API key. No-op if it doesn't exist. *)

val list_api_keys : t -> user_id:string -> (string * string) list
(** Returns (key, created_at) pairs for the user. *)

val authenticate : t -> key:string -> string option
(** Returns Some user_id if the key is valid, None otherwise. *)

val list_users : t -> (string * string * string) list
(** Returns (id, email, created_at) triples. *)

(** {2 Job tracking} *)

val register_job : t -> job_id:string -> user_id:string -> unit
(** Record a new job as queued. *)

val get_job_user : t -> job_id:string -> string option
(** Returns the user_id who owns this job, or None if unknown. *)

val set_job_cancelled : t -> job_id:string -> unit

val set_job_completed : t -> job_id:string -> result_file:string -> unit

val get_job_cancelled : t -> job_id:string -> bool
(** Returns true if the job was marked cancelled. *)
