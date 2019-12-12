(* This file is free software. See file "license" for more details. *)

(** {1 Config File} *)

[@@@ocaml.deprecated "use sexp stanzas"]

type 'a or_error = ('a, string) result
type file = string

type t

val empty : t

val create : unit -> t
(** Local config file *)

val merge : t -> t -> t
(** [merge c1 c2] merges [c1] and [c2] together. Values in [c1] will have
    priority over values of [c2]. *)

val merge_l : t list -> t
(** Same as {!merge} *)

val parse_file : file -> t or_error

val parse_or_empty : file -> t
(** Parse a config file or return an empty one *)

val parse_files : file list -> t or_error
(** Parse the given config files and merge them together
    (in increasing priority order) *)

(** {2 Accessors} *)

type 'a getter

type 'a field_getter = ?default:'a -> string -> 'a getter
(** Gets a field by its name in a (sub-)table.
    @param default if provided, this is returned in case the
     field is not present *)

type table
(** A TOML table *)

val top : table getter
(** Return the toplevel table itself *)

val table : table field_getter
val bool : bool field_getter
val int : int field_getter
val float : float field_getter
val string : string field_getter
val string_list : string list field_getter

val none : 'a option getter
val some : 'a getter -> 'a option getter

val table_l : string list -> table getter
(** [table_l path] looks for a table in the given nested path *)

val return : 'a -> 'a getter
(** [return c] is a getter that returns [c] no matter what. Never fails. *)

val pure : 'a -> 'a getter
(** Alias to {!return} *)

val fail : string -> _ getter

val pure_or_error : 'a or_error -> 'a getter

val add_ctx : string -> 'a getter -> 'a getter
val add_ctxf : ('a, Format.formatter, unit, 'b getter -> 'b getter) format4 -> 'a

val lazy_ : 'a lazy_t getter -> 'a getter

val try_ : 'a getter list -> 'a getter
(** Try these getters in successive order *)

val (|>>) : table getter -> 'a getter -> 'a getter
(** [k |>> g] gets a sub-table via key [k], then  finds a ['a] in it via [g] *)

val (<|>) : 'a getter -> 'a getter -> 'a getter
(** Binary version of {!try_} *)

val map : ('a -> 'b) -> 'a getter -> 'b getter

val (>|=) : 'a getter -> ('a -> 'b) -> 'b getter

val (>>=) : 'a getter -> ('a -> 'b getter) -> 'b getter

val map_l : ('a -> 'b getter) -> 'a list -> 'b list getter

val try_tables : table getter list -> 'a getter -> 'a getter
(** Mix of {!|>>} and {!try_} to find a field in several tables *)

val get : t -> 'a getter -> 'a or_error
(** [get config g] tries to find a value using [f] in [config],
    or returns an error. *)

val get_exn : t -> 'a getter -> 'a
(** [get config g] tries to find a value using [f] in [config],
    or returns an error. *)

val get_or : default:'a -> t -> 'a getter -> 'a
(** Get with a default *)
