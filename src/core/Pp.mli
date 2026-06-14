(** Pretty-printing utilities *)

val pp_list :
  ?sep:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a list ->
  unit
(** Pretty-print a list with a custom separator *)

val pp_l :
  ?sep:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a list ->
  unit
(** Alias for pp_list *)

val pp_pair :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  'a * 'b ->
  unit
(** Pretty-print a pair *)

val pp_f :
  string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
(** Pretty-print with a label *)

val pp_opt :
  string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a option ->
  unit
(** Pretty-print an optional value with a label *)

val pp_fl1 :
  string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a list ->
  unit
(** Pretty-print a list with a label (non-empty) *)

val pp_l1 :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
(** Pretty-print a non-empty list *)

val pp_str : Format.formatter -> string -> unit
(** Pretty-print a quoted string *)

val pp_regex : Format.formatter -> string -> unit
(** Pretty-print a regex pattern *)

(** {2 YAML record combinators} *)

type field
(** A typed, self-contained field descriptor for record printing. *)

val field : string -> (Format.formatter -> 'a -> unit) -> 'a -> field
(** [field name printer value] creates a required field. *)

val field_opt : string -> (Format.formatter -> 'a -> unit) -> 'a option -> field
(** [field_opt name printer value] creates an optional field. Skipped (no
    output) when [value] is [None]. *)

val field_list : string -> (Format.formatter -> 'a -> unit) -> 'a list -> field
(** [field_list name printer value] creates a list field. Skipped (no output)
    when [value] is [[]]. *)

val pp_record : string -> Format.formatter -> field list -> unit
(** Print a named YAML record. E.g. [pp_record "prover" out fields] prints:
    {\[prover:
      name: "z3"
      cmd: "z3 $file"\\]} *)

(** {2 YAML-style formatting} *)

val pp_yaml_field :
  string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
(** Pretty-print a field in YAML style: [  name: value] *)

val pp_yaml_field_opt :
  string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a option ->
  unit
(** Pretty-print an optional field in YAML style *)

val pp_yaml_list :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
(** Pretty-print a YAML-style list with [- item] prefixes *)

val pp_yaml_list_field :
  string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a list ->
  unit
(** Pretty-print a named YAML list field *)
