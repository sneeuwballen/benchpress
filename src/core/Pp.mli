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
