(* This file is free software. See file "license" for more details. *)

(** {1 Time limits} *)
module Time : sig
  type t
  (** Type used to represent timeouts *)

  (** The different ways to view a time *)
  type view = Seconds | Minutes | Hours (**)

  val hash : t -> int
  val equal : t -> t -> bool

  val compare : t -> t -> int
  (** Usual functions *)

  val pp : t CCFormat.printer
  (** Printer *)

  val mk : ?s:int -> ?m:int -> ?h:int -> unit -> t
  (** Create a timeout of [~h] hours, [~m] minutes and
      [~s] seconds. The arguments default to [0] is not provided. *)

  val default : t

  val add : t -> t -> t
  (** Add timeouts. *)

  val as_int : view -> t -> int
  (** View a time converted into the given view/units, truncating
      the result. For instance: [as_int Minutes (mk ~s:90) = 1] *)

  val as_float : view -> t -> float
  (** View a time converted into the given view/units.
      For instance: [as_float Minutes (mk ~s:90) = 1.5] *)
end

(** {1 Memory limits} *)
module Memory : sig
  type t
  (** Type used to represent memory limits *)

  (** The different ways to view a memory limit *)
  type view = Bytes | Kilobytes | Megabytes | Gigabytes | Terabytes (**)

  val hash : t -> int
  val equal : t -> t -> bool

  val compare : t -> t -> int
  (** Usual functions *)

  val pp : t CCFormat.printer
  (** Printer *)

  val mk : ?b:int -> ?k:int -> ?m:int -> ?g:int -> ?t:int -> unit -> t
  (** Create a memory limits of [~t] terabytes, [~g] gigabytes,
      [~m] megabytes, [~k] kilobytes, and [~b] bytes. The arguments default
      to [0] if not provided. *)

  val default : t

  val as_int : view -> t -> int
  (** View a memory size converted into the given view/units, truncating
      the result. For instance: [as_int Kilobytes (mk ~b:1_500) = 1] *)

  val as_float : view -> t -> float
  (** View a memory size converted into the given view/units.
      For instance: [as_float Kilobytes (mk ~b:1_500) = 1.5] *)
end

(** {1 Stack limits} *)
module Stack : sig
  (** Stack size limits. *)
  type t = Unlimited | Limited of Memory.t (**)

  val hash : t -> int
  val equal : t -> t -> bool

  val compare : t -> t -> int
  (** Usual functions *)

  val pp : t CCFormat.printer
  (** Printer *)
end

(** {1 Set of limits} *)
module All : sig
  type t = {
    time: Time.t option;
    memory: Memory.t option;
    stack: Stack.t option;
  }
  (** Type used to represent a set of (optional) limits,
      including a time limit, a memory limit and a stack limit. *)

  val hash : t -> int
  val equal : t -> t -> bool

  val compare : t -> t -> int
  (** Usual functions *)

  val pp : t CCFormat.printer
  (** Printer *)

  val mk : ?time:Time.t -> ?memory:Memory.t -> ?stack:Stack.t -> unit -> t
  (** Create a set of limits. *)

  val default : t
  val update_time : (Time.t option -> Time.t option) -> t -> t
  val update_memory : (Memory.t option -> Memory.t option) -> t -> t

  val update_stack : (Stack.t option -> Stack.t option) -> t -> t
  (** Update functions *)

  val with_defaults : defaults:t -> t -> t
  (** [with_defaults ~defaults t] is the same as t, except for limits
      of [t] which were [None], in which case the value from [defaults]
      is used (which can itself also be [None]). *)

  exception Limit_missing of string
  (** Exception raised by {!substitute} when trying to substitute a
      limit that was not provided. *)

  val substitute :
    time_as:Time.view ->
    memory_as:Memory.view ->
    stack_as:Memory.view ->
    t ->
    string ->
    string option
  (** Given a set of limits, and a view for each of these limits,
      return a substitution function adequate for use with
      {!Buffer.add_substitute}.
      @raise Limit_missing if a limit is needed for substitution
        but not present in the argument. *)
end
