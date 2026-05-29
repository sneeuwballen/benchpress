(* This file is free software. See file "license" for more details. *)

type field = Result | Has_proof
type cmp = Eq | Neq

type t =
  | Compare of field * cmp * string
  | Field of field
  | And of t * t
  | Or of t * t
  | Not of t
  | Always

type context = { result: string; has_proof: bool }

val eval : t -> context -> bool
val parse : string -> (t, string) result
