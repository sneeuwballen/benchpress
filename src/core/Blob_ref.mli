val inline_limit : int
(** Blobs up to this many bytes are stored inline; larger ones are stored by
    hash reference. *)

val sha256_hex : bytes -> string
(** Compute the SHA-256 hex digest of [bytes]. *)

val of_bytes : bytes -> Benchpress_core.data_ref
(** [of_bytes b] returns [Inline b] when [Bytes.length b <= inline_limit],
    otherwise [Sha256ref (sha256_hex b)]. *)

val to_bytes :
  read_zip_entry:(string -> bytes) -> Benchpress_core.data_ref -> bytes
(** [to_bytes ~read_zip_entry dr] resolves [dr] to bytes. For [Sha256ref h],
    calls [read_zip_entry ("data." ^ h)]. *)

val of_string : string -> Benchpress_core.data_ref

val to_string :
  read_zip_entry:(string -> bytes) -> Benchpress_core.data_ref -> string
