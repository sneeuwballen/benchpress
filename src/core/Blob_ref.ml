open Benchpress_core

let inline_limit = 200

let sha256_hex (b : bytes) : string =
  let d = Digestif.SHA256.digest_bytes b in
  Digestif.SHA256.to_hex d

let of_bytes (b : bytes) : data_ref =
  if Bytes.length b <= inline_limit then
    Inline b
  else
    Sha256ref (sha256_hex b)

let to_bytes ~(read_zip_entry : string -> bytes) (dr : data_ref) : bytes =
  match dr with
  | Inline b -> b
  | Sha256ref h -> read_zip_entry ("data." ^ h)

let of_string (s : string) : data_ref = of_bytes (Bytes.of_string s)

let to_string ~read_zip_entry (dr : data_ref) : string =
  Bytes.to_string (to_bytes ~read_zip_entry dr)
