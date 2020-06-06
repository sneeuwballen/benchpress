
include Api_types
include Api_pp
include Api_pb

(** Port to listen on by default *)
let default_port = 8090

(** Helper to serialize a value into protobuf *)
let pb_to_string (f: 'a -> Pbrt.Encoder.t -> unit) (x:'a) : string =
  let e = Pbrt.Encoder.create() in
  f x e;
  Pbrt.Encoder.to_string e

let pb_of_string (f:Pbrt.Decoder.t -> 'a) (s:string) : 'a =
  let d = Pbrt.Decoder.of_string s in
  try f d
  with e ->
    failwith ("protobuf decoding error: " ^ Printexc.to_string e)

let unwrap_field_ what = function
  | Some x -> x
  | None -> failwith ("tried to unwrap protobuf field " ^ what)

