(** {1 S-expressions with locations} *)

open Common

type t = {
  loc: Loc.t;
  view: view;
}
and view =
  | Atom of string
  | List of t list

let atom_with_loc ~loc s : t= {loc; view=Atom s}
let list_with_loc ~loc l : t = {loc; view=List l}
let atom = atom_with_loc ~loc:Loc.none
let list = list_with_loc ~loc:Loc.none

(** {2 Serialization and helpers} *)

module Sexp0 : sig
  type 'a or_error = ('a, string) result
  val pp : t Fmt.printer
  val of_int : int -> t
  val of_float : float -> t
  val of_bool : bool -> t
  val of_list : t list -> t
  val to_string : t -> string
  val parse_string : filename:string -> string -> t or_error
  val parse_string_l : filename:string -> string -> t list or_error
  val parse_file : string -> t or_error
  val parse_file_l : string -> t list or_error
  module Sexp : CCSexp_intf.S with type t = t
end = struct
  type 'a or_error = ('a, string) result

  let cur_file_ = ref ""
  let cur_input_ = ref (Loc.Input.string "")

  module Sexp = CCSexp.Make(struct
      type nonrec loc=Loc.t
      type nonrec t=t

      let make_loc =
        Some (fun (l1,c1)(l2,c2) file : loc ->
            let input = !cur_input_ in
            let file = if file="" then !cur_file_ else file in
            let loc = {Loc.file; input; start={line=l1;col=c1};stop={line=l2;col=c2}} in
            Logs.debug (fun k->k"make_loc %d:%d - %d:%d@ res %a" l1 c1 l2 c2 Loc.pp loc);
            loc)

      let atom_with_loc ~loc s : t= {loc; view=Atom s}
      let list_with_loc ~loc l : t = {loc; view=List l}
      let atom = atom
      let list = list

      let match_ s ~atom:fa ~list:fl =
        match s.view with
        | Atom s -> fa s
        | List l -> fl l
    end)

  include Sexp

  let parse_string ~filename s =
    cur_file_ := filename;
    cur_input_ := Loc.Input.string s;
    parse_string s

  let parse_string_l ~filename s =
    cur_file_ := filename;
    cur_input_ := Loc.Input.string s;
    parse_string_list s

  let parse_file file =
    cur_file_ := file;
    cur_input_ := Loc.Input.file file;
    parse_file file

  let parse_file_l file =
    cur_file_ := file;
    cur_input_ := Loc.Input.file file;
    parse_file_list file
end

include Sexp0

(** {2 Decoder} *)

module D = struct
  include Decoders.Decode.Make(struct
      type value = t
      let to_list l = list l
      let pp = pp
      let of_string s = parse_string ~filename:"<string>" s
      let of_file = parse_file
      let get_string s = match s.view with Atom s -> Some s | List _ -> None
      let get_ ~f s = match s.view with
        | Atom s -> (try Some (f s) with _ -> None)
        | List _ -> None
      let get_int s = get_ ~f:int_of_string s
      let get_float s = get_ ~f:float_of_string s
      let get_bool s = get_ ~f:bool_of_string s
      let get_null s = match s.view with Atom "null" | List [] -> Some () | _ -> None
      let get_list s = match s.view with List l -> Some l | Atom _ -> None

      let get_key_value_pairs s = match s.view with
        | List l ->
          (try
             Some
               (List.map
                  (fun kv -> match kv.view with
                     | List [k;v] -> k, v
                     | List (k :: vs) -> k, list_with_loc ~loc:kv.loc vs (* support "(foo a b c)" *)
                     | _ -> raise Exit)
                  l)
           with Exit -> None)
        | Atom _ -> None
    end)

  let (>>::) e f = uncons f e
  let list0 = list value >>= function [] -> succeed () | _ -> fail "need empty list"
  let list1 s = list s >>= function [x] -> succeed x | _ -> fail "need unary list"
  let list2 s1 s2 =
    list value >>= function
    | [x;y] -> s1 x >>= fun x -> s2 y >|= fun y -> x,y
    | _ -> fail "need binary list"
end

let rec loc_of_err : D.error -> Loc.t list =
  function
  | Decoders.Decode.Decoder_error (_, s) ->
    begin match s with Some s -> [s.loc] | None -> [] end
  | Decoders.Decode.Decoder_errors l -> CCList.flat_map loc_of_err l
  | Decoders.Decode.Decoder_tag (_, e) -> loc_of_err e

(** {2 Encoder} *)

module E = Decoders.Encode.Make(struct
    type value = t
    let to_string = to_string
    let of_string = atom
    let of_int = of_int
    let of_float = of_float
    let of_bool = of_bool
    let of_list = of_list
    let null = of_list []
    let of_key_value_pairs l = of_list (List.map (fun (k,v) -> of_list [k;v]) l)
  end)
