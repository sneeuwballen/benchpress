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
        Some (fun (l1,c1)(l2,c2) _file : loc ->
            let loc = Loc.{
                start = Pos.of_line_col l1 c1;
                stop = Pos.of_line_col l2 c2;
                input = !cur_input_
              } in
            (*Logs.debug (fun k->k"make_loc %S %d:%d - %d:%d@ res %a" file l1 c1 l2 c2 Loc.pp loc);*)
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
