(** {1 S-expressions with locations} *)

type pos = {line: int; col: int}

type loc = {
  file: string;
  start: pos;
  stop: pos;
}

let noloc = {file=""; start={line=1;col=1}; stop={line=1;col=1}}

let pp_loc out (loc:loc) : unit =
  if loc.start.line=loc.stop.line then (
    Format.fprintf out "%s:%d.%d-%d" loc.file loc.start.line loc.start.col loc.stop.col
  ) else (
    Format.fprintf out "%s:%d.%d-%d.%d"
      loc.file loc.start.line loc.start.col loc.stop.line loc.stop.col
  )

include CCSexp.Make(struct
    type nonrec loc=loc
    type t = {
      loc: loc;
      view: view;
    }
    and view =
      | Atom of string
      | List of t list

    let make_loc =
      Some (fun (l1,c1)(l2,c2) file : loc ->
          {file; start={line=l1;col=c1};stop={line=l2;col=c2}})

    let atom_with_loc ~loc s : t= {loc; view=Atom s}
    let list_with_loc ~loc l : t = {loc; view=List l}
    let atom = atom_with_loc ~loc:noloc
    let list = list_with_loc ~loc:noloc

    let match_ s ~atom ~list =
      match s.view with
      | Atom s -> atom s
      | List l -> list l
  end)
