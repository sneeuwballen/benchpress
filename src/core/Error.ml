
open Common

type t = {
  msg: string;
  loc: Loc.t option;
  ctx_of: t option;
}

let msg self = self.msg
let loc self = self.loc
let ctx_of self = self.ctx_of

let unwrap_ctx self =
  let rec aux acc self = match self.ctx_of with
    | None -> self, acc
    | Some e -> aux (self::acc) e
  in
  aux [] self

let make ?loc msg : t = { msg; loc; ctx_of=None }
let makef ?loc fmt = Fmt.kasprintf (make ?loc) fmt

let of_exn ?loc e =
  let res = Printf.sprintf "%s\n%s"
      (Printexc.to_string e) (Printexc.get_backtrace ())
  in
  make ?loc res

let wrap ?loc msg e = { msg; loc; ctx_of=Some e }
let wrapf ?loc fmt = Fmt.kasprintf (wrap ?loc) fmt

let pp out (self:t) =
  let pp_self_ out self =
    let {msg; loc; ctx_of=_} = self in
    match loc with
    | None ->
      Fmt.fprintf out "@[@{<Red>Error@}:@ %a@]" Fmt.string_lines msg
    | Some loc ->
      Fmt.fprintf out "@[<v>%a@[@{<Red>Error@}:@ %a@]@]"
        Loc.pp loc Fmt.string_lines msg
  in
  let rec pp_rec_ out self =
    begin match self.ctx_of with
      | None -> ()
      | Some e ->
        pp_rec_ out e;
        Fmt.fprintf out "@,%s@,@{<Blue>Context@}:@," (String.make 60 '-');
    end;
    pp_self_ out self
  in
  Fmt.fprintf out "@[<v>%a@]" pp_rec_ self

let show = Fmt.to_string pp
