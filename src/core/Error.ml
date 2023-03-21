open Common

type t = { msg: string; loc: Loc.t option; ctx_of: t option }

exception E of t

let raise x = raise (E x)
let msg self = self.msg
let loc self = self.loc
let ctx_of self = self.ctx_of

let unwrap_ctx self =
  let rec aux acc self =
    match self.ctx_of with
    | None -> self, acc
    | Some e -> aux (self :: acc) e
  in
  aux [] self

let make ?loc msg : t = { msg; loc; ctx_of = None }
let makef ?loc fmt = Fmt.kasprintf (make ?loc) fmt

let of_exn ?loc e =
  let res =
    Printf.sprintf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ())
  in
  make ?loc res

let wrap ?loc msg e = { msg; loc; ctx_of = Some e }
let wrapf ?loc fmt = Fmt.kasprintf (wrap ?loc) fmt
let fail ?loc msg = raise (make ?loc msg)
let failf ?loc fmt = Fmt.kasprintf (fail ?loc) fmt

let guard wrap f =
  try f () with
  | E e -> raise (wrap e)
  | exn -> raise (wrap (of_exn exn))

let unwrap = function
  | Ok x -> x
  | Error e -> raise e

let unwrap_opt' ?loc msg = function
  | Some x -> x
  | None -> fail ?loc (msg ())

let unwrap_opt ?loc msg o = unwrap_opt' ?loc (fun () -> msg) o
let hbar = String.make 60 '-'

let pp out (self : t) =
  let pp_itself out self =
    let { msg; loc; ctx_of = _ } = self in
    match loc with
    | None -> Fmt.string_lines out msg
    | Some loc ->
      Fmt.fprintf out "@[<v>%a@,%a@]@]" Fmt.string_lines msg Loc.pp loc
  in
  let rec loop out self =
    match self.ctx_of with
    | None -> Fmt.fprintf out "@[<v2>@{<Red>Error@}:@ %a@]" pp_itself self
    | Some e ->
      Fmt.fprintf out "%a@,%s@,@[<v2>@{<Blue>Context@}:@ %a@]" loop e hbar
        pp_itself self
  in
  Fmt.fprintf out "@[<v>%a@]" loop self

let show e = Fmt.sprintf "%a" pp e

let () =
  Printexc.register_printer (function
    | E e -> Some (show e)
    | _ -> None)
