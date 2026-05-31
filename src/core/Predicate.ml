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

let rec eval (t : t) (ctx : context) : bool =
  match t with
  | Always -> true
  | Compare (Result, Eq, s) -> ctx.result = s
  | Compare (Result, Neq, s) -> ctx.result <> s
  | Compare (Has_proof, Eq, "true") -> ctx.has_proof
  | Compare (Has_proof, Eq, _) -> not ctx.has_proof
  | Compare (Has_proof, Neq, "true") -> not ctx.has_proof
  | Compare (Has_proof, Neq, _) -> ctx.has_proof
  | Field Has_proof -> ctx.has_proof
  | Field Result -> true
  | And (a, b) ->
    (match eval a ctx with
    | false -> false
    | true -> eval b ctx)
  | Or (a, b) ->
    (match eval a ctx with
    | true -> true
    | false -> eval b ctx)
  | Not e -> not (eval e ctx)

let[@inline] is_space c = c = ' ' || c = '\t'

let is_word_char c =
  match c with
  | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let parse (s : string) : (t, string) result =
  let len = String.length s in
  let i = ref 0 in
  let skip_ws () =
    while !i < len && is_space s.[!i] do
      incr i
    done
  in
  let peek () =
    if !i < len then
      Some s.[!i]
    else
      None
  in
  let starts_with word =
    let wlen = String.length word in
    !i + wlen <= len
    && String.sub s !i wlen = word
    && (!i + wlen >= len || not (is_word_char s.[!i + wlen]))
  in
  let read_word () =
    let start = !i in
    while !i < len && is_word_char s.[!i] do
      incr i
    done;
    String.sub s start (!i - start)
  in
  let read_string_literal () =
    if !i >= len || s.[!i] <> '"' then
      None
    else (
      incr i;
      let start = !i in
      while !i < len && s.[!i] <> '"' do
        incr i
      done;
      if !i >= len then
        None
      else (
        let v = String.sub s start (!i - start) in
        incr i;
        Some v
      )
    )
  in
  let read_cmp () =
    if !i + 1 < len && Char.equal s.[!i] '=' && Char.equal s.[!i + 1] '=' then (
      i := !i + 2;
      Some Eq
    ) else if !i + 1 < len && Char.equal s.[!i] '!' && Char.equal s.[!i + 1] '='
      then (
      i := !i + 2;
      Some Neq
    ) else
      None
  in
  let rec parse_primary () =
    skip_ws ();
    match peek () with
    | Some '(' ->
      incr i;
      let e = parse_or_expr () in
      skip_ws ();
      if !i < len && s.[!i] = ')' then incr i;
      e
    | _ ->
      let word_start = !i in
      let word = read_word () in
      let field =
        match word with
        | "result" -> Some Result
        | "has_proof" -> Some Has_proof
        | _ -> None
      in
      (match field with
      | None ->
        i := word_start;
        Always
      | Some f ->
        skip_ws ();
        (match read_cmp () with
        | Some cmp ->
          skip_ws ();
          (match read_string_literal () with
          | Some v -> Compare (f, cmp, v)
          | None -> Field f)
        | None -> Field f))
  and parse_not_expr () =
    skip_ws ();
    if starts_with "not" then (
      i := !i + 3;
      Not (parse_not_expr ())
    ) else
      parse_primary ()
  and parse_and_expr () =
    let left = parse_not_expr () in
    let rec loop left =
      skip_ws ();
      if starts_with "and" then (
        i := !i + 3;
        let right = parse_not_expr () in
        loop (And (left, right))
      ) else
        left
    in
    loop left
  and parse_or_expr () =
    let left = parse_and_expr () in
    let rec loop left =
      skip_ws ();
      if starts_with "or" then (
        i := !i + 2;
        let right = parse_and_expr () in
        loop (Or (left, right))
      ) else
        left
    in
    loop left
  in
  skip_ws ();
  if len = 0 then
    Ok Always
  else (
    let t = parse_or_expr () in
    skip_ws ();
    if !i = len then
      Ok t
    else
      Error
        (Printf.sprintf "unexpected input at position %d: %S" !i
           (String.sub s !i (len - !i)))
  )
