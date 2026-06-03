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

open CCParse
open CCParse.Infix

let is_word_char c = is_alpha_num c || c = '_'

let parse s =
  let kw_test w =
    chars1_if is_word_char >>= fun word ->
    if String.equal word w then
      pure ()
    else
      fail ""
  in
  let kw_consume w = kw_test w >|= fun _ -> w in
  let str_literal = char '"' *> chars_if (fun c -> c <> '"') <* char '"' in
  let cmp_ = string "==" >|= (fun _ -> Eq) <|> (string "!=" >|= fun _ -> Neq) in
  let field_rest f =
    (let* c = skip_space *> cmp_ in
     let* v = skip_space *> str_literal in
     pure (Compare (f, c, v)))
    <|> pure (Field f)
  in
  let expr =
    fix (fun expr ->
        let primary =
          char '(' *> skip_space *> expr
          <* skip_space
          <* optional (char ')')
          <|> try_or_l
                [
                  kw_test "result", kw_consume "result" *> field_rest Result;
                  ( kw_test "has_proof",
                    kw_consume "has_proof" *> field_rest Has_proof );
                ]
                ~else_:(pure Always)
        in
        let not_expr =
          fix (fun self ->
              skip_space
              *> try_or_l
                   [
                     ( kw_test "not",
                       kw_consume "not" *> skip_space *> self >|= fun e -> Not e
                     );
                   ]
                   ~else_:primary)
        in
        let and_expr =
          skip_space
          *>
          let* left = not_expr in
          let rec loop left =
            skip_space
            *> try_or_l
                 [
                   ( kw_test "and",
                     kw_consume "and" *> skip_space *> not_expr >>= fun right ->
                     loop (And (left, right)) );
                 ]
                 ~else_:(pure left)
          in
          loop left
        in
        skip_space
        *>
        let* left = and_expr in
        let rec loop left =
          skip_space
          *> try_or_l
               [
                 ( kw_test "or",
                   kw_consume "or" *> skip_space *> and_expr >>= fun right ->
                   loop (Or (left, right)) );
               ]
               ~else_:(pure left)
        in
        loop left)
  in
  parse_string
    (skip_space *> (eoi >|= (fun () -> Always) <|> (expr <* skip_space <* eoi)))
    s
