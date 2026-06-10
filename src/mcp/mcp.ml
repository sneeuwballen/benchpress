(** MCP JSON-RPC 2.0 handler.

    The JSON-RPC envelope is hand-parsed for spec compliance. Proto-generated
    types ([Benchpress_mcp_types]) handle inner payload (de)serialization. *)

module T = Benchpress_mcp_types
module J = Json_helpers

type handler = Yojson.Basic.t -> Yojson.Basic.t

type tool_def = {
  name: string;
  description: string;
  input_schema: Yojson.Basic.t;
  handler: handler;
}

type t = (string, tool_def) Hashtbl.t

let create () : t = Hashtbl.create 8

let register (tbl : t) ~(tool : tool_def) : unit =
  Hashtbl.replace tbl tool.name tool

(* -- JSON-RPC 2.0 primitives -- *)

let err_parse = -32700
let err_invalid_request = -32600
let err_method_not_found = -32601
let err_invalid_params = -32602
let err_internal = -32603

let mk_error (id : Yojson.Basic.t) ~code ~msg : string =
  `Assoc
    [ "jsonrpc", `String "2.0"; "id", id;
      "error", `Assoc [ "code", `Int code; "message", `String msg ] ]
  |> Yojson.Basic.to_string

let mk_result (id : Yojson.Basic.t) (result : Yojson.Basic.t) : string =
  `Assoc [ "jsonrpc", `String "2.0"; "id", id; "result", result ]
  |> Yojson.Basic.to_string

let parse_request (body : string)
    : (Yojson.Basic.t * string * Yojson.Basic.t) option =
  try
    let j = Yojson.Basic.from_string body in
    Some
      ( Option.value ~default:`Null (J.assoc_field "id" j),
        Option.value ~default:"" (J.assoc_string "method" j),
        Option.value ~default:`Null (J.assoc_field "params" j) )
  with _ -> None

(* -- tools/list -- *)

let handle_list (tbl : t) (id : Yojson.Basic.t) : string =
  let tools =
    Hashtbl.fold
      (fun _name tool acc ->
        T.make_tool_def ~name:tool.name ~description:tool.description
          ~input_schema_json:(Yojson.Basic.to_string tool.input_schema)
          ()
        :: acc)
      tbl []
  in
  mk_result id
    (T.encode_json_tools_list_result (T.make_tools_list_result ~tools ()))

(* -- tools/call -- *)

let handle_call (tbl : t) (params : Yojson.Basic.t) (id : Yojson.Basic.t)
    : string =
  let name = J.assoc_string_or "name" ~default:"" params in
  let args = Option.value ~default:`Null (J.assoc_field "arguments" params) in
  if name = "" then
    mk_error id ~code:err_invalid_params ~msg:"missing tool name"
  else
    match Hashtbl.find_opt tbl name with
    | None ->
      mk_error id ~code:err_method_not_found
        ~msg:(Printf.sprintf "unknown tool: %s" name)
    | Some tool ->
      (try
         let r = tool.handler args in
         let content_json =
           `List [ `Assoc [ "type", `String "text"; "text", r ] ]
           |> Yojson.Basic.to_string
         in
         mk_result id
           (T.encode_json_tool_call_result
              (T.make_tool_call_result ~content_json ()))
       with exn ->
         let bt = Printexc.get_backtrace () in
         mk_error id ~code:err_internal
           ~msg:
             (Printf.sprintf "tool error: %s\n%s"
                (Printexc.to_string exn) bt))

(* -- initialize -- *)

let handle_initialize (id : Yojson.Basic.t) : string =
  let caps = T.make_server_capabilities ~tools:() () in
  let info = T.make_server_info ~name:"benchpress" ~version:"0.1" () in
  mk_result id
    (T.encode_json_initialize_result
       (T.make_initialize_result ~protocol_version:"2024-11-05"
          ~capabilities:caps ~server_info:info ()))

(* -- main dispatch -- *)

let handle_request (tbl : t) (body : string) : string =
  match parse_request body with
  | None -> mk_error `Null ~code:err_parse ~msg:"Parse error"
  | Some (id, "initialize", _) -> handle_initialize id
  | Some (_, "notifications/initialized", _) -> ""
  | Some (id, "tools/list", _) -> handle_list tbl id
  | Some (id, "tools/call", params) -> handle_call tbl params id
  | Some (id, "", _) ->
    mk_error id ~code:err_invalid_request ~msg:"missing method"
  | Some (id, meth, _) ->
    mk_error id ~code:err_method_not_found
      ~msg:(Printf.sprintf "unknown method: %s" meth)
