(** MCP (Model Context Protocol) JSON-RPC handler.

    Supports dispatching [tools/list] and [tools/call] methods. *)

type handler = Yojson.Basic.t -> Yojson.Basic.t
(** A tool handler receives JSON params and returns JSON result. *)

type tool_def = {
  name: string;
  description: string;
  input_schema: Yojson.Basic.t;  (** JSON Schema for the tool's input *)
  handler: handler;
}
(** A tool definition registered with the MCP server. *)

type t
(** Registry of MCP tools. *)

val create : unit -> t
(** Create a new empty tool registry. *)

val register : t -> tool:tool_def -> unit
(** Register a tool in the registry. *)

val handle_request : t -> string -> string
(** Handle a raw JSON-RPC request string, returns a JSON-RPC response string. *)
