module Data :
  sig
    exception Invalid_type_from_string
    type t =
        Json of Yojson.Safe.t
      | Int of int
      | Float of float
      | Text of string
      | Uuid of string
      | Blob of bytes
      | Bool of bool
      | Date of int
      | Null
    and node = {
      mutable id : string;
      created_at : int;
      mutable raw : string;
      mutable data : t;
    }
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val node_to_yojson : node -> Yojson.Safe.t
    val node_of_yojson :
      Yojson.Safe.t -> node Ppx_deriving_yojson_runtime.error_or
    val string_to_typed : string -> string -> t
    val string_of_type : t -> string
    val type_to_string : t -> string
    val jsonify_node : node -> string
    val is_valid_type_string : string -> bool
  end
module Parser :
  sig
    exception Parsing_error
    exception Invalid_token of string
    exception Invalid_type of string
    exception Invalid_string
    type action_token = Exit | Get | Put | Update | To | Delete | As | Named
    val action_token_of_string : string -> action_token
    type ast = { action : action_token; data : string; next : ast option; }
    val string_quote : char
    val string_quote_str : string
    val ends_with : string -> char -> bool
    val starts_with : string -> char -> bool
    val compile_quoted_string : string list -> string * string list
    val parse_statement : string -> ast
  end
module Core :
  sig
    exception Invalid_naming of string
    exception Invalid_action of string
    exception Invalid_json_target of string
    exception Type_mismatch
    exception Syntax_error
    exception Exit_exception
    val root_map : (string, Data.node) Hashtbl.t
    module Persist :
      sig
        exception Import_error
        exception Export_error
        val export : ('a, Data.node) Hashtbl.t -> string
        val import : string -> unit
      end
    val remove_char : string -> char -> string
    val get_json_by_psv : string list -> Yojson.Safe.t -> string
    val get_json : Data.node -> Data.t
    val get : Data.node -> string
    val put : Data.node -> string
    val delete : Data.node -> string
    val update : Data.node -> string
    val check_exists : string -> unit
    val eval_ast : Parser.ast -> string
    val get_input : unit -> string
  end
