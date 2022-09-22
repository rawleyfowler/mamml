(* 
   Mamml is an in memory key-value caching solution with first class JSON.

   ISC License

   Copyright 2022 Rawley Fowler <rawleyfowler@gmail.com>

   Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, 
   provided that the above copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO 
   THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO 
   EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR 
   ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, 
   NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

   .......................................................................................   

   Example valid mamml instructions for storage:
   
   PUT '123' AS Number; -> Returns the unique ID for this node.
   PUT '{ "hello": "world" }' AS Json; -> Returns the unique ID for this node.
   PUT '{ "hello": "world" }' AS Json NAMED 'myJson'; -> Returns the label 'myJson'.

   Example valid mamml instructions for retrieval:

   GET 'myJson'; -> '{ "hello": "world" }'
   GET 'myJson.hello'; -> 'world'
   GET 'myJson.hello' AS Json; -> '{ "hello": "world" }'

   We can also just store primitives

   PUT '12345678' AS Number; -> Returns ID pointing to this node containing the number.
   PUT '12345678' AS Number NAMED 'myAge'; -> Returns the label 'myAge'.

   UPDATE 'myAge' TO '12393844893'; -> Update a given node by name. 

   CLEAR 'myAge'; -> Sets a node to Null.
   
   Null can be achieved also by:

   PUT Null AS Number; -> Id of new Null, number node.

   DELETE 'myAge'; -> Deletes a given node by name.  
 
   * * * * * WARNING * * * * *
   ...........................
   .......... W I P ..........
   ...........................
 *)

module Data = struct
  exception Invalid_type_from_string
  
  type t = 
    | Json of Yojson.Safe.t
    | Int of int
    | Float of float
    | Text of string
    | Uuid of string
    | Blob of bytes
    | Bool of bool              (* Stored as 1 or 0 *)
    | Date of int               (* Unix time *)
    | Null
    [@@deriving yojson]
  and node = { 
      (* What is cached *)
      mutable id : string [@key "id"];
      created_at : int [@key "created_at"];
      mutable raw : string [@key "raw"];
      mutable data : t [@key "data"];
    } [@@deriving yojson]

  let string_to_typed (d : string) = function
    | "json" ->
       if d = "" then
         Json (Yojson.Safe.from_string {|{ "placeholder": "placeholder" }|})
       else Json (Yojson.Safe.from_string d)
    | "text" -> Text d
    | "uuid" -> Uuid d
    | "int" -> Int (int_of_string d)
    | "float" -> Float (float_of_string d)
    | "blob" -> Blob (Bytes.of_string d)
    | "bool" -> Bool (bool_of_string d)
    | "date" -> Date (int_of_string d)
    | "null" -> Null
    | _ -> raise Invalid_type_from_string

  let string_of_type = function
    | Json t -> Yojson.Safe.to_string t
    | Int t -> string_of_int t
    | Float t -> string_of_float t
    | Text t -> t
    | Uuid t -> t
    | Blob t -> Bytes.to_string t
    | Bool t -> string_of_bool t
    | Date t -> string_of_int t
    | Null -> "null"

  let jsonify_node n =
    let handle_text id str =
      Printf.sprintf
      {|
      "%s": "%s",\r\n
      |} id str
    in
    let handle_non_text id str =
      Printf.sprintf
      {|
      "%s": %s,\r\n
      |} id str
    in
    match n.data with
    | Text t -> handle_text n.id t
    | Uuid t -> handle_text n.id t
    | _ -> handle_non_text n.id (string_of_type n.data)


  let is_valid_type_string = function
    | "json" -> true
    | "text" -> true
    | "uuid" -> true
    | "int" -> true
    | "float" -> true
    | "blob" -> true
    | "bool" -> true
    | "date" -> true
    | "null" -> true
    | _ -> false
end

(* Since the syntax is similar to SQL, and fairly simple, we can ignore lexing I think, and just parse. *)
module Parser = struct
  exception Parsing_error
  exception Invalid_token of string
  exception Invalid_type of string
  exception Invalid_string
  
  type action_token =
    | Get
    | Put
    | Update
    | To
    | Delete
    | As
    | Named

  let action_token_of_string = function
    | "get" -> Get
    | "put" -> Put
    | "to" -> To
    | "update" -> Update
    | "delete" -> Delete
    | "as" -> As
    | "named" -> Named
    | s -> raise (Invalid_token s)
  
  type ast = {
      action : action_token;
      data : string;
      next : ast option;
    }

  let string_quote = '`'
  let string_quote_str = (String.make 1 string_quote)

  let ends_with s c =
    match String.length s with
    | 0 -> raise Invalid_string
    | _ ->
       let len = (String.length s) - 1 in
       String.contains_from s len c

  let starts_with s c =
    match String.length s with
    | 0 -> raise Invalid_string
    | _ -> String.contains_from s 0 c

  let compile_quoted_string rest =
    let buff = Buffer.create 512 in
    let rec compile_rest = function
      | h :: t ->
         Buffer.add_string buff h;
         if not @@ ends_with h string_quote then Buffer.add_char buff ' ';
         if ends_with h string_quote then
           let result =
             Str.global_replace (Str.regexp string_quote_str) "" @@ Buffer.contents buff in
           (result, t)
         else compile_rest t
      | [] -> raise Invalid_string
    in compile_rest rest
          
  let parse_statement (statement : string) : ast =
    let tokens =
      statement
      |> Str.global_replace (Str.regexp ";") ""
      |> String.split_on_char ' '
    in
    let confirm_type t =
      if not @@ Data.is_valid_type_string t then raise (Invalid_type t);
    in
    let rec aux = function
      | a :: (b :: tail as rest) ->
         let action =
           a
           |> String.lowercase_ascii
           |> action_token_of_string
         in
         let data = ref b in
         let check_as_action () =
           data := String.lowercase_ascii !data;
           confirm_type !data;
         in
         if action = As then check_as_action ();
         if starts_with !data string_quote then
           let (content, rem) = compile_quoted_string rest in
           Some
             {
               action = action;
               data = content;
               next = aux rem
             }
         else
           Some
             {
               action = action;
               data = !data;
               next = aux tail
             }
      | [] -> None
      | _ -> raise Parsing_error
    in
    match aux tokens with
    | Some ast -> ast
    | None -> raise Parsing_error
end

module Core = struct
  exception Invalid_naming of string
  exception Invalid_action of string
  exception Invalid_json_target of string
  exception Type_mismatch
  exception Syntax_error
  
  open Data

  let root_map = Hashtbl.create ~random: true 512

  module Persist = struct
    let export map =
      let json_seq = Seq.map jsonify_node (Hashtbl.to_seq_values map) in
      let json = Seq.fold_left (fun a b -> b ^ a) "{\r\n" json_seq in
      String.(sub json 0 (length json - 3))  ^ "\r\n}"

    (* let import = () *)
  end

  (*
    (* Function to clone records, this may come in handy later if we want to add copy/paste *)
    let clone (type t) (x : t) : t =
      let buff = Marshal.(to_bytes x [No_sharing; Closures]) in
      Marshal.from_bytes buff 0
  *)

  let remove_char s c =
    let rgx = String.make 1 c in
    Str.(global_replace (regexp rgx) "" s)

  (* 
    Takes a list of keys and drills into the json to find the value:
    ie: ["hello"] + { "hello": "world" } = "world"
    ie2: ["hello", "friend"] + { "hello": { "friend": "my friend" } } = "my friend"
  *)
  let get_json_by_psv psv json =
    let open Yojson.Safe.Util in
    let result = List.fold_left (fun c a -> c |> member a) json psv in
    let open Yojson.Safe in
      match result with
      | `Assoc _ | `Variant _ | `List _ | `Null -> to_string result
      | _ -> remove_char (to_string result) '"' (* We don't want to return with "" around strings *)

  let get_json (target : node) : Data.t =
    let tokens = String.split_on_char '.' target.id in
    match tokens with
    | [] -> raise (Invalid_json_target target.id)
    | [_] -> raise (Invalid_json_target target.id)
    | h :: t ->
      let n = Hashtbl.find root_map h in
      match n.data with
      | Json d -> Text (get_json_by_psv t d)
      | _ -> raise Type_mismatch

  let get (target : node) : string =
    let data =
      if String.contains target.id '.' then
        get_json target
      else
        (Hashtbl.find root_map target.id).data
    in 
    string_of_type data

  let put (target : node) : string =
    if String.contains target.id '.' then
      raise (Invalid_naming target.id)
    else
      Hashtbl.add root_map target.id target; target.id

  let delete (target : node) : string =
    let old = get target in
    Hashtbl.remove root_map target.id; old

  let update (target : node) : string =
    Hashtbl.replace root_map target.id target; get target

  let check_exists (target : string) : unit =
    ignore @@ Hashtbl.find root_map target

  open Parser
  
  let eval_ast (a : ast) =
    let root_action = a.action in
    let acc = {
        id = string_of_int @@ Oo.id (object end);
        raw = "";
        data = Null;
        created_at = int_of_float @@ Unix.time ();
      }
    in
    let handle_no_next a =
      match a.action with
      | To -> acc.raw <- a.data; acc.data <- (string_to_typed acc.raw a.data)
      | As -> acc.data <- (string_to_typed acc.raw a.data)
      | Get -> acc.id <- a.data
      | Named -> acc.id <- a.data
      | Delete -> acc.id <- a.data
      | _ -> raise Syntax_error
    in
    let handle_next a =
      match a.action with
      | Update -> check_exists a.data; acc.id <- a.data
      | As -> acc.data <- (string_to_typed acc.raw a.data)
      | Get -> acc.id <- a.data
      | Put -> acc.raw <- a.data
      | _ -> raise Syntax_error
    in
    let rec aux (a : ast) =
      match a.next with
      | None -> handle_no_next a
      | Some next -> handle_next a; aux next
    in
    let res = aux a; (root_action, acc) in
    match res with
    | (Get, n) -> get n
    | (Put, n) -> put n
    | (Delete, n) -> delete n
    | (Update, n) -> update n
    | _ -> raise (Invalid_action "The root action of a command must be: GET, PUT, DELETE, or UPDATE")

  let get_input () =
    let statement = read_line () in
    let result = parse_statement statement in
    eval_ast result
end

let () =
  while true do
    print_endline @@
      try
        Core.get_input ()
      with t -> Printf.sprintf {|{"error":"%s"}|} @@ Printexc.to_string t
  done
