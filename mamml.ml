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
  and node = { (* What is cached *)
      mutable id : string;
      created_at : int;
      mutable raw : string;
      mutable data : t;
    }    

  let string_to_typed (d : string) = function
    | "json" -> Json (Yojson.Safe.from_string d)
    | "text" -> Text d
    | "uuid" -> Uuid d
    | "int" -> Int (int_of_string d)
    | "float" -> Float (float_of_string d)
    | "blob" -> Blob (Bytes.of_string d)
    | "bool" -> Bool (bool_of_string d)
    | "date" -> Date (int_of_string d)
    | "null" -> Null
    | _ -> failwith "Invalid type, this should have been caught by the parser!"

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
end

(* Since the syntax is similar to SQL, and fairly simple, we can ignore lexing I think, and just parse. *)
module Parser = struct
  exception Parsing_exception
  
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
    | _ -> failwith "Invalid token"
  
  type ast = {
      action : action_token;
      data : string;
      next : ast option;
    }

  let parse_statement (statement : string) : ast =
    let tokens =
      statement
      |> Str.global_replace (Str.regexp ";") ""
      |> String.lowercase_ascii
      |> String.split_on_char ' '
      |> List.map (fun a -> Str.(global_replace (regexp "'") "" a))
    in
    let rec aux = function
      | a :: (b :: t) ->
         Some
           {
             action = action_token_of_string a;
             data = b;
             next = aux t 
           }
      | [] -> None
      | _ -> raise Parsing_exception
    in
    match aux tokens with
    | Some ast -> ast
    | None -> raise Parsing_exception
end

module Core = struct
  open Data

  let root_map = Hashtbl.create ~random: true 512

  let get (target : node) : string =
    let n = Hashtbl.find root_map target.id in
    string_of_type n.data
  
  let put (target : node) : string =
    Hashtbl.add root_map target.id target; target.id

  let delete (target : node) : string =    
    Hashtbl.remove root_map target.id; get target

  let update (target : node) : string =
    Hashtbl.replace root_map target.id target; get target

  let check_exists (target : string) : unit =
    ignore @@ Hashtbl.find root_map target

  open Parser
  (* Evaluate the AST and return the node, plus the action to take: (action, node) *)
  let eval_ast (a : ast) =
    let root_action = a.action in
    let acc = {
        id = string_of_int @@ Oo.id (object end);
        raw = "";
        data = Null;
        created_at = int_of_float @@ Unix.time ();
      }
    in
    (* The parser should ensure that singularly evaluated expressions (like DELETE) never hit this match *)
    let handle_no_next a =
      match a.action with
      | To -> acc.raw <- a.data; acc.data <- (string_to_typed acc.raw a.data)
      | As -> acc.data <- (string_to_typed acc.raw a.data)
      | Get -> acc.id <- a.data
      | Named -> acc.id <- a.data
      | Delete -> acc.id <- a.data
      | _ -> failwith "Impossible"
    in
    let handle_next a =
      match a.action with
      | Update -> check_exists a.data
      | As -> acc.data <- (string_to_typed acc.raw a.data)
      | Get -> acc.id <- a.data
      | Put -> acc.raw <- a.data
      | _ -> failwith "Impossible"
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
    | _ -> failwith "Invalid root action, can only be: GET, PUT, DELETE, UPDATE"

  let get_input () =
    let statement = read_line () in
    let result = parse_statement statement in
    eval_ast result
end

let () =
  while true do
        let result = Core.get_input() in
            print_endline result
  done
