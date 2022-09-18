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
  type json = 
    [
    | `Null
    | `Bool of datatype
    | `Int of int
    | `Float of float
    | `String of string
    | `Object of ( string * json ) list
    | `List of json list
    ]
  and t = 
    | Json of json
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
    | "json" -> Json @@ json_of_string d
    | "number" -> Number @@ num_of_string d
    | "text" -> Text d
    | "uuid" -> Uuid d
    | "int" -> Int @@ int_of_string d
    | "float" -> Float @@ float_of_string d
    | "blob" -> Blob @@ Bytes.bytes_of_string d
    | "bool" -> Bool @@ bool_of_string d
    | "date" -> Date @@ int_of_string d
    | "null" -> Null
    | _ -> failwith "Invalid type, this should have been caught by the parser!"

  let string_of_type = function
    | Json _ -> "json"
    | Int _ -> "int"
    | Float _ -> "float"
    | Text _ -> "text"
    | Uuid _ -> "uuid"
    | Blob _ -> "blob"
    | Bool _ -> "bool"
    | Date _ -> "date"
    | Null -> "null"
end

(* Since the syntax is similar to SQL, and fairly simple, we can ignore lexing I think, and just parse. *)
module Parser = struct
  type request_token =
    | Get
    | Put
    | Update
    | To
    | Delete
    | As
    | Named

  let request_token_of_string = function
    | "get" -> Get
    | "put" -> Put
    | "update" -> Update
    | "delete" -> Delete
    | "as" -> As
    | "named" -> Named
  
  type ast = {
      request : request_token;
      data : string;
      next : ast option;
    }  
end

module Core = struct
  open Data

  let root_map = Hashtbl.create ~random: true 512
  
  let put n =
    let () = match n.data with
    | Json t -> "json"
    | Number t -> "number"
    | Text t -> t
    | Uuid t -> t
    | Blob t -> Bytes.to_string t
    | Bool t -> "true"
    | Date t -> "1992"
    | Null -> "Nothing"
    in n.id

  let get (target : node) : Data.t =
    Number (Int 123)

  let delete (target : string) : bool option =    
    None

  let update (target : string) : bool option =
    None

  open Parser
  (* Evaluate the AST and return the node, plus the action to take: (action, node) *)
  let eval_ast (a : ast) =
    let root_action = a.request in
    let acc = {
        id = string_of_int @@ Oo.id (object end);
        raw = "";
        data = Null;
        created_at = int_of_float @@ Unix.time ();
      }
    in  
    let rec aux = function
      | { req, data, None } ->
         match req with
         | To -> acc.raw <- data; acc.data <- (string_to_typed acc.raw)
         | As -> acc.data <- (string_to_typed acc.raw)
         | Named -> acc.id <- data
         | _ -> failwith "Impossible"
      | { req, data, Some next } -> (* The parser should ensure that singularly evaluated expressions (like DELETE) never hit this match *)
         match req with
         | Update -> check_exists data; aux next
         | As -> acc.data <- (string_to_typed acc.raw); aux next
         | _ -> acc.raw <- data; aux next
    in
    let res = aux a; (root_action, acc) in
    match res with
    | (Get, n) -> get n
    | (Put, n) -> put n
end

let () = print_endline "Hello World"
