module Data = struct
  type numeric = Int of int | Float of float
  and datatype =
    | Json
    | Number
    | Text
    | Uuid
    | Blob
    | Bool
    | Date
    | Null
  and json = 
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Object of ( string * json ) list
    | `List of json list
    ]
  and t = 
    | Json of json
    | Number of numeric
    | Text of string
    | Uuid of string
    | Blob of bytes
    | Bool of bool              (* Stored as 1 or 0 *)
    | Date of int               (* Unix time *)
    | Null
end

(* 
   Example valid mamml instructions for storage:
   
   PUT '{ "hello": "world" }' AS Json; -> Returns the unique ID for this element.
   PUT '{ "hello": "world" }' AS Json NAMED 'myJson'; -> Returns the label 'myJson'.

   Example valid mamml instructions for retrieval:

   GET 'myJson'; -> '{ "hello": "world" }'
   GET 'myJson.hello'; -> 'world'

   We can also just store primitives

   PUT '12345678' AS Number; -> Returns ID pointing to this number.
   PUT '12345678' AS Number NAMED 'myAge'; -> Returns the label 'myAge'.

   UPDATE 'myAge' '12393844893'; -> Update a given element by name. 

   DELETE 'myAge'; -> Deletes a given element by name.
 
   ...........................
   .......... W I P ..........
   ...........................
 *)
module Core = struct
  open Data

  type request =
    [
    | `Get
    | `Put
    | `Update
    | `Delete
    ]
  
  let put = function
    | Json t -> "json"
    | Number t -> "number"
    | Text t -> t
    | Uuid t -> t
    | Blob t -> Bytes.to_string t
    | Bool t -> "true"
    | Date t -> "1992"
    | Null -> "Nothing"

  let get (target : string) : Data.t =
    Number (Int 123)
  
end

let () = print_endline "Hello World"
