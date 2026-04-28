module StringMap = Map.Make (String)

module Json = struct
  type t =
    | Object of t StringMap.t
    | Array of t list
    | String of string
    | Number of int
    | Bool of bool
    | Null
end

module JsonToken = struct
  type t =
    | ObjectOpen
    | ObjectClose
    | ArrayOpen
    | ArrayClose
    | Colon
    | Comma
    | StringOpen
    | StringClose
    | Expression of string
end

let json_token_dump = function
  | JsonToken.ObjectOpen -> "ObjectOpen"
  | JsonToken.ObjectClose -> "ObjectClose"
  | JsonToken.ArrayOpen -> "ArrayOpen"
  | JsonToken.ArrayClose -> "ArrayClose"
  | JsonToken.Colon -> "Colon"
  | JsonToken.Comma -> "Comma"
  | JsonToken.StringOpen -> "StringOpen"
  | JsonToken.StringClose -> "StringClose"
  | JsonToken.Expression s -> "Expr(" ^ s ^ ")"

let _error_unexpected_end_of_tokens = Error "Unexpected end of tokens"

let _error_unexpected_token token =
  Error (Printf.sprintf "Unexpected token %s" (json_token_dump token))

let _explode s = List.init (String.length s) (String.get s)

let _tokenize_string s =
  let char_list = _explode (String.trim s) in
  let rec _tokenize_inner char_list expr_accum string_state =
    match char_list with
    | [] -> []
    | h :: t -> (
        let finish_ss token string_state =
          (if List.is_empty expr_accum then [ token ]
           else
             [
               JsonToken.Expression
                 (String.of_seq (List.to_seq (List.rev expr_accum)));
               token;
             ])
          @ _tokenize_inner t [] string_state
        in
        let finish token = finish_ss token string_state in
        match h with
        | '{' -> finish JsonToken.ObjectOpen
        | '}' -> finish JsonToken.ObjectClose
        | '[' -> finish JsonToken.ArrayOpen
        | ']' -> finish JsonToken.ArrayClose
        | ':' -> finish JsonToken.Colon
        | ',' -> finish JsonToken.Comma
        | '"' ->
            finish_ss
              (if string_state then JsonToken.StringClose
               else JsonToken.StringOpen)
              (not string_state)
        | x -> _tokenize_inner t (x :: expr_accum) string_state)
  in
  _tokenize_inner char_list [] false

let _parse_string tokens =
  match tokens with
  | JsonToken.StringOpen :: JsonToken.Expression s :: JsonToken.StringClose :: t
    ->
      Ok (Json.String s)
  | token :: _ -> _error_unexpected_token token
  | [] -> _error_unexpected_end_of_tokens

let _parse_expression expr =
  match expr with
  | "true" -> Ok (Json.Bool true)
  | "false" -> Ok (Json.Bool false)
  | "null" -> Ok Json.Null
  | _ -> (
      match int_of_string_opt (String.trim expr) with
      | Some num -> Ok (Json.Number num)
      | None -> Error "Invalid Json.Number")

let ( let* ) r f = match r with Error e -> Error e | Ok v -> f v

let rec _parse_object_inner tokens =
  match tokens with
  | JsonToken.StringOpen
    :: JsonToken.Expression key
    :: JsonToken.StringClose :: JsonToken.Colon :: h :: t -> (
      let* v =
        match h with
        | JsonToken.ObjectOpen -> _parse_object (h :: t)
        | JsonToken.ArrayOpen -> _parse_array (h :: t)
        | JsonToken.StringOpen -> _parse_string (h :: t)
        | JsonToken.Expression e -> _parse_expression e
        | _ -> _error_unexpected_token h
      in
      match t with
      | JsonToken.ObjectClose :: _ -> Ok [ (key, v) ]
      | JsonToken.Comma :: rest ->
          let* r = _parse_object_inner rest in
          Ok ((key, v) :: r)
      | token :: _ -> _error_unexpected_token token
      | [] -> _error_unexpected_end_of_tokens)
  | token :: _ -> _error_unexpected_token token
  | [] -> _error_unexpected_end_of_tokens

and _parse_object tokens =
  match tokens with
  | JsonToken.ObjectOpen :: t ->
      let* pairs = _parse_object_inner tokens in
      let map =
        List.fold_left
          (fun acc (k, v) -> StringMap.add k v acc)
          StringMap.empty pairs
      in
      Ok (Json.Object map)
  | token :: _ -> _error_unexpected_token token
  | [] -> _error_unexpected_end_of_tokens

and _parse_array tokens = Ok Json.Null

let _parse_json_tokens tokens = [ Json.Null ]
let parse_json s = Json.Null

(* string_of_json implementation *)

let _stringify_number v = string_of_int v
let _stringify_bool v = string_of_bool v
let _stringify_null = "null"
let _stringify_string v = "\"" ^ v ^ "\""

let rec _stringify_inner v =
  match v with
  | Json.Null -> _stringify_null
  | Json.Bool v -> _stringify_bool v
  | Json.Number v -> _stringify_number v
  | Json.String v -> _stringify_string v
  | Json.Array arr -> _stringify_array "" arr
  | Json.Object obj -> _stringify_object "" obj

and _stringify_array s arr =
  let rec _stringify_array_inner s arr =
    match arr with
    | [] -> s
    | h :: t ->
        s ^ _stringify_inner h
        ^ (if List.length t != 0 then ", " else "")
        ^ _stringify_array_inner s t
  in
  "[" ^ _stringify_array_inner s arr ^ "]"

and _stringify_object s (obj : Json.t StringMap.t) =
  let ending =
    StringMap.fold
      (fun k v acc ->
        acc ^ _stringify_string k ^ ":" ^ _stringify_inner v ^ ", ")
      obj s
  in
  "{"
  ^ (if StringMap.is_empty obj then ""
     else String.sub ending 0 (String.length ending - 2))
  ^ "}"

let string_of_json obj = _stringify_inner obj
