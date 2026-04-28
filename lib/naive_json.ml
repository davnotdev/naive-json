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

let _error_make s =
  let bt = Printexc.get_callstack 10 in
  Error (s, Printexc.raw_backtrace_to_string bt)

let _error_unexpected_end_of_tokens = _error_make "Unexpected end of tokens"

let _error_unexpected_token token =
  _error_make (Printf.sprintf "Unexpected token %s" (json_token_dump token))

let _explode s = List.init (String.length s) (String.get s)

let _tokenize_string s =
  let char_list = _explode (String.trim s) in
  let rec _tokenize_inner char_list expr_accum string_state =
    match char_list with
    | [] -> []
    | h :: t -> (
        let finish_ss token string_state next_string_state =
          (if List.is_empty expr_accum then [ token ]
           else
             let expr_string =
               String.of_seq (List.to_seq (List.rev expr_accum))
             in
             if
               (not string_state) && String.length (String.trim expr_string) = 0
             then [ token ]
             else [ JsonToken.Expression expr_string; token ])
          @ _tokenize_inner t [] next_string_state
        in
        let finish token = finish_ss token string_state string_state in
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
              string_state (not string_state)
        | x -> _tokenize_inner t (x :: expr_accum) string_state)
  in
  _tokenize_inner char_list [] false

let _parse_string tokens =
  match tokens with
  | JsonToken.StringOpen :: JsonToken.Expression s :: JsonToken.StringClose :: t
    ->
      Ok (Json.String s, t)
  | token :: _ -> _error_unexpected_token token
  | [] -> _error_unexpected_end_of_tokens

let _parse_expression expr remaining =
  match String.trim expr with
  | "true" -> Ok (Json.Bool true, remaining)
  | "false" -> Ok (Json.Bool false, remaining)
  | "null" -> Ok (Json.Null, remaining)
  | _ -> (
      match int_of_string_opt (String.trim expr) with
      | Some num -> Ok (Json.Number num, remaining)
      | None -> _error_make "Invalid Json.Number")

let ( let* ) r f = match r with Error e -> Error e | Ok v -> f v

let rec _parse_any token remaining =
  match token with
  | JsonToken.ObjectOpen -> _parse_object (token :: remaining)
  | JsonToken.ArrayOpen -> _parse_array (token :: remaining)
  | JsonToken.StringOpen -> _parse_string (token :: remaining)
  | JsonToken.Expression e -> _parse_expression e remaining
  | _ -> _error_unexpected_token token

and _parse_object_inner tokens =
  match tokens with
  | JsonToken.StringOpen
    :: JsonToken.Expression key
    :: JsonToken.StringClose :: JsonToken.Colon :: h :: t -> (
      let* v, remaining = _parse_any h t in
      match remaining with
      | JsonToken.ObjectClose :: t -> Ok ([ (key, v) ], t)
      | JsonToken.Comma :: rest ->
          let* r, t = _parse_object_inner rest in
          Ok ((key, v) :: r, t)
      | token :: _ -> _error_unexpected_token token
      | [] -> _error_unexpected_end_of_tokens)
  | token :: _ -> _error_unexpected_token token
  | [] -> _error_unexpected_end_of_tokens

and _parse_object tokens =
  match tokens with
  | JsonToken.ObjectOpen :: t ->
      let* pairs, remaining = _parse_object_inner t in
      let map =
        List.fold_left
          (fun acc (k, v) -> StringMap.add k v acc)
          StringMap.empty pairs
      in
      Ok (Json.Object map, remaining)
  | token :: _ -> _error_unexpected_token token
  | [] -> _error_unexpected_end_of_tokens

and _parse_array_inner h tokens =
  let* v, remaining = _parse_any h tokens in
  match remaining with
  | JsonToken.ArrayClose :: t -> Ok ([ v ], t)
  | JsonToken.Comma :: h :: rest ->
      let* next, remaining = _parse_array_inner h rest in
      Ok (v :: next, remaining)
  | token :: _ -> _error_unexpected_token token
  | [] -> _error_unexpected_end_of_tokens

and _parse_array tokens =
  match tokens with
  | JsonToken.ArrayOpen :: h :: t ->
      let* items, remaining = _parse_array_inner h t in
      Ok (Json.Array items, remaining)
  | token :: t -> _error_unexpected_token token
  | [] -> _error_unexpected_end_of_tokens

let _parse_json_tokens tokens =
  let* v, _ = _parse_object tokens in
  Ok v

let parse_json s =
  let tokens = _tokenize_string s in
  _parse_json_tokens tokens

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
