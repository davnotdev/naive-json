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

let parse_json s = Json.Null
let _stringify_func v f = f v
let _stringify_number v = _stringify_func v string_of_int
let _stringify_bool v = _stringify_func v string_of_bool
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
