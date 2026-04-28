open Naive_json

(* Test Stringify *)
let () = print_endline "Testing stringify\n"
let () =
  let obj =
    Json.Object
      (StringMap.empty
      |> StringMap.add "username" (Json.String "david")
      |> StringMap.add "password" (Json.Number 123)
      |> StringMap.add "knows_ocaml" (Json.Bool false)
      |> StringMap.add "brain_matter" Json.Null
      |> StringMap.add "other_stuff"
           (Json.Object
              (StringMap.empty
              |> StringMap.add "reverse" (Json.Number 321)
              |> StringMap.add "empty" (Json.Array [])
              |> StringMap.add "not_empty"
                   (Json.Array
                      [ Json.Number 123; Json.Array [ Json.Number 54321 ] ]))))
  in
  let s = Naive_json.string_of_json obj in
  print_endline s

(* Test Tokenizer *)
let () = print_endline "\n\nTesting tokenizer\n"
let () =
  let s = {|
    {
        "username": "david",
        "hello": ["array", 123]
    }
  |} in
  Naive_json._tokenize_string s
  |> List.map Naive_json.json_token_dump
  |> List.iter print_endline
