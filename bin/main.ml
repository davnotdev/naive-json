open Naive_json

(* Test Stringify *)
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

(* Test Everything*)
let () =
  let s =
    {|
    {
        "username": "david",
        "hello": ["array", 123, {
            "smuggled": "object",
            "in": true
        }]
    }
  |}
  in
  let tokens = Naive_json._tokenize_string s in
  let () =
    tokens |> List.map Naive_json.json_token_dump |> List.iter print_endline
  in
  let parsed = Naive_json._parse_json_tokens tokens in
  match parsed with
  | Ok parsed ->
      let () = print_endline "Parsing sucess" in
      let back_to_string = Naive_json.string_of_json parsed in
      print_endline back_to_string
  | Error (e, bt) -> Printf.printf "%s\n\n%s" e bt
