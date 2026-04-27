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
