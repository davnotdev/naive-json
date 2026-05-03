module Main where

import qualified Data.Map.Strict as Map
import qualified MyLib as Json
import qualified MyLib.JsonError as JsonError
import qualified MyLib.JsonToken as JsonToken
import qualified MyLib.JsonValue as JsonValue

resultString :: Either JsonError.JsonError String -> String
resultString v =
  case v of
    Left e -> JsonError.showJsonError e
    Right ss -> ss

listString :: (a -> String) -> [a] -> String
listString f = foldl (\acc v -> acc ++ f v ++ "\n") ""

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let testStringifyObject =
        JsonValue.Object
          ( Map.fromList
              [ ("k1", JsonValue.Bool True),
                ("k2", JsonValue.Number 10),
                ( "k3",
                  JsonValue.Array
                    [JsonValue.Bool False, JsonValue.String "Hello"]
                )
              ]
          )
  putStrLn (resultString (Json.showJsonValue testStringifyObject))

  let testParseString =
        "{\n\
        \    \"hello\": \"world\",\n\
        \    \"array\": [\"one\", 2, false],\n\
        \    \"object\": {\n\
        \       \"key\": false\n\
        \    }\n\
        \}"

  putStrLn (listString JsonToken.showJsonToken (Json.tokenizeJsonString testParseString))

  return ()
