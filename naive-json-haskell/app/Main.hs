module Main where

import qualified Data.Map.Strict as Map
import qualified MyLib as Json
import qualified MyLib.JsonError as JsonError
import qualified MyLib.JsonValue as JsonValue

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let v =
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
  let s =
        ( case Json.showJsonValue v of
            Left e -> JsonError.showJsonError e
            Right ss -> ss
        )
  putStrLn s
  return ()
