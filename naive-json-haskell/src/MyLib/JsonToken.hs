module MyLib.JsonToken (JsonToken (..), showJsonToken) where

import Text.Printf

data JsonToken
  = ObjectOpen
  | ObjectClose
  | ArrayOpen
  | ArrayClose
  | StringOpen
  | StringClose
  | Expression String
  | Comma
  | Colon

showJsonToken :: JsonToken -> String
showJsonToken token = case token of
  ObjectOpen -> "ObjectOpen"
  ObjectClose -> "ObjectClose"
  ArrayOpen -> "ArrayOpen"
  ArrayClose -> "ArrayClose"
  StringOpen -> "StringOpen"
  StringClose -> "StringClose"
  Expression s -> printf "Expression String %s" s
  Comma -> "Comma"
  Colon -> "Colon"
