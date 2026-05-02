module MyLib.JsonError (JsonError (..), showJsonError) where

data JsonError
  = UnexpectedToken String
  | Unreachable

showJsonError :: JsonError -> String
showJsonError e = case e of
  UnexpectedToken msg -> msg
  Unreachable -> "Hit unreachable error"
