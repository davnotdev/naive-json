module MyLib.JsonValue (JsonValue (..)) where

import qualified Data.Map.Strict as Map

data JsonValue
  = Bool Bool
  | Number Int
  | String String
  | Array [JsonValue]
  | Object (Map.Map String JsonValue)
