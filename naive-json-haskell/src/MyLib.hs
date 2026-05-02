module MyLib (showJsonValue) where

import qualified Data.Map.Strict as Map
import MyLib.JsonError as JsonError
import MyLib.JsonValue as JsonValue

showJsonValueObjectInner :: Map.Map String JsonValue -> Either JsonError String
showJsonValueObjectInner object = go (Map.toList object) ""
  where
    go [] acc = Right acc
    go ((k, v) : t) acc =
      case showJsonValue v of
        Left e -> Left e
        Right vs -> go t (acc ++ k ++ ": " ++ vs ++ if null t then "" else ", ")

showJsonValueArrayInner :: [JsonValue] -> Either JsonError String
showJsonValueArrayInner array = go array ""
  where
    go [] acc = Right acc
    go (h : t) acc =
      case showJsonValue h of
        Left e -> Left e
        Right s -> go t (acc ++ s ++ if null t then "" else ", ")

showJsonValue :: JsonValue -> Either JsonError String
showJsonValue value = case value of
  JsonValue.Bool b -> Right (if b then "true" else "false")
  JsonValue.Number n -> Right (show n)
  JsonValue.String s -> Right s
  JsonValue.Array a -> fmap (\s -> "[" ++ s ++ "]") (showJsonValueArrayInner a)
  JsonValue.Object o -> fmap (\s -> "{" ++ s ++ "}") (showJsonValueObjectInner o)
