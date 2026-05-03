module MyLib (showJsonValue, tokenizeJsonString) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import MyLib.JsonError as JsonError
import MyLib.JsonToken as JsonToken
import MyLib.JsonValue as JsonValue

tokenizeJsonString :: String -> [JsonToken]
tokenizeJsonString s =
  go s False []
  where
    flushExprAccumWith token f exprAccum =
      if null exprAccum
        then
          JsonToken.Expression exprAccum : token : f []
        else
          token : f []

    go "" _ _ = []
    go (c : t) stringState exprAccum =
      case c of
        '{' -> flushExprAccumWith JsonToken.ObjectOpen (go t stringState) exprAccum
        '}' -> flushExprAccumWith JsonToken.ObjectClose (go t stringState) exprAccum
        '[' -> flushExprAccumWith JsonToken.ArrayOpen (go t stringState) exprAccum
        ']' -> flushExprAccumWith JsonToken.ArrayClose (go t stringState) exprAccum
        ',' -> flushExprAccumWith JsonToken.Comma (go t stringState) exprAccum
        ':' -> flushExprAccumWith JsonToken.Colon (go t stringState) exprAccum
        '"' ->
          flushExprAccumWith
            (if stringState then JsonToken.StringOpen else JsonToken.StringClose)
            (go t (not stringState))
            exprAccum
        cc ->
          if isSpace cc && not stringState
            then go t stringState exprAccum
            else go t stringState (cc : exprAccum)

showJsonValueObjectInner :: Map.Map String JsonValue -> Either JsonError String
showJsonValueObjectInner object = go (Map.toList object) ""
  where
    go [] acc = Right acc
    go ((k, v) : t) acc =
      case showJsonValue v of
        Left e -> Left e
        Right vs -> go t (acc ++ "\"" ++ k ++ "\": " ++ vs ++ if null t then "" else ", ")

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
  JsonValue.String s -> Right ('\"' : s ++ "\"")
  JsonValue.Array a -> fmap (\s -> "[" ++ s ++ "]") (showJsonValueArrayInner a)
  JsonValue.Object o -> fmap (\s -> "{" ++ s ++ "}") (showJsonValueObjectInner o)
