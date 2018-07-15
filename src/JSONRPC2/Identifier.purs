module JSONRPC2.Identifier where

import Prelude

import JSONRPC2.Json (Json, caseJson, jsonNull)
import JSONRPC2.Json as Json
import Data.Maybe (Maybe(..))

data Identifier = IdStr String | IdNum Number | IdNull
derive instance eqIdentifier :: Eq Identifier
derive instance ordIdentifier :: Ord Identifier

instance showIdentifier :: Show Identifier where
  show (IdStr s) = s
  show (IdNum n) = show n
  show IdNull = "null"

toJson :: Identifier -> Json
toJson (IdStr s) = Json.fromString s
toJson (IdNum n) = Json.fromNumber n
toJson IdNull = jsonNull

fromJson :: Json -> Maybe Identifier
fromJson = caseJson
  (const $ Just IdNull)
  (const Nothing)
  (Just <<< IdNum)
  (Just <<< IdStr)
  (const Nothing)
  (const Nothing)
