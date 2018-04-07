module JSONRPC2.Identifier where

import Prelude

import Data.Argonaut.Core (Json, foldJson, jsonNull)
import Data.Argonaut.Core as Json
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
fromJson = foldJson
  (const $ Just IdNull)
  (const Nothing)
  (Just <<< IdNum)
  (Just <<< IdStr)
  (const Nothing)
  (const Nothing)
