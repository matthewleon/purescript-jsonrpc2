module JSONRPC2.Batch (
    fromJson
  , toJson
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import JSONRPC2.Json (Json)
import JSONRPC2.Json as Json

fromJson
  :: forall batch t err
   . Newtype batch (Array t)
  => (Json -> Either err t)
  -> Json
  -> Maybe (Either (Array (Either err t)) batch)
fromJson tFromJson = map arrayToBatch <<< Json.toArray
  where
  arrayToBatch js = if A.null left then Right (wrap right) else Left eitherReqs
    where
    eitherReqs = tFromJson <$> js
    {left, right} = partitionMap identity eitherReqs

toJson
  :: forall batch t
   . Newtype batch (Array t)
  => (t -> Json) -> batch -> Json
toJson tToJson = Json.fromArray <<< map tToJson <<< unwrap
