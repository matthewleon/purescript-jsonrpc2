module JSONRPC2.Request.Batch (
    Batch
  , fromJson
  , toJson
  ) where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import JSONRPC2.Batch as Batch
import JSONRPC2.Json (Json)
import JSONRPC2.Request (Request, RequestFormatError)
import JSONRPC2.Request as Request

newtype Batch = Batch (Array Request)
derive instance newtypeBatch :: Newtype Batch _
derive instance genericBatch :: Generic Batch _
derive newtype instance eqBatch :: Eq Batch
derive newtype instance ordBatch :: Ord Batch
derive newtype instance showBatch :: Show Batch

fromJson
  :: Json
  -> Maybe (Either (Array (Either RequestFormatError Request)) Batch)
fromJson = Batch.fromJson Request.fromJson

toJson :: Batch -> Json
toJson = Batch.toJson Request.toJson
