module JSONRPC2.Request.Batch (
    Batch
  , fromJson
  , toJson
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import JSONRPC2.Batch as Batch
import JSONRPC2.Request (Request, RequestFormatError)
import JSONRPC2.Request as Request

newtype Batch = Batch (Array Request)
derive instance newtypeBatch :: Newtype Batch _
derive instance genericBatch :: Generic Batch _
instance showBatch :: Show Batch where
  show = genericShow
instance eqBatch :: Eq Batch where
  eq = genericEq
instance ordBatch :: Ord Batch where
  compare = genericCompare

fromJson
  :: Json
  -> Maybe (Either (Array (Either RequestFormatError Request)) Batch)
fromJson = Batch.fromJson Request.fromJson

toJson :: Batch -> Json
toJson = Batch.toJson Request.toJson
