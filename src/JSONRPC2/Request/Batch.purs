module JSONRPC2.Request.Batch where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
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
  -> Maybe (Array (Either RequestFormatError Request))
fromJson = map (map Request.fromJson) <<< Json.toArray

toJson :: Batch -> Json
toJson (Batch reqs) = Json.fromArray $ map Request.toJson reqs
