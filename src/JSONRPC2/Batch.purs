module JSONRPC2.Batch (
    toJson
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Newtype (class Newtype, unwrap)

toJson
  :: forall batch t
   . Newtype batch (Array t)
  => (t -> Json) -> batch -> Json
toJson tToJson = Json.fromArray <<< map tToJson <<< unwrap
