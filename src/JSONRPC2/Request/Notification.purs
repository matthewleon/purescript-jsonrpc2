module JSONRPC2.Request.Notification where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))
import Data.Record as Record
import Data.Symbol (SProxy(..))
import JSONRPC2.Request (Params, Request)
import JSONRPC2.Request as Request

type Notification = {
    method :: String
  , params :: Maybe Params
}

toRequest :: Notification -> Request
toRequest = Record.insert (SProxy :: SProxy "id") Nothing

toJson :: Notification -> Json
toJson = Request.toJson <<< toRequest
