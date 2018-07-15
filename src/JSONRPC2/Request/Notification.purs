module JSONRPC2.Request.Notification where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import JSONRPC2.Json (Json)
import JSONRPC2.Request (Params, Request(..))
import JSONRPC2.Request as Request
import Record as Record

newtype Notification = Notification {
    method :: String
  , params :: Maybe Params
}
derive instance newtypeNotification :: Newtype Notification _
derive instance eqNotification :: Eq Notification
derive instance ordNotification :: Ord Notification
instance showNotification :: Show Notification where
  show (Notification n) = "Notification " <> show n

toRequest :: Notification -> Request
toRequest (Notification n) =
  Request $ Record.insert (SProxy :: SProxy "id") Nothing n

toJson :: Notification -> Json
toJson = Request.toJson <<< toRequest
