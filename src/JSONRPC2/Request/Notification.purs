module JSONRPC2.Request.Notification where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Record as Record
import Data.Symbol (SProxy(..))
import JSONRPC2.Request (Params, Request(..))
import JSONRPC2.Request as Request

newtype Notification = Notification {
    method :: String
  , params :: Maybe Params
}
derive instance newtypeNotification :: Newtype Notification _
derive instance genericNotification :: Generic Notification _
instance showNotification :: Show Notification where
  show = genericShow
instance eqNotification :: Eq Notification where
  eq = genericEq
instance ordNotification :: Ord Notification where
  compare = genericCompare

toRequest :: Notification -> Request
toRequest (Notification n) =
  Request $ Record.insert (SProxy :: SProxy "id") Nothing n

toJson :: Notification -> Json
toJson = Request.toJson <<< toRequest
