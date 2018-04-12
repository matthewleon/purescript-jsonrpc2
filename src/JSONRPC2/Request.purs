module JSONRPC2.Request where

import Prelude

import Data.Argonaut.Core (JArray, JObject, Json)
import Data.Argonaut.Core as Json
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import JSONRPC2.Constants as Constants
import JSONRPC2.Identifier (Identifier)
import JSONRPC2.Identifier as Id

newtype Request = Request {
    id :: Maybe Identifier
  , method :: String
  , params :: Maybe Params
}
derive instance newtypeRequest :: Newtype Request _
derive instance genericRequest :: Generic Request _
instance showRequest :: Show Request where
  show = genericShow
instance eqRequest :: Eq Request where
  eq = genericEq
instance ordRequest :: Ord Request where
  compare = genericCompare

data Params = PArray JArray | PObject JObject
derive instance eqParams :: Eq Params
derive instance ordParams :: Ord Params
derive instance genericParams :: Generic Params _
instance showParams :: Show Params where
  show = genericShow

toJson :: Request -> Json
toJson (Request req) = Json.fromObject $ toJObject req
  where
  toJObject {method, params, id} = StrMap.fromFoldable $ [
        (Tuple Constants.protocolKey $ Json.fromString Constants.protocolValue)
      , (Tuple "method" $ Json.fromString method)
    ] <> maybeSingleton "params" paramsToJson params
      <> maybeSingleton Constants.idKey Id.toJson id
    
    where
    maybeSingleton
      :: forall a b
       . String -> (a -> b) -> (Maybe a) -> Array (Tuple String b)
    maybeSingleton label f x
      = maybe [] (A.singleton <<< Tuple label <<< f) x

paramsToJson :: Params -> Json
paramsToJson (PArray jarr) = Json.fromArray jarr
paramsToJson (PObject jobj) = Json.fromObject jobj

paramsFromJson :: Json -> Maybe Params
paramsFromJson json = case Json.toArray json of
  Just jarr -> Just $ PArray jarr
  Nothing -> PObject <$> Json.toObject json
