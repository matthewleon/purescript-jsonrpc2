module JSONRPC2.Request where

import Prelude

import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as SM
import JSONRPC2.Constants as Constants
import JSONRPC2.Identifier (Identifier)
import JSONRPC2.Identifier as Id
import JSONRPC2.Json (Json, caseJson)
import JSONRPC2.Json as Json
import JSONRPC2.Protocol (ProtocolError, checkProtocol, protocolKey, protocolValue)

newtype Request = Request {
    id :: Maybe Identifier
  , method :: String
  , params :: Maybe Params
}
derive instance newtypeRequest :: Newtype Request _
instance showRequest :: Show Request
  where show (Request r) = "Request " <> show r
derive instance eqRequest :: Eq Request
derive instance ordRequest :: Ord Request

data Params = PArray (Array Json) | PObject (Object Json)
derive instance eqParams :: Eq Params
derive instance ordParams :: Ord Params
derive instance genericParams :: Generic Params _
instance showParams :: Show Params where
  show = genericShow

data RequestFormatError =
    ErrorNonObject
  | ProtocolError ProtocolError
  | ErrorMissingMethod
  | ErrorInvalidMethodType Json
  | ErrorInvalidIdType Json
  | ErrorInvalidParamsType Json
derive instance eqRequestFormatError :: Eq RequestFormatError
derive instance ordRequestFormatError :: Ord RequestFormatError
derive instance genericRequestFormatError :: Generic RequestFormatError _
instance showRequestFormatError :: Show RequestFormatError where
  show = genericShow

methodKey :: String
methodKey = "method"

paramsKey :: String
paramsKey = "params"

fromJson :: Json -> Either RequestFormatError Request
fromJson json = do
  reqMap <- note ErrorNonObject $ Json.toObject json
  lmap ProtocolError $ checkProtocol reqMap
  id <- getId reqMap
  method <- getMethod reqMap
  params <- getParams reqMap
  pure $ Request { id, method, params }

  where

  getId reqMap =
    case SM.lookup Constants.idKey reqMap of
         Nothing -> Right Nothing
         Just idJson -> Just <$> parseId idJson
    where
    parseId idJson = note (ErrorInvalidIdType idJson) $ Id.fromJson idJson

  getMethod reqMap = do
    methodJson <- note ErrorMissingMethod $ SM.lookup methodKey reqMap
    note (ErrorInvalidMethodType methodJson) $ Json.toString methodJson

  getParams reqMap = 
    case SM.lookup paramsKey reqMap of
         Nothing -> Right Nothing
         Just paramsJson -> Just <$> parseParams paramsJson
    where
    parseParams :: Json -> Either RequestFormatError Params
    parseParams paramsJson = note (ErrorInvalidParamsType paramsJson) $
      caseJson no no no no (Just <<< PArray) (Just <<< PObject) paramsJson
      where
      no :: forall a. a -> Maybe Params
      no _ = Nothing

toJson :: Request -> Json
toJson (Request req) = Json.fromObject $ toJObject req
  where
  toJObject {method, params, id} = SM.fromFoldable $ [
        (Tuple protocolKey $ Json.fromString protocolValue)
      , (Tuple methodKey $ Json.fromString method)
    ] <> maybeSingleton paramsKey paramsToJson params
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
