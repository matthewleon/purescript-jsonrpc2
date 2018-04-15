module JSONRPC2.Response where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import JSONRPC2.Constants as Constants
import JSONRPC2.Identifier (Identifier)
import JSONRPC2.Identifier as Id
import JSONRPC2.Protocol (ProtocolError, checkProtocol, protocolKey, protocolValue)
import JSONRPC2.Request (Params, Request(..))
import JSONRPC2.Response.ErrorCode (ErrorCode)
import JSONRPC2.Response.ErrorCode as ErrorCode

data Response = Response Identifier (Either Error Result)

derive instance eqResponse :: Eq Response
derive instance ordResponse :: Ord Response
derive instance genericResponse :: Generic Response _
instance showReponse :: Show Response where
  show = genericShow

newtype Error = Error {
    code :: ErrorCode
  , message :: String
  , data :: Maybe Json
}
derive instance newtypeError :: Newtype Error _
derive instance genericError :: Generic Error _
instance showError :: Show Error where
  show = genericShow
instance eqError :: Eq Error where
  eq = genericEq
instance ordError :: Ord Error where
  compare = genericCompare

newtype Result = Result Json
derive newtype instance showResult :: Show Result
derive newtype instance eqResult :: Eq Result
derive newtype instance ordResult :: Ord Result

data ResponseFormatError =
    ErrorNonObject
  | ProtocolError ProtocolError
  | ErrorBothResultAndError
  | ErrorMissingResultOrError
  | ErrorInvalidJson Json
  | ErrorMissingId
  | ErrorInvalidIdType Json
derive instance eqResponseFormatError :: Eq ResponseFormatError
derive instance ordResponseFormatError :: Ord ResponseFormatError
derive instance genericResponseFormatError :: Generic ResponseFormatError _
instance showResponseFormatError :: Show ResponseFormatError where
  show = genericShow

fromJson :: Json -> Either ResponseFormatError Response
fromJson json = do
  respMap <- note ErrorNonObject $ Json.toObject json
  lmap ProtocolError $ checkProtocol respMap
  Response <$> getId respMap <*> getResultOrError respMap

  where

  getId respMap = do
     idJson <- note ErrorMissingId $ SM.lookup Constants.idKey respMap
     note (ErrorInvalidIdType idJson) $ Id.fromJson idJson

  getResultOrError respMap =
    let mResult = SM.lookup "result" respMap
        mError = SM.lookup "error" respMap
    in case mResult, mError of
         Just result, Just error -> Left ErrorBothResultAndError
         Nothing, Just errJson -> Left <<< Error <$> getError errJson
         Just result, Nothing -> Right $ Right $ Result result
         Nothing, Nothing -> Left ErrorMissingResultOrError

    where

    getError errJson = note (ErrorInvalidJson errJson) do
       errMap <- Json.toObject errJson
       let d = SM.lookup "data" errMap
       code <- ErrorCode.fromNumber
               <$> (Json.toNumber =<< SM.lookup "code" errMap)
       message <- Json.toString =<< SM.lookup "message" errMap
       pure {code, message, data: d}

toJson :: Response -> Json
toJson (Response id errOrResult) = Json.fromObject $ SM.fromFoldable [
      (Tuple protocolKey $ Json.fromString protocolValue)
    , (Tuple Constants.idKey $ Id.toJson id)
    , toTuple errOrResult
  ]

    where
    toTuple :: Either Error Result -> Tuple String Json
    toTuple (Left (Error {code, message, data: mData})) =
      Tuple "error" $ Json.fromObject $ SM.fromFoldable $ [
          (Tuple "code" $ Json.fromNumber $ ErrorCode.toNumber code)
        , (Tuple "message" $ Json.fromString $ message)
      ] <> maybe [] (A.singleton <<< Tuple "data") mData
    toTuple (Right (Result resultJson)) = Tuple "result" resultJson

respond
  :: ({ method :: String, params :: Maybe Params } -> Either Error Result)
  -> Request
  -> Maybe Response
respond f (Request { id, method, params }) = case id of
  Just id' -> Just $ Response id' $ f { method, params }
  Nothing -> Nothing
