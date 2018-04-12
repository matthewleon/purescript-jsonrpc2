module JSONRPC2.Response where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (class Newtype)
import Data.StrMap as SM
import JSONRPC2.Constants as Constants
import JSONRPC2.Identifier (Identifier)
import JSONRPC2.Identifier as Identifier
import JSONRPC2.Request (Params, Request(..))

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

data ErrorCode =
    CodeParseError
  | CodeInvalidRequest
  | CodeMethodNotFound
  | CodeInvalidParams
  | CodeInternalError
  | CodeServerError Number
  | CodeOther Number
derive instance eqErrorCode :: Eq ErrorCode
derive instance ordErrorCode :: Ord ErrorCode
derive instance genericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow

errorCodeExplanation :: ErrorCode -> String
errorCodeExplanation = case _ of
  CodeParseError -> "Invalid JSON was received by the server, \
                    \or an error occured on the server \
                    \while parsing the JSON text."
  CodeInvalidRequest -> "Invalid request: \
                       \The JSON sent is not a valid request object."
  CodeMethodNotFound -> "Method not found: \
                        \The method does not exist / is not available."
  CodeInvalidParams -> "Invalid params: Invalid method parameter(s)."
  CodeInternalError -> "Internal error: Internal JSON-RPC error."
  CodeServerError n -> "Server error: "
                       <> (maybe' (\_ -> show n) show $ fromNumber n)
  CodeOther n -> "Other error (check message): " <> show n

data ResponseFormatError =
    ErrorNonObject
  | ErrorMissingProtocol
  | ErrorInvalidProtocolType Json
  | ErrorInvalidProtocol String
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
  checkProtocol respMap
  Response <$> getId respMap <*> getResultOrError respMap

  where

  checkProtocol respMap = do
    protocolJson <- note ErrorMissingProtocol
      $ SM.lookup Constants.protocolKey respMap
    protocolStr <- note (ErrorInvalidProtocolType protocolJson)
      $ Json.toString protocolJson
    if protocolStr == Constants.protocolValue
      then Right unit
      else Left $ ErrorInvalidProtocol protocolStr

  getId respMap = do
     idJson <- note ErrorMissingId $ SM.lookup Constants.idKey respMap
     note (ErrorInvalidIdType idJson) $ Identifier.fromJson idJson

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
       code <- errorCodeFromNumber
               <$> (Json.toNumber =<< SM.lookup "code" errMap)
       message <- Json.toString =<< SM.lookup "message" errMap
       pure {code, message, data: d}

      where

      errorCodeFromNumber :: Number -> ErrorCode
      errorCodeFromNumber = case _ of
        -32700.0 -> CodeParseError
        -32600.0 -> CodeInvalidRequest
        -32601.0 -> CodeMethodNotFound
        -32602.0 -> CodeInvalidParams
        -32603.0 -> CodeInternalError
        code -> code # if (code >= -3200.0) && (code <= -32099.0)
                  then CodeServerError
                  else CodeOther

respond
  :: ({ method :: String, params :: Maybe Params } -> Either Error Result)
  -> Request
  -> Maybe Response
respond f (Request { id, method, params }) = case id of
  Just id' -> Just $ Response id' $ f { method, params }
  Nothing -> Nothing
