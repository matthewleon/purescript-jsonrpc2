module JSONRPC2.Response where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), maybe')
import Data.Record.Extra (eqRecord, compareRecord, showRecord)
import Data.StrMap as SM
import JSONRPC2.Constants as Constants
import JSONRPC2.Identifier (Identifier)
import JSONRPC2.Identifier as Identifier
import JSONRPC2.Request (Params, Request)

data Response = Response Identifier (Either Error Result)

instance eqResponse :: Eq Response where
  eq (Response id1 errRes1) (Response id2 errRes2) =
    id1 == id2 && eq' errRes1 errRes2

    where
    eq' (Left _) (Right _) = false
    eq' (Right _) (Left _) = false
    eq' (Left err1) (Left err2) = eqRecord err1 err2
    eq' (Right res1) (Right res2) = res1 == res2

instance ordResponse :: Ord Response where
  compare (Response id1 errRes1) (Response id2 errRes2) =
    case compare id1 id2 of
         EQ -> compare' errRes1 errRes2
         ord -> ord

    where
    compare' (Left _) (Right _) = LT
    compare' (Right _) (Left _) = GT
    compare' (Left err1) (Left err2) = compareRecord err1 err2
    compare' (Right res1) (Right res2) = compare res1 res2

instance showResponse :: Show Response where
  show (Response id eitherErrResult) =
    "Response " <> show id <> show' eitherErrResult

    where
    show' (Left err) = "(Left " <> showRecord err <> ")"
    show' (Right res) = "(Right " <> show res <> ")"


type Error = {
    code :: ErrorCode
  , message :: String
  , data :: Maybe Json
}

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
    NonObjectError
  | MissingProtocolError
  | InvalidProtocolTypeError Json
  | InvalidProtocolError String
  | BothResultAndErrorError
  | MissingResultOrErrorError
  | InvalidErrorJson Json
  | MissingIdError
  | InvalidIdTypeError Json
derive instance eqResponseFormatError :: Eq ResponseFormatError
derive instance ordResponseFormatError :: Ord ResponseFormatError
derive instance genericResponseFormatError :: Generic ResponseFormatError _
instance showResponseFormatError :: Show ResponseFormatError where
  show = genericShow

fromJson :: Json -> Either ResponseFormatError Response
fromJson json = do
  respMap <- note NonObjectError $ Json.toObject json
  checkProtocol respMap
  Response <$> getId respMap <*> getResultOrError respMap

  where

  checkProtocol respMap = do
    protocolJson <- note MissingProtocolError
      $ SM.lookup Constants.protocolKey respMap
    protocolStr <- note (InvalidProtocolTypeError protocolJson)
      $ Json.toString protocolJson
    if protocolStr == Constants.protocolValue
      then Right unit
      else Left $ InvalidProtocolError protocolStr

  getId respMap = do
     idJson <- note MissingIdError $ SM.lookup Constants.idKey respMap
     note (InvalidIdTypeError idJson) $ Identifier.fromJson idJson

  getResultOrError respMap =
    let mResult = SM.lookup "result" respMap
        mError = SM.lookup "error" respMap
    in case mResult, mError of
         Just result, Just error -> Left BothResultAndErrorError
         Nothing, Just errJson -> Left <$> getError errJson
         Just result, Nothing -> Right $ Right $ Result result
         Nothing, Nothing -> Left MissingResultOrErrorError

    where

    getError errJson = note (InvalidErrorJson errJson) do
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
respond f { id, method, params } = case id of
  Just id' -> Just $ Response id' $ f { method, params }
  Nothing -> Nothing
