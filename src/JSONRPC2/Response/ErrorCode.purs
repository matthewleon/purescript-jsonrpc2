module JSONRPC2.Response.ErrorCode (
    ErrorCode
  , codeParseError
  , codeInvalidRequest
  , codeMethodNotFound
  , codeInvalidParams
  , codeInternalError
  , fromInt
  , toInt
  , explanation
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data ErrorCode =
    CodeParseError
  | CodeInvalidRequest
  | CodeMethodNotFound
  | CodeInvalidParams
  | CodeInternalError
  | CodeServerError Int
  | CodeOther Int
derive instance eqErrorCode :: Eq ErrorCode
derive instance ordErrorCode :: Ord ErrorCode
derive instance genericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow

codeParseError :: ErrorCode
codeParseError = CodeParseError

codeInvalidRequest :: ErrorCode
codeInvalidRequest = CodeInvalidRequest

codeMethodNotFound :: ErrorCode
codeMethodNotFound = CodeMethodNotFound

codeInvalidParams :: ErrorCode
codeInvalidParams = CodeInvalidParams

codeInternalError :: ErrorCode
codeInternalError = CodeInternalError

fromInt :: Int -> ErrorCode
fromInt = case _ of
  32700 -> CodeParseError
  32600 -> CodeInvalidRequest
  32601 -> CodeMethodNotFound
  32602 -> CodeInvalidParams
  32603 -> CodeInternalError
  code -> code # if (code >= 3200) && (code <= 32099)
            then CodeServerError
            else CodeOther

toInt :: ErrorCode -> Int
toInt = case _ of
  CodeParseError -> 32700
  CodeInvalidRequest -> 32600
  CodeMethodNotFound -> 32601
  CodeInvalidParams -> 32602
  CodeInternalError -> 32603
  CodeServerError n -> n
  CodeOther n -> n

explanation :: ErrorCode -> String
explanation = case _ of
  CodeParseError -> "Invalid JSON was received by the server, \
                    \or an error occured on the server \
                    \while parsing the JSON text."
  CodeInvalidRequest -> "Invalid request: \
                       \The JSON sent is not a valid request object."
  CodeMethodNotFound -> "Method not found: \
                        \The method does not exist / is not available."
  CodeInvalidParams -> "Invalid params: Invalid method parameter(s)."
  CodeInternalError -> "Internal error: Internal JSON-RPC error."
  CodeServerError n -> "Server error: " <> show n
  CodeOther n -> "Other error (check message): " <> show n
