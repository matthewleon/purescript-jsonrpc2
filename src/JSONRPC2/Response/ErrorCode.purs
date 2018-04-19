module JSONRPC2.Response.ErrorCode (
    ErrorCode
  , codeParseError
  , codeInvalidRequest
  , codeMethodNotFound
  , codeInvalidParams
  , codeInternalError
  , fromNumber
  , toNumber
  , explanation
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (maybe')

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

fromNumber :: Number -> ErrorCode
fromNumber = case _ of
  32700.0 -> CodeParseError
  32600.0 -> CodeInvalidRequest
  32601.0 -> CodeMethodNotFound
  32602.0 -> CodeInvalidParams
  32603.0 -> CodeInternalError
  code -> code # if (code >= 3200.0) && (code <= 32099.0)
            then CodeServerError
            else CodeOther

toNumber :: ErrorCode -> Number
toNumber = case _ of
  CodeParseError -> 32700.0
  CodeInvalidRequest -> 32600.0
  CodeMethodNotFound -> 32601.0
  CodeInvalidParams -> 32602.0
  CodeInternalError -> 32603.0
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
  CodeServerError n -> "Server error: "
                       <> maybe' (\_ -> show n) show (Int.fromNumber n)
  CodeOther n -> "Other error (check message): " <> show n
