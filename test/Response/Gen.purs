module Test.Response.Gen where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen, chooseInt, oneOf)
import Control.Monad.Gen.Common (genEither, genMaybe)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core as Argonaut
import Data.Array.NonEmpty as NEA
import Data.Maybe (fromJust)
import Data.String.Gen (genAsciiString)
import JSONRPC2.Response (Response(..))
import JSONRPC2.Response as Response
import JSONRPC2.Response.ErrorCode as ErrorCode
import Partial.Unsafe (unsafePartial)
import Test.Identifier.Gen (genIdentifier, genIdentifierNonFractional)
import Test.Json.Gen (genJson)

genResponse
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Argonaut.Json)
  => m Response
genResponse = Response <$> genIdentifier <*> genEither genError genResult

genResponseWithNonFractionalId
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Argonaut.Json)
  => m Response
genResponseWithNonFractionalId = Response <$> genIdentifierNonFractional <*> genEither genError genResult

genError
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Argonaut.Json)
  => m Response.Error
genError = do
  code <- genErrorCode
  message <- genAsciiString
  _data <- genMaybe genJson -- "data" is a reserved word
  pure $ Response.Error { code, message, data: _data}

  where
  genErrorCode = oneOf $ unsafePartial fromJust $ NEA.fromArray [
    pure ErrorCode.codeParseError
  , pure ErrorCode.codeInvalidRequest
  , pure ErrorCode.codeMethodNotFound
  , pure ErrorCode.codeInvalidParams
  , pure ErrorCode.codeInternalError
  , ErrorCode.fromInt <$> chooseInt 3200 32099 -- CodeServerError
  , ErrorCode.fromInt <$> chooseInt (-10000) 10000
  ]

genResult
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Argonaut.Json)
  => m Response.Result
genResult = Response.Result <$> genJson
