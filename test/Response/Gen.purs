module Test.Response.Gen where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen, chooseFloat, oneOf)
import Control.Monad.Gen.Common (genEither, genMaybe)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Gen (genJson)
import Data.Array.NonEmpty as NEA
import Data.Maybe (fromJust)
import Data.String.Gen (genAsciiString)
import JSONRPC2.Response (Response(..))
import JSONRPC2.Response as Response
import JSONRPC2.Response.ErrorCode as ErrorCode
import Partial.Unsafe (unsafePartial)
import Test.Identifier.Gen (genIdentifier)

genResponse
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Json)
  => m Response
genResponse = Response <$> genIdentifier <*> genEither genError genResult
  where
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
    , ErrorCode.fromNumber <$> chooseFloat 3200.0 32099.0 -- CodeServerError
    , ErrorCode.fromNumber <$> chooseFloat (-10000.0) 10000.0
    ]

  genResult = Response.Result <$> genJson
