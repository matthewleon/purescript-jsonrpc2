module Test.Request.Gen where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen, oneOf, unfoldable)
import Control.Monad.Gen.Common (genMaybe)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array.NonEmpty as NEA
import Data.Maybe (fromJust)
import Data.String.Gen (genAsciiString)
import Data.Tuple (Tuple(..))
import Foreign.Object as SM
import JSONRPC2.Json (Json)
import JSONRPC2.Request (Params(..), Request(..))
import Partial.Unsafe (unsafePartial)
import Test.Identifier.Gen (genIdentifier)
import Test.Json.Gen (genJson)

genRequest
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Json)
  => m Request
genRequest = do
  id <- genId
  method <- genMethod
  params <- genParams
  pure $ Request {id, method, params}

  where
  genId = genMaybe genIdentifier

  genMethod = genAsciiString

  genParams = genMaybe $ oneOf $ unsafePartial $ fromJust $ NEA.fromArray [
      genPArray, genPObject ]

    where
    genPArray = PArray <$> unfoldable genJson

    genPObject = PObject <<< SM.fromFoldable <$> genObjMap
    
      where
      genObjMap :: m (Array (Tuple String Json))
      genObjMap = unfoldable $ Tuple <$> genAsciiString <*> genJson
