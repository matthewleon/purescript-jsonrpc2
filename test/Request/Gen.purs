module Test.Request.Gen where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen, oneOf, unfoldable)
import Control.Monad.Gen.Common (genMaybe)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Gen (genJson)
import Data.Array.NonEmpty as NEA
import Data.Maybe (fromJust)
import Data.StrMap as SM
import Data.String.Gen (genAsciiString)
import Data.Tuple (Tuple(..))
import JSONRPC2.Request (Params(..), Request(..))
import Partial.Unsafe (unsafePartial)
import Test.Identifier.Gen (genIdentifier)

genRequest
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Json)
  => m Request
genRequest = map Request $ { id: _, method: _, params: _}
    <$> genId
    <*> genMethod
    <*> genParams

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
