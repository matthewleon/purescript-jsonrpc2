module Test.Json.Gen where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Gen as ArgonautGen
import Data.Newtype (wrap)
import JSONRPC2.Json (Json)

genJson :: forall m. MonadGen m => MonadRec m => Lazy (m Argonaut.Json) => m Json
genJson = wrap <$> ArgonautGen.genJson
