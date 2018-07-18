module Test.Json.Gen where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Gen as Argonaut
import Data.Newtype (wrap)
import JSONRPC2.Json (Json)

genJson :: forall m a. MonadGen m => MonadRec m => Lazy (m a) => m Json
genJson = wrap <$> Argonaut.genJson
