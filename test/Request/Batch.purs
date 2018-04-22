module Test.Request.Batch where

import Prelude

import Control.Monad.Gen (unfoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import JSONRPC2.Request.Batch (fromJson, toJson)
import Test.QuickCheck.Gen (Gen)
import Test.Request.Gen (genRequest)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

spec :: forall r. Spec (QCRunnerEffects r) Unit
spec = describe "Request.Batch" $
  describe "bidirectional serialization" $
    it "roundtrips" $
      quickCheck $
        (gen <#> \batch -> fromJson (toJson batch) == Just (Right batch))
          :: Gen Boolean
  where
  gen = wrap <$> unfoldable genRequest
