module Test.Response.Batch where

import Prelude

import Control.Monad.Gen (unfoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import JSONRPC2.Response.Batch (fromJson, toJson)
import Test.QuickCheck.Gen (Gen)
import Test.Response.Gen (genResponse)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

spec :: forall r. Spec (QCRunnerEffects r) Unit
spec = describe "Response.Batch" $
  describe "bidirectional serialization" $
    it "roundtrips" $
      quickCheck $
        (gen <#> \batch -> fromJson (toJson batch) == Just (Right batch))
          :: Gen Boolean
  where
  gen = wrap <$> unfoldable genResponse
