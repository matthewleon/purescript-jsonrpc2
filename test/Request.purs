module Test.Request where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import JSONRPC2.Identifier (Identifier(..))
import JSONRPC2.Json (Json)
import JSONRPC2.Json as Json
import JSONRPC2.Request (Request(..), fromJson, toJson)
import JSONRPC2.Request as Request
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Gen (Gen)
import Test.Request.Gen (genRequest)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "Request" do
    let reqArrayParams = Request {
            id: Just $ IdStr "stringId"
          , method: "some method"
          , params: Just $ Request.PArray [Json.fromString "StringParam", Json.fromNumber (123.456)]
        }

        reqArrayParamsJson = wrap $ unsafePartial fromRight $ jsonParser "{\
          \\"jsonrpc\":\"2.0\",\
          \\"method\":\"some method\",\
          \\"params\":[\"StringParam\",123.456],\
          \\"id\":\"stringId\"\
          \}"

    describe "fromJson" $
      it "deserializes requests correctly" $
          Request.fromJson reqArrayParamsJson `shouldEqual` Right reqArrayParams

    describe "toJson" $ do
      it "serializes requests correctly" $
         Request.toJson reqArrayParams `shouldEqual` reqArrayParamsJson
      it "passes JSON Schema validation" $
         quickCheck $ (validateRequest <<< toJson <$> genRequest) :: Gen Boolean

    it "roundtrips serialization" $
      quickCheck $
        (genRequest <#> \req -> fromJson (toJson req) == Right req)
          :: Gen Boolean

foreign import validateRequest :: Json -> Boolean
