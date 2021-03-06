module Test.Response where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), fromRight)
import Data.Newtype (wrap)
import JSONRPC2.Identifier (Identifier(..))
import JSONRPC2.Json (Json)
import JSONRPC2.Json as Json
import JSONRPC2.Response (Response(..), fromJson, toJson)
import JSONRPC2.Response as Response
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Gen (Gen)
import Test.Response.Gen (genResponse, genResponseWithNonFractionalId)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "Response" do
    let successJson = wrap $ unsafePartial fromRight
          $ jsonParser "{\"jsonrpc\": \"2.0\", \"result\": -19, \"id\": 2}"
        errorStr = "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}"

        successResponse = Response (IdNum 2.0) $ Right $ Response.Result $ Json.fromNumber $ -19.0

    describe "fromJson" $
      it "deserializes responses correctly" $
        Response.fromJson successJson `shouldEqual` Right successResponse

    describe "toJson" $ do
      it "serializes responses correctly" $
        Response.toJson successResponse `shouldEqual` successJson
      it "passes JSON Schema validation" $
         quickCheck $ (validateResponse <<< toJson <$> genResponseWithNonFractionalId) :: Gen Boolean

    describe "bidirectional serialization" $
      it "roundtrips" $
        quickCheck $
          (genResponse <#> \resp -> fromJson (toJson resp) == Right resp)
            :: Gen Boolean

foreign import validateResponse :: Json -> Boolean
