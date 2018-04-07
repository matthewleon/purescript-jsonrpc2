module Test.Response where

import Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), fromRight)
import JSONRPC2.Identifier (Identifier(..))
import JSONRPC2.Response (Response(..))
import JSONRPC2.Response as Response
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

responseSpec :: forall r. Spec r Unit
responseSpec = 
  describe "Response" do
    describe "fromJson" do
      let successJson = unsafePartial fromRight
            $ jsonParser "{\"jsonrpc\": \"2.0\", \"result\": -19, \"id\": 2}"
          errorStr = "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}"

          successResponse = Response (IdNum 2.0) $ Right $ Response.Result $ Json.fromNumber $ -19.0
      it "deserializes responses correctly" do
        Response.fromJson successJson `shouldEqual` Right successResponse
