module Test.Request where

import Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import JSONRPC2.Identifier (Identifier(..))
import JSONRPC2.Request (Request(..))
import JSONRPC2.Request as Request
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

requestSpec :: forall r. Spec r Unit
requestSpec =
  describe "Request" do
    let reqArrayParams = Request {
            id: Just $ IdStr "stringId"
          , method: "some method"
          , params: Just $ Request.PArray [Json.fromString "StringParam", Json.fromNumber (123.456)]
        }
    describe "toJson" do
      let reqArrayParamsStr =  "{\
          \\"jsonrpc\":\"2.0\",\
          \\"method\":\"some method\",\
          \\"params\":[\"StringParam\",123.456],\
          \\"id\":\"stringId\"\
          \}"
      it "serializes requests correctly" do
         Right (Request.toJson reqArrayParams)
           `shouldEqual` jsonParser reqArrayParamsStr
