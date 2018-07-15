module JSONRPC2.Protocol where

import Prelude

import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object (Object)
import Foreign.Object as SM
import JSONRPC2.Json (Json)
import JSONRPC2.Json as Json

data ProtocolError =
    ErrorMissingProtocol
  | ErrorInvalidProtocolType Json
  | ErrorInvalidProtocol String
derive instance eqProtocolError :: Eq ProtocolError
derive instance ordProtocolError :: Ord ProtocolError
derive instance genericProtocolError :: Generic ProtocolError _
instance showProtocolError :: Show ProtocolError where
  show = genericShow

protocolKey :: String
protocolKey = "jsonrpc"

protocolValue :: String
protocolValue = "2.0"

checkProtocol :: Object Json -> Either ProtocolError Unit
checkProtocol jMap = do
  protocolJson <- note ErrorMissingProtocol $ SM.lookup protocolKey jMap
  protocolStr <- note (ErrorInvalidProtocolType protocolJson)
    $ Json.toString protocolJson
  if protocolStr == protocolValue
    then Right unit
    else Left $ ErrorInvalidProtocol protocolStr
