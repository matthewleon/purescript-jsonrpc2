module JSONRPC2.Protocol where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.StrMap (StrMap)
import Data.StrMap as SM

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

checkProtocol :: StrMap Json -> Either ProtocolError Unit
checkProtocol jMap = do
  protocolJson <- note ErrorMissingProtocol $ SM.lookup protocolKey jMap
  protocolStr <- note (ErrorInvalidProtocolType protocolJson)
    $ Json.toString protocolJson
  if protocolStr == protocolValue
    then Right unit
    else Left $ ErrorInvalidProtocol protocolStr
