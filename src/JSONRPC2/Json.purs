module JSONRPC2.Json where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Foreign.Object (Object)

-- wrap the JSONArgonaut JSON type to give it a Show instance.
newtype Json = Json Argonaut.Json
derive instance newtypeJson :: Newtype Json _
derive newtype instance eqJson :: Eq Json
derive newtype instance ordJson :: Ord Json
instance showJson :: Show Json where
  show (Json j) = Argonaut.stringify j

caseJson :: ∀ a. (Unit → a) → (Boolean → a) → (Number → a) → (String → a) → (Array Json → a) → (Object Json → a) → Json → a
caseJson f1 f2 f3 f4 f5 f6 (Json j) = Argonaut.caseJson f1 f2 f3 f4 (f5 <<< map Json) (f6 <<< map Json) j

fromArray :: Array Json -> Json
fromArray = Json <<< Argonaut.fromArray <<< map unwrap

toArray :: Json -> Maybe (Array Json)
toArray (Json j) = map Json <$> Argonaut.toArray j

fromNumber :: Number -> Json
fromNumber = Json <<< Argonaut.fromNumber

toNumber :: Json -> Maybe Number
toNumber (Json j) = Argonaut.toNumber j

fromString :: String -> Json
fromString = Json <<< Argonaut.fromString

toString :: Json -> Maybe String
toString (Json j) = Argonaut.toString j

fromObject :: Object Json -> Json
fromObject = Json <<< Argonaut.fromObject <<< map unwrap

toObject :: Json -> Maybe (Object Json)
toObject (Json j) = map Json <$> Argonaut.toObject j

jsonNull :: Json
jsonNull = Json Argonaut.jsonNull
