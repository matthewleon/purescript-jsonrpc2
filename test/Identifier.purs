module Test.Identifier.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseFloat, chooseInt, frequency)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array.NonEmpty as NEA
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.String.Gen (genAsciiString)
import Data.Tuple (Tuple(..))
import JSONRPC2.Identifier (Identifier(..))
import Partial.Unsafe (unsafePartial)

genIdentifier :: forall m. MonadGen m => MonadRec m => m Identifier
genIdentifier = frequency $ unsafePartial $ fromJust $ NEA.fromArray [
    Tuple 0.45 $ IdStr <$> genAsciiString
  , Tuple 0.45 $ IdNum <$> chooseFloat (-1000.0) 1000.0
  , Tuple 0.1 $ pure IdNull
  ]

-- Spec says Id should not be fractional.
-- RPC schema uses an integer.
genIdentifierNonFractional :: forall m. MonadGen m => MonadRec m => m Identifier
genIdentifierNonFractional = frequency $ unsafePartial $ fromJust $ NEA.fromArray [
    Tuple 0.45 $ IdStr <$> genAsciiString
  , Tuple 0.45 $ IdNum <<< toNumber <$> chooseInt (-1000) 1000
  , Tuple 0.1 $ pure IdNull
  ]
