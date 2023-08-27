module CardanoMultiplatformLib.Types
  ( JsonString
  , jsonStringToString
  , jsonStringFromString
  , jsonStringFromJson
  , unsafeJsonString
  , cborHexToHex
  , cborHexToCbor
  , cborToCborHex
  , unsafeCborHex
  , cborToUint8Array
  -- FIXME: Import only the type
  , CborHex(..)
  , Cbor
  , Bech32
  , bech32ToString
  , unsafeBech32
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, parseJson, stringify)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (hush)
import Data.Maybe (Maybe)
import HexString (Hex)
import HexString as HexString

newtype CborHex :: Type -> Type
newtype CborHex a = CborHex Hex

derive instance Eq (CborHex a)
derive newtype instance EncodeJson (CborHex a)
derive newtype instance DecodeJson (CborHex a)

cborHexToHex :: forall a. CborHex a -> Hex
cborHexToHex (CborHex h) = h

cborHexToCbor :: forall a. CborHex a -> Cbor a
cborHexToCbor = Cbor <<< HexString.decode <<< cborHexToHex

cborToCborHex :: forall a. Cbor a -> CborHex a
cborToCborHex = CborHex <<< HexString.encode <<< cborToUint8Array

unsafeCborHex :: forall a. Hex -> CborHex a
unsafeCborHex = CborHex

newtype Cbor :: Type -> Type
newtype Cbor a = Cbor Uint8Array

cborToUint8Array :: forall a. Cbor a -> Uint8Array
cborToUint8Array (Cbor a) = a

newtype JsonString = JsonString String

unsafeJsonString :: String -> JsonString
unsafeJsonString = JsonString

jsonStringFromJson :: Json -> JsonString
jsonStringFromJson = JsonString <<< stringify

jsonStringFromString :: String -> Maybe JsonString
jsonStringFromString = map (JsonString <<< stringify) <<< hush <<< parseJson

jsonStringToString :: JsonString -> String
jsonStringToString (JsonString s) = s

newtype Bech32 = Bech32 String

derive newtype instance Eq Bech32
derive newtype instance Ord Bech32
derive newtype instance EncodeJson Bech32
derive newtype instance DecodeJson Bech32
derive newtype instance Show Bech32

bech32ToString :: Bech32 -> String
bech32ToString (Bech32 str) = str

unsafeBech32 :: String -> Bech32
unsafeBech32 = Bech32

