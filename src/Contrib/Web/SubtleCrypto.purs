module Contrib.Web.SubtleCrypto where

import Prelude

import Contrib.String (padStartWith) as String
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.ArrayBuffer.Typed as ArrayBuffer.Typed
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, DataView, Uint8)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (joinWith, length) as String
import Data.Traversable (for)
import Data.UInt as UInt
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import HexString (Hex, hex, hexToString)
import Promise (Promise)
import Promise.Aff as Promise
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Web.Encoding.TextEncoder as TextEncoder

data Sha2Length

foreign import data Sha256 :: Sha2Length
foreign import data Sha384 :: Sha2Length
foreign import data Sha512 :: Sha2Length

foreign import data Algorithm :: Sha2Length -> Type

sha256 :: Algorithm Sha256
sha256 = unsafeCoerce "SHA-256"

sha384 :: Algorithm Sha384
sha384 = unsafeCoerce "SHA-384"

sha512 :: Algorithm Sha512
sha512 = unsafeCoerce "SHA-512"

class ReflectSha2Length (l :: Sha2Length) where
  reflectSha2Length :: Proxy l -> Int

instance ReflectSha2Length Sha256 where
  reflectSha2Length _ = 64

instance ReflectSha2Length Sha384 where
  reflectSha2Length _ = 96

instance ReflectSha2Length Sha512 where
  reflectSha2Length _ = 128

foreign import data MutableData :: Type

dataFromArrayBuffer:: ArrayBuffer -> MutableData
dataFromArrayBuffer = unsafeCoerce

dataFromArrayView :: forall t. ArrayView t -> MutableData
dataFromArrayView = unsafeCoerce

dataFromDataView :: DataView -> MutableData
dataFromDataView = unsafeCoerce

foreign import digestImpl :: forall (sha2Length :: Sha2Length). EffectFn2 (Algorithm sha2Length) MutableData (Nullable (Promise ArrayBuffer))

newtype Sha2ArrayBuffer (sha2Length :: Sha2Length) = Sha2ArrayBuffer ArrayBuffer

digest :: forall (sha2Length :: Sha2Length)
  . Algorithm sha2Length
  -> MutableData -> Aff (Maybe (Sha2ArrayBuffer sha2Length))
digest alg d = do
  pp <- liftEffect $ runEffectFn2 digestImpl alg d
  case Nullable.toMaybe pp of
    Nothing -> pure Nothing
    Just p -> do
      res <- Promise.toAff p
      pure $ Just $ Sha2ArrayBuffer res

newtype Sha2Hex (sha2Length :: Sha2Length) = Sha2Hex Hex

derive instance Eq (Sha2Hex l)
derive instance Ord (Sha2Hex l)
derive newtype instance Show (Sha2Hex l)

instance EncodeJson (Sha2Hex l) where
  encodeJson (Sha2Hex s) = encodeJson s

instance ReflectSha2Length l => DecodeJson (Sha2Hex l) where
  decodeJson json = do
    s <- decodeJson json
    case sha2Hex s of
      Just shaHex -> pure shaHex
      Nothing -> throwError $ TypeMismatch $ "Invalid sha256 hex: " <> hexToString s

digestString :: forall (sha2Length :: Sha2Length). Algorithm sha2Length -> String -> Aff (Maybe (Sha2Hex sha2Length))
digestString alg inStr = do
  uintArray <- liftEffect do
    textEncoder <- TextEncoder.new
    pure $ TextEncoder.encode inStr textEncoder

  possibleBuf <- digest alg (dataFromArrayView uintArray)
  join <$> for possibleBuf \(Sha2ArrayBuffer arrBuf) -> liftEffect do
    (arrView :: ArrayView Uint8) <- ArrayBuffer.Typed.whole arrBuf
    arr <- ArrayBuffer.Typed.toArray arrView
    let
      hashStr = String.joinWith "" $ arr <#> \n -> do
        String.padStartWith 2 '0' (Int.toStringAs Int.hexadecimal $ UInt.toInt n)
    pure $ Sha2Hex <$> (hex hashStr :: Maybe Hex)

sha2Hex :: forall l. ReflectSha2Length l => Hex -> Maybe (Sha2Hex l)
sha2Hex hex = if String.length (hexToString hex) == reflectSha2Length (Proxy :: Proxy l)
  then Just $ Sha2Hex hex
  else Nothing

