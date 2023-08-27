module HexString where

import Prelude

import Control.Monad.Except (catchError)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Data.String.Regex as Regex
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Unsafe (unsafePerformEffect)
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.TextEncoder as TextEncoder
import Web.Encoding.UtfLabel (utf8)

newtype Hex = Hex String

derive newtype instance Eq Hex
derive newtype instance Ord Hex
derive newtype instance EncodeJson Hex
derive newtype instance DecodeJson Hex
derive newtype instance Show Hex

hexToString :: Hex -> String
hexToString (Hex str) = str

hex :: String -> Maybe Hex
hex = do
  let
    lowerCaseHexPattern = unsafeRegex "^[0-9a-f]*$" mempty
    anyHexPattern = unsafeRegex "^[0-9a-fA-F]*$" mempty

  case _ of
    str | Regex.test lowerCaseHexPattern str -> Just $ Hex str
    str | Regex.test anyHexPattern str -> Just $ Hex $ toLower str
    _ -> Nothing

foreign import decode :: Hex -> Uint8Array

foreign import encode :: Uint8Array -> Hex

stringToUtf8Hex :: String -> Hex
stringToUtf8Hex str = unsafePerformEffect do
  textEncoder <- TextEncoder.new
  let
    uint8Array = TextEncoder.encode str textEncoder
  pure $ encode uint8Array

utf8HexToString :: Hex -> Maybe String
utf8HexToString h = unsafePerformEffect do
  let
    uint8Array = decode h
  textDecoder <- TextDecoder.new utf8
  (Just <$> TextDecoder.decode uint8Array textDecoder) `catchError` \_ -> pure Nothing
