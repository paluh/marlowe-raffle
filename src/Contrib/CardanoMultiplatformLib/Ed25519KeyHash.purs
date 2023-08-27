module CardanoMultiplatformLib.Ed25519KeyHash where

import Prelude

import CardanoMultiplatformLib.Types (Bech32, Cbor, CborHex)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Newtype (class Newtype)
import Effect (Effect)
import JS.Object (EffectMth0, EffectMth1, JSObject)
import JS.Object.Generic (mkNewtypedFFI)
import Type.Prelude (Proxy(..))

-- export class Ed25519KeyHash {
--   free(): void;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Ed25519KeyHash}
-- */
--   static from_bytes(bytes: Uint8Array): Ed25519KeyHash;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {string} prefix
-- * @returns {string}
-- */
--   to_bech32(prefix: string): string;
-- /**
-- * @param {string} bech_str
-- * @returns {Ed25519KeyHash}
-- */
--   static from_bech32(bech_str: string): Ed25519KeyHash;
-- /**
-- * @returns {string}
-- */
--   to_hex(): string;
-- /**
-- * @param {string} hex
-- * @returns {Ed25519KeyHash}
-- */
--   static from_hex(hex: string): Ed25519KeyHash;
-- }

newtype Ed25519KeyHash = Ed25519KeyHash
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor Ed25519KeyHashObject) Ed25519KeyHashObject
      , from_bech32 :: EffectMth1 Bech32 Ed25519KeyHashObject
      , from_hex :: EffectMth1 String Ed25519KeyHashObject
      )
  )

derive instance Newtype Ed25519KeyHash _

ed25519KeyHash ::
  { from_bytes :: Ed25519KeyHash -> (Cbor Ed25519KeyHashObject) -> Effect Ed25519KeyHashObject
  , from_bech32 :: Ed25519KeyHash -> Bech32 -> Effect Ed25519KeyHashObject
  , from_hex :: Ed25519KeyHash -> String -> Effect Ed25519KeyHashObject
  }
ed25519KeyHash = mkNewtypedFFI (Proxy :: Proxy Ed25519KeyHash)

newtype Ed25519KeyHashObject = Ed25519KeyHashObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 Uint8Array
      , to_bech32 :: EffectMth1 String String
      , to_hex :: EffectMth0 (CborHex Ed25519KeyHashObject)
      )
  )

derive instance Newtype Ed25519KeyHashObject _

ed25519KeyHashObject ::
  { free :: Ed25519KeyHashObject -> Effect Unit
  , to_bytes :: Ed25519KeyHashObject -> Effect Uint8Array
  , to_bech32 :: Ed25519KeyHashObject -> String -> Effect String
  , to_hex :: Ed25519KeyHashObject -> Effect (CborHex Ed25519KeyHashObject)
  }
ed25519KeyHashObject = mkNewtypedFFI (Proxy :: Proxy Ed25519KeyHashObject)


-- export class Ed25519KeyHashes {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Ed25519KeyHashes}
-- */
--   static from_bytes(bytes: Uint8Array): Ed25519KeyHashes;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {Ed25519KeyHashesJSON}
-- */
--   to_js_value(): Ed25519KeyHashesJSON;
-- /**
-- * @param {string} json
-- * @returns {Ed25519KeyHashes}
-- */
--   static from_json(json: string): Ed25519KeyHashes;
-- /**
-- * @returns {Ed25519KeyHashes}
-- */
--   static new(): Ed25519KeyHashes;
-- /**
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * @param {number} index
-- * @returns {Ed25519KeyHash}
-- */
--   get(index: number): Ed25519KeyHash;
-- /**
-- * @param {Ed25519KeyHash} elem
-- */
--   add(elem: Ed25519KeyHash): void;
-- }

