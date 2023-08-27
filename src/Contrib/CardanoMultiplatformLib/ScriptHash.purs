module Contrib.CardanoMultiplatformLib.ScriptHash
  ( ScriptHash(..)
  , ScriptHashObject(..)
  , ScriptHashes(..)
  , ScriptHashesObject(..)
  , scriptHash
  , scriptHashObject
  , scriptHashes
  , scriptHashesObject
  ) where

import Prelude

import CardanoMultiplatformLib.Types (Bech32, Cbor, CborHex, JsonString)
import Data.Argonaut (Json)
import Data.Newtype (class Newtype)
import Effect (Effect)
import JS.Object (EffectMth0, EffectMth1, JSObject)
import JS.Object.Generic (mkNewtypedFFI)
import Type.Prelude (Proxy(..))

-- export class ScriptHash {
--   free(): void;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {ScriptHash}
-- */
--   static from_bytes(bytes: Uint8Array): ScriptHash;
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
-- * @returns {ScriptHash}
-- */
--   static from_bech32(bech_str: string): ScriptHash;
-- /**
-- * @returns {string}
-- */
--   to_hex(): string;
-- /**
-- * @param {string} hex
-- * @returns {ScriptHash}
-- */
--   static from_hex(hex: string): ScriptHash;
-- }

newtype ScriptHash = ScriptHash
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor ScriptHashObject) ScriptHashObject
    , from_bech32 :: EffectMth1 Bech32 ScriptHashObject
    , from_hex :: EffectMth1 (CborHex ScriptHashObject) ScriptHashObject
    )
  )

derive instance Newtype ScriptHash _

scriptHash
  :: { from_bytes :: ScriptHash -> Cbor ScriptHashObject -> Effect ScriptHashObject
     , from_bech32 :: ScriptHash -> Bech32 -> Effect ScriptHashObject
     , from_hex :: ScriptHash -> CborHex ScriptHashObject -> Effect ScriptHashObject
     }
scriptHash = mkNewtypedFFI (Proxy :: Proxy ScriptHash)

newtype ScriptHashObject = ScriptHashObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor ScriptHashObject)
    , to_bech32 :: EffectMth1 Bech32 String
    , to_hex :: EffectMth0 (CborHex ScriptHashObject)
    )
  )

derive instance Newtype ScriptHashObject _

scriptHashObject
  :: { free :: ScriptHashObject -> Effect Unit
     , to_bytes :: ScriptHashObject -> Effect (Cbor ScriptHashObject)
     , to_bech32 :: ScriptHashObject -> Bech32 -> Effect String
     , to_hex :: ScriptHashObject -> Effect (CborHex ScriptHashObject)
     }
scriptHashObject = mkNewtypedFFI (Proxy :: Proxy ScriptHashObject)

-- export class ScriptHashes {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {ScriptHashes}
-- */
--   static from_bytes(bytes: Uint8Array): ScriptHashes;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {ScriptHashesJSON}
-- */
--   to_js_value(): ScriptHashesJSON;
-- /**
-- * @param {string} json
-- * @returns {ScriptHashes}
-- */
--   static from_json(json: string): ScriptHashes;
-- /**
-- * @returns {ScriptHashes}
-- */
--   static new(): ScriptHashes;
-- /**
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * @param {number} index
-- * @returns {ScriptHash}
-- */
--   get(index: number): ScriptHash;
-- /**
-- * @param {ScriptHash} elem
-- */
--   add(elem: ScriptHash): void;
-- }

newtype ScriptHashes = ScriptHashes
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor ScriptHashesObject) ScriptHashesObject
    , from_json :: EffectMth1 JsonString ScriptHashesObject
    , new :: EffectMth0 ScriptHashesObject
    )
  )

derive instance Newtype ScriptHashes _

scriptHashes
  :: { from_bytes :: ScriptHashes -> Cbor ScriptHashesObject -> Effect ScriptHashesObject
     , from_json :: ScriptHashes -> JsonString -> Effect ScriptHashesObject
     , new :: ScriptHashes -> Effect ScriptHashesObject
     }
scriptHashes = mkNewtypedFFI (Proxy :: Proxy ScriptHashes)

newtype ScriptHashesObject = ScriptHashesObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor ScriptHashesObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , len :: EffectMth0 Int
    , get :: EffectMth1 Int ScriptHashObject
    , add :: EffectMth1 ScriptHashObject Unit
    )
  )

derive instance Newtype ScriptHashesObject _

scriptHashesObject
  :: { free :: ScriptHashesObject -> Effect Unit
     , to_bytes :: ScriptHashesObject -> Effect (Cbor ScriptHashesObject)
     , to_json :: ScriptHashesObject -> Effect JsonString
     , to_js_value :: ScriptHashesObject -> Effect Json
     , len :: ScriptHashesObject -> Effect Int
     , get :: ScriptHashesObject -> Int -> Effect ScriptHashObject
     , add :: ScriptHashesObject -> ScriptHashObject -> Effect Unit
     }
scriptHashesObject = mkNewtypedFFI (Proxy :: Proxy ScriptHashesObject)


