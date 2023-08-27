module CardanoMultiplatformLib.CostModel where

import Prelude

import CardanoMultiplatformLib.Types (Cbor, JsonString)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Newtype (class Newtype)
import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import JS.Object (EffectMth0, EffectMth1, EffectMth2, JSObject)
import JS.Object.Generic (mkNewtypedFFI)
import Type.Prelude (Proxy(..))

-- */
-- export class CostModel {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {CostModel}
-- */
--   static from_bytes(bytes: Uint8Array): CostModel;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {CostModelJSON}
-- */
--   to_js_value(): CostModelJSON;
-- /**
-- * @param {string} json
-- * @returns {CostModel}
-- */
--   static from_json(json: string): CostModel;
-- /**
-- * @param {Language} language
-- * @returns {CostModel}
-- */
--   static empty_model(language: Language): CostModel;
-- /**
-- * @param {number} operation
-- * @param {Int} cost
-- * @returns {Int}
-- */
--   set(operation: number, cost: Int): Int;
-- /**
-- * @param {number} operation
-- * @returns {Int}
-- */
--   get(operation: number): Int;
-- /**
-- * @returns {Language}
-- */
--   language(): Language;
-- }

newtype CostModel = CostModel
  ( JSObject
      ( from_json :: EffectMth1 String CostModelObject
      , empty_model :: EffectMth1 Language CostModelObject
      )
  )

derive instance Newtype CostModel _

costModel ::
  { from_json :: CostModel -> String -> Effect CostModelObject
  , empty_model :: CostModel -> Language -> Effect CostModelObject
  }
costModel = mkNewtypedFFI (Proxy :: Proxy CostModel)

newtype CostModelObject = CostModelObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 Uint8Array
      , to_json :: EffectMth0 String
      , to_js_value :: EffectMth0 JsonString
      , set :: EffectMth2 Number Int Int
      , get :: EffectMth1 Number Int
      , language :: EffectMth0 Language
      )
  )

derive instance Newtype CostModelObject _

costModelObject ::
  { free :: CostModelObject -> Effect Unit
  , to_bytes :: CostModelObject -> Effect Uint8Array
  , to_json :: CostModelObject -> Effect String
  , to_js_value :: CostModelObject -> Effect JsonString
  , set :: CostModelObject -> Number -> Int -> Effect Int
  , get :: CostModelObject -> Number -> Effect Int
  , language :: CostModelObject -> Effect Language
  }
costModelObject = mkNewtypedFFI (Proxy :: Proxy CostModelObject)

-- /**
-- */
-- export class Costmdls {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Costmdls}
-- */
--   static from_bytes(bytes: Uint8Array): Costmdls;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {CostmdlsJSON}
-- */
--   to_js_value(): CostmdlsJSON;
-- /**
-- * @param {string} json
-- * @returns {Costmdls}
-- */
--   static from_json(json: string): Costmdls;
-- /**
-- * @returns {Costmdls}
-- */
--   static new(): Costmdls;
-- /**
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * @param {CostModel} value
-- * @returns {CostModel | undefined}
-- */
--   insert(value: CostModel): CostModel | undefined;
-- /**
-- * @param {Language} key
-- * @returns {CostModel | undefined}
-- */
--   get(key: Language): CostModel | undefined;
-- /**
-- * @returns {Languages}
-- */
--   keys(): Languages;
-- }

newtype Costmdls = Costmdls
  ( JSObject
      ( from_json :: EffectMth1 String CostmdlsObject
      , new :: EffectMth0 CostmdlsObject
      )
  )

derive instance Newtype Costmdls _

costmdls ::
  { from_json :: Costmdls -> String -> Effect CostmdlsObject
  , new :: Costmdls -> Effect CostmdlsObject
  }
costmdls = mkNewtypedFFI (Proxy :: Proxy Costmdls)

foreign import data Languages :: Type

foreign import data LanguagesObject :: Type


newtype CostmdlsObject = CostmdlsObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 Uint8Array
      , to_json :: EffectMth0 String
      , to_js_value :: EffectMth0 JsonString
      , len :: EffectMth0 Number
      , insert :: EffectMth1 CostModelObject (Opt CostmdlsObject)
      , get :: EffectMth1 Language (Opt CostModel)
      , keys :: EffectMth0 LanguagesObject
      )
  )

derive instance Newtype CostmdlsObject _

costmdlsObject ::
  { free :: CostmdlsObject -> Effect Unit
  , to_bytes :: CostmdlsObject -> Effect Uint8Array
  , to_json :: CostmdlsObject -> Effect String
  , to_js_value :: CostmdlsObject -> Effect JsonString
  , len :: CostmdlsObject -> Effect Number
  , insert :: CostmdlsObject -> CostModelObject -> Effect (Opt CostmdlsObject)
  , get :: CostmdlsObject -> Language -> Effect (Opt CostModel)
  , keys :: CostmdlsObject -> Effect LanguagesObject
  }
costmdlsObject = mkNewtypedFFI (Proxy :: Proxy CostmdlsObject)

-- export class Language {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Language}
-- */
--   static from_bytes(bytes: Uint8Array): Language;
-- /**
-- * @returns {Language}
-- */
--   static new_plutus_v1(): Language;
-- /**
-- * @returns {Language}
-- */
--   static new_plutus_v2(): Language;
-- /**
-- * @returns {number}
-- */
--   kind(): number;
-- }

newtype Language = Language
  ( JSObject
      ( new_plutus_v1 :: EffectMth0 LanguageObject
      , new_plutus_v2 :: EffectMth0 LanguageObject
      , from_bytes :: EffectMth1 (Cbor LanguageObject) LanguageObject
      )
  )

derive instance Newtype Language _

language ::
  { new_plutus_v1 :: Language -> Effect LanguageObject
  , new_plutus_v2 :: Language -> Effect LanguageObject
  , from_bytes :: Language -> (Cbor LanguageObject) -> Effect LanguageObject
  }
language = mkNewtypedFFI (Proxy :: Proxy Language)

newtype LanguageObject = LanguageObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , kind :: EffectMth0 Int
      , to_bytes :: EffectMth0 Uint8Array
      )
  )

derive instance Newtype LanguageObject _

languageObject ::
  { free :: LanguageObject -> Effect Unit
  , kind :: LanguageObject -> Effect Int
  , to_bytes :: LanguageObject -> Effect Uint8Array
  }
languageObject = mkNewtypedFFI (Proxy :: Proxy LanguageObject)

