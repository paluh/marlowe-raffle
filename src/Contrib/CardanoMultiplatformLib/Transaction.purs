module CardanoMultiplatformLib.Transaction where

import Prelude

import CardanoMultiplatformLib.Address (AddressObject)
import CardanoMultiplatformLib.Types (Bech32, Cbor, CborHex, JsonString)
import Contrib.CardanoMultiplatformLib.ScriptHash (ScriptHashObject, ScriptHashesObject)
import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Newtype (class Newtype)
import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectMth3, JSObject)
import JS.Object.Generic (mkNewtypedFFI)
import Type.Prelude (Proxy(..))

-- export class Transaction {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Transaction}
-- */
--   static from_bytes(bytes: Uint8Array): Transaction;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {TransactionJSON}
-- */
--   to_js_value(): TransactionJSON;
-- /**
-- * @param {string} json
-- * @returns {Transaction}
-- */
--   static from_json(json: string): Transaction;
-- /**
-- * @returns {TransactionBody}
-- */
--   body(): TransactionBody;
-- /**
-- * @returns {TransactionWitnessSet}
-- */
--   witness_set(): TransactionWitnessSet;
-- /**
-- * @returns {boolean}
-- */
--   is_valid(): boolean;
-- /**
-- * @returns {AuxiliaryData | undefined}
-- */
--   auxiliary_data(): AuxiliaryData | undefined;
-- /**
-- * @param {boolean} valid
-- */
--   set_is_valid(valid: boolean): void;
-- /**
-- * @param {TransactionBody} body
-- * @param {TransactionWitnessSet} witness_set
-- * @param {AuxiliaryData | undefined} auxiliary_data
-- * @returns {Transaction}
-- */
--   static new(body: TransactionBody, witness_set: TransactionWitnessSet, auxiliary_data?: AuxiliaryData): Transaction;
-- }

newtype AuxiliaryData = AuxiliaryData (JSObject ())

derive instance Newtype AuxiliaryData _

newtype Transaction = Transaction
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionObject) TransactionObject
      , from_json :: EffectMth1 JsonString TransactionObject
      , new :: EffectMth3 TransactionBodyObject TransactionWitnessSetObject (Opt AuxiliaryData) TransactionObject
      )
  )

derive instance Newtype Transaction _

transaction
  :: { from_bytes :: Transaction -> Cbor TransactionObject -> Effect TransactionObject
     , from_json :: Transaction -> JsonString -> Effect TransactionObject
     , new :: Transaction -> TransactionBodyObject -> TransactionWitnessSetObject -> Opt AuxiliaryData -> Effect TransactionObject
     }
transaction = mkNewtypedFFI (Proxy :: Proxy Transaction)

newtype TransactionObject = TransactionObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 (Cbor TransactionObject)
      , to_json :: EffectMth0 JsonString
      , auxiliary_data :: EffectMth0 (Opt AuxiliaryData)
      -- | Clone the tx body
      , body :: EffectMth0 TransactionBodyObject
      )
  )

derive instance Newtype TransactionObject _

transactionObject
  :: { free :: TransactionObject -> Effect Unit
     , to_bytes :: TransactionObject -> Effect (Cbor TransactionObject)
     , to_json :: TransactionObject -> Effect JsonString
     , auxiliary_data :: TransactionObject -> Effect (Opt AuxiliaryData)
     , body :: TransactionObject -> Effect TransactionBodyObject
     }
transactionObject = mkNewtypedFFI (Proxy :: Proxy TransactionObject)

-- export class TransactionBody {
--   free(): void;
--   to_bytes(): Uint8Array;
--
--   static from_bytes(bytes: Uint8Array): TransactionBody;
--
--   to_js_value(): TransactionBodyJSON;
--
--   static from_json(json: string): TransactionBody;
--
--   inputs(): TransactionInputs;
--
--   outputs(): TransactionOutputs;
--
--   fee(): BigNum;
--
--   ttl(): BigNum | undefined;
--
--   set_certs(certs: Certificates): void;
--
--   certs(): Certificates | undefined;
--
--   set_withdrawals(withdrawals: Withdrawals): void;
--
--   withdrawals(): Withdrawals | undefined;
--
--   set_update(update: Update): void;
--
--   update(): Update | undefined;
--
--   set_auxiliary_data_hash(auxiliary_data_hash: AuxiliaryDataHash): void;
--
--   auxiliary_data_hash(): AuxiliaryDataHash | undefined;
--
--   set_validity_start_interval(validity_start_interval: BigNum): void;
--
--   validity_start_interval(): BigNum | undefined;
--
--   set_mint(mint: Mint): void;
--
--   mint(): Mint | undefined;
--
--   multiassets(): Mint | undefined;
--
--   set_script_data_hash(script_data_hash: ScriptDataHash): void;
--
--   script_data_hash(): ScriptDataHash | undefined;
--
--   set_collateral(collateral: TransactionInputs): void;
--
--   collateral(): TransactionInputs | undefined;
--
--   set_required_signers(required_signers: Ed25519KeyHashes): void;
--
--   required_signers(): Ed25519KeyHashes | undefined;
--
--   set_network_id(network_id: NetworkId): void;
--
--   network_id(): NetworkId | undefined;
--
--   set_collateral_return(collateral_return: TransactionOutput): void;
--
--   collateral_return(): TransactionOutput | undefined;
--
--   set_total_collateral(total_collateral: BigNum): void;
--
--   total_collateral(): BigNum | undefined;
--
--   set_reference_inputs(reference_inputs: TransactionInputs): void;
--
--   reference_inputs(): TransactionInputs | undefined;
--
--   static new(inputs: TransactionInputs, outputs: TransactionOutputs, fee: BigNum, ttl?: BigNum): TransactionBody;
-- }

newtype TransactionBody = TransactionBody
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionBodyObject) TransactionBodyObject
      , from_json :: EffectMth1 JsonString TransactionBodyObject
      )
  )

derive instance Newtype TransactionBody _

transactionBody
  :: { from_bytes :: TransactionBody -> Cbor TransactionBodyObject -> Effect TransactionBodyObject
     , from_json :: TransactionBody -> JsonString -> Effect TransactionBodyObject
     }
transactionBody = mkNewtypedFFI (Proxy :: Proxy TransactionBody)

newtype TransactionBodyObject = TransactionBodyObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_js_value :: EffectMth0 Json -- TransactionBodyJSON
      )
  )

derive instance Newtype TransactionBodyObject _

transactionBodyObject
  :: { free :: TransactionBodyObject -> Effect Unit
     , to_js_value :: TransactionBodyObject -> Effect Json
     }
transactionBodyObject = mkNewtypedFFI (Proxy :: Proxy TransactionBodyObject)

-- export class TransactionWitnessSet {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionWitnessSet}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionWitnessSet;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {TransactionWitnessSetJSON}
-- */
--   to_js_value(): TransactionWitnessSetJSON;
-- /**
-- * @param {string} json
-- * @returns {TransactionWitnessSet}
-- */
--   static from_json(json: string): TransactionWitnessSet;
-- /**
-- * @param {Vkeywitnesses} vkeys
-- */
--   set_vkeys(vkeys: Vkeywitnesses): void;
-- /**
-- * @returns {Vkeywitnesses | undefined}
-- */
--   vkeys(): Vkeywitnesses | undefined;
-- /**
-- * @param {NativeScripts} native_scripts
-- */
--   set_native_scripts(native_scripts: NativeScripts): void;
-- /**
-- * @returns {NativeScripts | undefined}
-- */
--   native_scripts(): NativeScripts | undefined;
-- /**
-- * @param {BootstrapWitnesses} bootstraps
-- */
--   set_bootstraps(bootstraps: BootstrapWitnesses): void;
-- /**
-- * @returns {BootstrapWitnesses | undefined}
-- */
--   bootstraps(): BootstrapWitnesses | undefined;
-- /**
-- * @param {PlutusV1Scripts} plutus_v1_scripts
-- */
--   set_plutus_v1_scripts(plutus_v1_scripts: PlutusV1Scripts): void;
-- /**
-- * @returns {PlutusV1Scripts | undefined}
-- */
--   plutus_v1_scripts(): PlutusV1Scripts | undefined;
-- /**
-- * @param {PlutusList} plutus_data
-- */
--   set_plutus_data(plutus_data: PlutusList): void;
-- /**
-- * @returns {PlutusList | undefined}
-- */
--   plutus_data(): PlutusList | undefined;
-- /**
-- * @param {Redeemers} redeemers
-- */
--   set_redeemers(redeemers: Redeemers): void;
-- /**
-- * @returns {Redeemers | undefined}
-- */
--   redeemers(): Redeemers | undefined;
-- /**
-- * @param {PlutusV2Scripts} plutus_v2_scripts
-- */
--   set_plutus_v2_scripts(plutus_v2_scripts: PlutusV2Scripts): void;
-- /**
-- * @returns {PlutusV2Scripts | undefined}
-- */
--   plutus_v2_scripts(): PlutusV2Scripts | undefined;
-- /**
-- * @returns {TransactionWitnessSet}
-- */
--   static new(): TransactionWitnessSet;
-- }

newtype TransactionWitnessSet = TransactionWitnessSet
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionWitnessSetObject) TransactionWitnessSetObject
      , from_json :: EffectMth1 JsonString TransactionWitnessSetObject
      )
  )

derive instance Newtype TransactionWitnessSet _

transactionWitnessSet
  :: { from_bytes :: TransactionWitnessSet -> (Cbor TransactionWitnessSetObject) -> Effect TransactionWitnessSetObject
     , from_json :: TransactionWitnessSet -> JsonString -> Effect TransactionWitnessSetObject
     }
transactionWitnessSet = mkNewtypedFFI (Proxy :: Proxy TransactionWitnessSet)

newtype TransactionWitnessSetObject = TransactionWitnessSetObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 Uint8Array
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json -- TransactionWitnessSetJSON
    -- , set_vkeys :: Vkeywitnesses -> Effect Unit
    -- , vkeys :: Effect (Maybe Vkeywitnesses)
    -- , set_native_scripts :: NativeScripts -> Effect Unit
    -- , native_scripts :: Effect (Maybe NativeScripts)
    -- , set_bootstraps :: BootstrapWitnesses -> Effect Unit
    -- , bootstraps :: Effect (Maybe BootstrapWitnesses)
    -- , set_plutus_v1_scripts :: PlutusV1Scripts -> Effect Unit
    -- , plutus_v1_scripts :: Effect (Maybe PlutusV1Scripts)
    -- , set_plutus_data :: PlutusList -> Effect Unit
    , plutus_data :: EffectMth0 (Opt PlutusListObject)
    , redeemers :: EffectMth0 (Opt RedeemersObject)
    )
  )

derive instance Newtype TransactionWitnessSetObject _

transactionWitnessSetObject
  :: { free :: TransactionWitnessSetObject -> Effect Unit
     , to_bytes :: TransactionWitnessSetObject -> Effect Uint8Array
     , to_json :: TransactionWitnessSetObject -> Effect JsonString
     , to_js_value :: TransactionWitnessSetObject -> Effect Json
     , plutus_data :: TransactionWitnessSetObject -> Effect (Opt PlutusListObject)
     , redeemers :: TransactionWitnessSetObject -> Effect (Opt RedeemersObject)
     }
transactionWitnessSetObject = mkNewtypedFFI (Proxy :: Proxy TransactionWitnessSetObject)

-- export class Value {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Value}
-- */
--   static from_bytes(bytes: Uint8Array): Value;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {ValueJSON}
-- */
--   to_js_value(): ValueJSON;
-- /**
-- * @param {string} json
-- * @returns {Value}
-- */
--   static from_json(json: string): Value;
-- /**
-- * @param {BigNum} coin
-- * @returns {Value}
-- */
--   static new(coin: BigNum): Value;
-- /**
-- * @param {MultiAsset} multiasset
-- * @returns {Value}
-- */
--   static new_from_assets(multiasset: MultiAsset): Value;
-- /**
-- * @returns {Value}
-- */
--   static zero(): Value;
-- /**
-- * @returns {boolean}
-- */
--   is_zero(): boolean;
-- /**
-- * @returns {BigNum}
-- */
--   coin(): BigNum;
-- /**
-- * @param {BigNum} coin
-- */
--   set_coin(coin: BigNum): void;
-- /**
-- * @returns {MultiAsset | undefined}
-- */
--   multiasset(): MultiAsset | undefined;
-- /**
-- * @param {MultiAsset} multiasset
-- */
--   set_multiasset(multiasset: MultiAsset): void;
-- /**
-- * @param {Value} rhs
-- * @returns {Value}
-- */
--   checked_add(rhs: Value): Value;
-- /**
-- * @param {Value} rhs_value
-- * @returns {Value}
-- */
--   checked_sub(rhs_value: Value): Value;
-- /**
-- * @param {Value} rhs_value
-- * @returns {Value}
-- */
--   clamped_sub(rhs_value: Value): Value;
-- /**
-- * note: values are only partially comparable
-- * @param {Value} rhs_value
-- * @returns {number | undefined}
-- */
--   compare(rhs_value: Value): number | undefined;
-- }

newtype Value = Value
  (JSObject
    ( from_bytes :: EffectMth1 (Cbor ValueObject) ValueObject
    , from_json :: EffectMth1 JsonString ValueObject
    , new_from_assets :: EffectMth1 MultiAssetObject ValueObject
    -- , new :: EffectMth1 BigNum Value
    )
  )

derive instance Newtype Value _

value
  :: { from_bytes :: Value -> (Cbor ValueObject) -> Effect ValueObject
     , from_json :: Value -> JsonString -> Effect ValueObject
     , new_from_assets :: Value -> MultiAssetObject -> Effect ValueObject
     -- , new :: BigNum -> Effect Value
     }
value = mkNewtypedFFI (Proxy :: Proxy Value)

newtype ValueObject = ValueObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 (Cbor ValueObject)
      , to_json :: EffectMth0 JsonString
      , to_js_value :: EffectMth0 Json
      , is_zero :: EffectMth0 Boolean
      , coin :: EffectMth0 BigNumObject
      , set_coin :: EffectMth1 BigNumObject Unit
      , multiasset :: EffectMth0 (Opt MultiAssetObject)
      , set_multiasset :: EffectMth1 MultiAssetObject Unit
      , checked_add :: EffectMth1 ValueObject ValueObject
      , checked_sub :: EffectMth1 ValueObject ValueObject
      , clamped_sub :: EffectMth1 ValueObject ValueObject
      , compare :: EffectMth1 ValueObject (Opt Int)
      )
  )

derive instance Newtype ValueObject _

valueObject
  :: { free :: ValueObject -> Effect Unit
     , to_bytes :: ValueObject -> Effect (Cbor ValueObject)
     , to_json :: ValueObject -> Effect JsonString
     , to_js_value :: ValueObject -> Effect Json
     , is_zero :: ValueObject -> Effect Boolean
     , coin :: ValueObject -> Effect BigNumObject
     , set_coin :: ValueObject -> BigNumObject -> Effect Unit
     , multiasset :: ValueObject -> Effect (Opt MultiAssetObject)
     , set_multiasset :: ValueObject -> MultiAssetObject -> Effect Unit
     , checked_add :: ValueObject -> ValueObject -> Effect ValueObject
     , checked_sub :: ValueObject -> ValueObject -> Effect ValueObject
     , clamped_sub :: ValueObject -> ValueObject -> Effect ValueObject
     , compare :: ValueObject -> ValueObject -> Effect (Opt Int)
     }
valueObject = mkNewtypedFFI (Proxy :: Proxy ValueObject)

-- export class MultiAsset {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {MultiAsset}
-- */
--   static from_bytes(bytes: Uint8Array): MultiAsset;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {MultiAssetJSON}
-- */
--   to_js_value(): MultiAssetJSON;
-- /**
-- * @param {string} json
-- * @returns {MultiAsset}
-- */
--   static from_json(json: string): MultiAsset;
-- /**
-- * @returns {MultiAsset}
-- */
--   static new(): MultiAsset;
-- /**
-- * the number of unique policy IDs in the multiasset
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * set (and replace if it exists) all assets with policy {policy_id} to a copy of {assets}
-- * @param {ScriptHash} policy_id
-- * @param {Assets} assets
-- * @returns {Assets | undefined}
-- */
--   insert(policy_id: ScriptHash, assets: Assets): Assets | undefined;
-- /**
-- * all assets under {policy_id}, if any exist, or else None (undefined in JS)
-- * @param {ScriptHash} policy_id
-- * @returns {Assets | undefined}
-- */
--   get(policy_id: ScriptHash): Assets | undefined;
-- /**
-- * sets the asset {asset_name} to {value} under policy {policy_id}
-- * returns the previous amount if it was set, or else None (undefined in JS)
-- * @param {ScriptHash} policy_id
-- * @param {AssetName} asset_name
-- * @param {BigNum} value
-- * @returns {BigNum | undefined}
-- */
--   set_asset(policy_id: ScriptHash, asset_name: AssetName, value: BigNum): BigNum | undefined;
-- /**
-- * returns the amount of asset {asset_name} under policy {policy_id}
-- * If such an asset does not exist, 0 is returned.
-- * @param {ScriptHash} policy_id
-- * @param {AssetName} asset_name
-- * @returns {BigNum}
-- */
--   get_asset(policy_id: ScriptHash, asset_name: AssetName): BigNum;
-- /**
-- * returns all policy IDs used by assets in this multiasset
-- * @returns {ScriptHashes}
-- */
--   keys(): ScriptHashes;
-- /**
-- * removes an asset from the list if the result is 0 or less
-- * does not modify this object, instead the result is returned
-- * @param {MultiAsset} rhs_ma
-- * @returns {MultiAsset}
-- */
--   sub(rhs_ma: MultiAsset): MultiAsset;
-- }

newtype MultiAsset = MultiAsset
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor MultiAsset) MultiAssetObject
    , from_json :: EffectMth1 JsonString MultiAssetObject
    , new :: EffectMth0 MultiAssetObject
    )
  )

derive instance Newtype MultiAsset _

multiAsset
  :: { from_bytes :: MultiAsset -> Cbor MultiAsset -> Effect MultiAssetObject
     , from_json :: MultiAsset -> JsonString -> Effect MultiAssetObject
     , new :: MultiAsset -> Effect MultiAssetObject
     }
multiAsset = mkNewtypedFFI (Proxy :: Proxy MultiAsset)

newtype MultiAssetObject = MultiAssetObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor MultiAssetObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , len :: EffectMth0 Int
    , insert :: EffectMth2 ScriptHashObject AssetsObject (Opt AssetsObject)
    , get :: EffectMth1 ScriptHashObject (Opt AssetsObject)
    , set_asset :: EffectMth3 ScriptHashObject AssetNameObject BigNumObject (Opt BigNumObject)
    , get_asset :: EffectMth2 ScriptHashObject AssetNameObject BigNumObject
    , keys :: EffectMth0 ScriptHashesObject
    , sub :: EffectMth1 MultiAssetObject MultiAssetObject
    )
  )

derive instance Newtype MultiAssetObject _

multiAssetObject
  :: { free :: MultiAssetObject -> Effect Unit
     , to_bytes :: MultiAssetObject -> Effect (Cbor MultiAssetObject)
     , to_json :: MultiAssetObject -> Effect JsonString
     , to_js_value :: MultiAssetObject -> Effect Json
     , len :: MultiAssetObject -> Effect Int
     , insert :: MultiAssetObject -> ScriptHashObject -> AssetsObject -> Effect (Opt AssetsObject)
     , get :: MultiAssetObject -> ScriptHashObject -> Effect (Opt AssetsObject)
     , set_asset :: MultiAssetObject -> ScriptHashObject -> AssetNameObject -> BigNumObject -> Effect (Opt BigNumObject)
     , get_asset :: MultiAssetObject -> ScriptHashObject -> AssetNameObject -> Effect BigNumObject
     , keys :: MultiAssetObject -> Effect ScriptHashesObject
     , sub :: MultiAssetObject -> MultiAssetObject -> Effect MultiAssetObject
     }
multiAssetObject = mkNewtypedFFI (Proxy :: Proxy MultiAssetObject)

-- export class Assets {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Assets}
-- */
--   static from_bytes(bytes: Uint8Array): Assets;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {AssetsJSON}
-- */
--   to_js_value(): AssetsJSON;
-- /**
-- * @param {string} json
-- * @returns {Assets}
-- */
--   static from_json(json: string): Assets;
-- /**
-- * @returns {Assets}
-- */
--   static new(): Assets;
-- /**
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * @param {AssetName} key
-- * @param {BigNum} value
-- * @returns {BigNum | undefined}
-- */
--   insert(key: AssetName, value: BigNum): BigNum | undefined;
-- /**
-- * @param {AssetName} key
-- * @returns {BigNum | undefined}
-- */
--   get(key: AssetName): BigNum | undefined;
-- /**
-- * @returns {AssetNames}
-- */
--   keys(): AssetNames;
-- }

newtype Assets = Assets
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor AssetsObject) AssetsObject
    , from_json :: EffectMth1 JsonString AssetsObject
    , new :: EffectMth0 AssetsObject
    )
  )

derive instance Newtype Assets _

assets
  :: { from_bytes :: Assets -> Cbor AssetsObject -> Effect AssetsObject
     , from_json :: Assets -> JsonString -> Effect AssetsObject
     , new :: Assets -> Effect AssetsObject
     }
assets = mkNewtypedFFI (Proxy :: Proxy Assets)

newtype AssetsObject = AssetsObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor AssetsObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , len :: EffectMth0 Int
    , insert :: EffectMth2 AssetNameObject BigNum (Opt BigNum)
    , get :: EffectMth1 AssetNameObject (Opt BigNum)
    , keys :: EffectMth0 AssetNamesObject
    )
  )

derive instance Newtype AssetsObject _

assetsObject
  :: { free :: AssetsObject -> Effect Unit
     , to_bytes :: AssetsObject -> Effect (Cbor AssetsObject)
     , to_json :: AssetsObject -> Effect JsonString
     , to_js_value :: AssetsObject -> Effect Json
     , len :: AssetsObject -> Effect Int
     , insert :: AssetsObject -> AssetNameObject -> BigNum -> Effect (Opt BigNum)
     , get :: AssetsObject -> AssetNameObject -> Effect (Opt BigNum)
     , keys :: AssetsObject -> Effect AssetNamesObject
     }
assetsObject = mkNewtypedFFI (Proxy :: Proxy AssetsObject)

-- export class AssetName {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {AssetName}
-- */
--   static from_bytes(bytes: Uint8Array): AssetName;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {AssetNameJSON}
-- */
--   to_js_value(): AssetNameJSON;
-- /**
-- * @param {string} json
-- * @returns {AssetName}
-- */
--   static from_json(json: string): AssetName;
-- /**
-- * @param {Uint8Array} name
-- * @returns {AssetName}
-- */
--   static new(name: Uint8Array): AssetName;
-- /**
-- * @returns {Uint8Array}
-- */
--   name(): Uint8Array;
-- }

newtype AssetName = AssetName
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor AssetNameObject) AssetNameObject
    , from_json :: EffectMth1 JsonString AssetNameObject
    , new :: EffectMth1 Uint8Array AssetNameObject
    )
  )

derive instance Newtype AssetName _

assetName ::
  { from_bytes :: AssetName -> Cbor AssetNameObject -> Effect AssetNameObject
  , from_json :: AssetName -> JsonString -> Effect AssetNameObject
  , new :: AssetName -> Uint8Array -> Effect AssetNameObject
  }
assetName = mkNewtypedFFI (Proxy :: Proxy AssetName)

newtype AssetNameObject = AssetNameObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor AssetNameObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , name :: EffectMth0 Uint8Array
    )
  )

derive instance Newtype AssetNameObject _

assetNameObject
  :: { free :: AssetNameObject -> Effect Unit
     , to_bytes :: AssetNameObject -> Effect (Cbor AssetNameObject)
     , to_json :: AssetNameObject -> Effect JsonString
     , to_js_value :: AssetNameObject -> Effect Json
     , name :: AssetNameObject -> Effect Uint8Array
     }
assetNameObject = mkNewtypedFFI (Proxy :: Proxy AssetNameObject)


-- export class AssetNames {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {AssetNames}
-- */
--   static from_bytes(bytes: Uint8Array): AssetNames;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {AssetNamesJSON}
-- */
--   to_js_value(): AssetNamesJSON;
-- /**
-- * @param {string} json
-- * @returns {AssetNames}
-- */
--   static from_json(json: string): AssetNames;
-- /**
-- * @returns {AssetNames}
-- */
--   static new(): AssetNames;
-- /**
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * @param {number} index
-- * @returns {AssetName}
-- */
--   get(index: number): AssetName;
-- /**
-- * @param {AssetName} elem
-- */
--   add(elem: AssetName): void;
-- }

newtype AssetNames = AssetNames
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor AssetNamesObject) AssetNamesObject
    , from_json :: EffectMth1 JsonString AssetNamesObject
    , new :: EffectMth0 AssetNamesObject
    )
  )

derive instance Newtype AssetNames _

assetNames ::
  { from_bytes :: AssetNames -> Cbor AssetNamesObject -> Effect AssetNamesObject
  , from_json :: AssetNames -> JsonString -> Effect AssetNamesObject
  , new :: AssetNames -> Effect AssetNamesObject
  }
assetNames = mkNewtypedFFI (Proxy :: Proxy AssetNames)

newtype AssetNamesObject = AssetNamesObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor AssetNamesObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , len :: EffectMth0 Int
    , get :: EffectMth1 Int AssetNameObject
    , add :: EffectMth1 AssetNameObject Unit
    )
  )

derive instance Newtype AssetNamesObject _

assetNamesObject ::
  { free :: AssetNamesObject -> Effect Unit
  , to_bytes :: AssetNamesObject -> Effect (Cbor AssetNamesObject)
  , to_json :: AssetNamesObject -> Effect JsonString
  , to_js_value :: AssetNamesObject -> Effect Json
  , len :: AssetNamesObject -> Effect Int
  , get :: AssetNamesObject -> Int -> Effect AssetNameObject
  , add :: AssetNamesObject -> AssetNameObject -> Effect Unit
  }
assetNamesObject = mkNewtypedFFI (Proxy :: Proxy AssetNamesObject)


-- export class TransactionOutput {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionOutput}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionOutput;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {TransactionOutputJSON}
-- */
--   to_js_value(): TransactionOutputJSON;
-- /**
-- * @param {string} json
-- * @returns {TransactionOutput}
-- */
--   static from_json(json: string): TransactionOutput;
-- /**
-- * @returns {Address}
-- */
--   address(): Address;
-- /**
-- * @returns {Value}
-- */
--   amount(): Value;
-- /**
-- * @returns {Datum | undefined}
-- */
--   datum(): Datum | undefined;
-- /**
-- * @param {Datum} data
-- */
--   set_datum(data: Datum): void;
-- /**
-- * @returns {ScriptRef | undefined}
-- */
--   script_ref(): ScriptRef | undefined;
-- /**
-- * @param {ScriptRef} script_ref
-- */
--   set_script_ref(script_ref: ScriptRef): void;
-- /**
-- * @param {Address} address
-- * @param {Value} amount
-- * @returns {TransactionOutput}
-- */
--   static new(address: Address, amount: Value): TransactionOutput;
-- }
newtype TransactionOutput = TransactionOutput
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionOutputObject) TransactionOutputObject
      , from_json :: EffectMth1 JsonString TransactionOutputObject
      , new :: EffectMth2 AddressObject ValueObject TransactionOutputObject
      )
  )

derive instance Newtype TransactionOutput _

transactionOutput
  :: { from_bytes :: TransactionOutput -> (Cbor TransactionOutputObject) -> Effect TransactionOutputObject
     , from_json :: TransactionOutput -> JsonString -> Effect TransactionOutputObject
     , new :: TransactionOutput -> AddressObject -> ValueObject -> Effect TransactionOutputObject
     }
transactionOutput = mkNewtypedFFI (Proxy :: Proxy TransactionOutput)

newtype TransactionOutputObject = TransactionOutputObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , address :: EffectMth0 AddressObject
      , amount :: EffectMth0 ValueObject
      , datum :: EffectMth0 (Opt DatumObject)
      )
  )

derive instance Newtype TransactionOutputObject _

transactionOutputObject
  :: { free :: TransactionOutputObject -> Effect Unit
     , address :: TransactionOutputObject -> Effect AddressObject
     , amount :: TransactionOutputObject -> Effect ValueObject
     , datum :: TransactionOutputObject -> Effect (Opt DatumObject)
     }
transactionOutputObject = mkNewtypedFFI (Proxy :: Proxy TransactionOutputObject)

-- Placeholders:

foreign import data PlutusData :: Type

foreign import data PlutusDataObject :: Type

-- export class Datum {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Datum}
-- */
--   static from_bytes(bytes: Uint8Array): Datum;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {DatumJSON}
-- */
--   to_js_value(): DatumJSON;
-- /**
-- * @param {string} json
-- * @returns {Datum}
-- */
--   static from_json(json: string): Datum;
-- /**
-- * @param {DataHash} data_hash
-- * @returns {Datum}
-- */
--   static new_data_hash(data_hash: DataHash): Datum;
-- /**
-- * @param {PlutusData} data
-- * @returns {Datum}
-- */
--   static new_data(data: PlutusData): Datum;
-- /**
-- * @returns {number}
-- */
--   kind(): number;
-- /**
-- * @returns {DataHash | undefined}
-- */
--   as_data_hash(): DataHash | undefined;
-- /**
-- * @returns {PlutusData | undefined}
-- */
--   as_inline_data(): PlutusData | undefined;
-- }

newtype Datum = Datum
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor DatumObject) DatumObject
      , from_json :: EffectMth1 JsonString DatumObject
      , new_data_hash :: EffectMth1 DataHashObject DatumObject
      , new_data :: EffectMth1 PlutusDataObject DatumObject
      )
  )

derive instance Newtype Datum _

datum
  :: { from_bytes :: Datum -> (Cbor DatumObject) -> Effect DatumObject
     , from_json :: Datum -> JsonString -> Effect DatumObject
     , new_data_hash :: Datum -> DataHashObject -> Effect DatumObject
     , new_data :: Datum -> PlutusDataObject -> Effect DatumObject
     }
datum = mkNewtypedFFI (Proxy :: Proxy Datum)

newtype DatumObject = DatumObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , "kind" :: EffectMth0 Int
      , as_data_hash :: EffectMth0 DataHashObject
      , as_inline_data :: EffectMth0 PlutusDataObject
      )
  )

derive instance Newtype DatumObject _

datumObject
  :: { free :: DatumObject -> Effect Unit
     , "kind" :: DatumObject -> Effect Int
     , as_data_hash :: DatumObject -> Effect DataHashObject
     , as_inline_data :: DatumObject -> Effect PlutusDataObject
     }
datumObject = mkNewtypedFFI (Proxy :: Proxy DatumObject)

-- export class DataHash {
--   free(): void;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {DataHash}
-- */
--   static from_bytes(bytes: Uint8Array): DataHash;
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
-- * @returns {DataHash}
-- */
--   static from_bech32(bech_str: string): DataHash;
-- /**
-- * @returns {string}
-- */
--   to_hex(): string;
-- /**
-- * @param {string} hex
-- * @returns {DataHash}
-- */
--   static from_hex(hex: string): DataHash;
-- }

newtype DataHash = DataHash
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor DataHashObject) DataHashObject
      , from_bech32 :: EffectMth1 String DataHashObject
      , from_hex :: EffectMth1 String DataHashObject
      )
  )

derive instance Newtype DataHash _

dataHash
  :: { from_bytes :: DataHash -> (Cbor DataHashObject) -> Effect DataHashObject
     , from_bech32 :: DataHash -> String -> Effect DataHashObject
     , from_hex :: DataHash -> String -> Effect DataHashObject
     }
dataHash = mkNewtypedFFI (Proxy :: Proxy DataHash)

newtype DataHashObject = DataHashObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 (Cbor DataHashObject)
      , to_bech32 :: EffectMth1 String String
      , to_hex :: EffectMth0 (CborHex DataHashObject)
      )
  )

derive instance Newtype DataHashObject _

dataHashObject
  :: { free :: DataHashObject -> Effect Unit
     , to_bytes :: DataHashObject -> Effect (Cbor DataHashObject)
     , to_bech32 :: DataHashObject -> String -> Effect String
     , to_hex :: DataHashObject -> Effect (CborHex DataHashObject)
     }
dataHashObject = mkNewtypedFFI (Proxy :: Proxy DataHashObject)

-- export class TransactionUnspentOutput {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionUnspentOutput}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionUnspentOutput;
-- /**
-- * @param {TransactionInput} input
-- * @param {TransactionOutput} output
-- * @returns {TransactionUnspentOutput}
-- */
--   static new(input: TransactionInput, output: TransactionOutput): TransactionUnspentOutput;
-- /**
-- * @returns {TransactionInput}
-- */
--   input(): TransactionInput;
-- /**
-- * @returns {TransactionOutput}
-- */
--   output(): TransactionOutput;
-- }

newtype TransactionUnspentOutput = TransactionUnspentOutput
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionUnspentOutputObject) TransactionUnspentOutputObject
      , new :: EffectMth2 TransactionInputObject TransactionOutput TransactionUnspentOutputObject
      )
  )

derive instance Newtype TransactionUnspentOutput _

transactionUnspentOutput
  :: { from_bytes :: TransactionUnspentOutput -> (Cbor TransactionUnspentOutputObject) -> Effect TransactionUnspentOutputObject
     , new :: TransactionUnspentOutput -> TransactionInputObject -> TransactionOutput -> Effect TransactionUnspentOutputObject
     }
transactionUnspentOutput = mkNewtypedFFI (Proxy :: Proxy TransactionUnspentOutput)

newtype TransactionUnspentOutputObject = TransactionUnspentOutputObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , input :: EffectMth0 TransactionInputObject
      , output :: EffectMth0 TransactionOutputObject
      )
  )

derive instance Newtype TransactionUnspentOutputObject _

transactionUnspentOutputObject
  :: { free :: TransactionUnspentOutputObject -> Effect Unit
     , input :: TransactionUnspentOutputObject -> Effect TransactionInputObject
     , output :: TransactionUnspentOutputObject -> Effect TransactionOutputObject
     }
transactionUnspentOutputObject = mkNewtypedFFI (Proxy :: Proxy TransactionUnspentOutputObject)

-- export class TransactionInput {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionInput}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionInput;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {TransactionInputJSON}
-- */
--   to_js_value(): TransactionInputJSON;
-- /**
-- * @param {string} json
-- * @returns {TransactionInput}
-- */
--   static from_json(json: string): TransactionInput;
-- /**
-- * @returns {TransactionHash}
-- */
--   transaction_id(): TransactionHash;
-- /**
-- * @returns {BigNum}
-- */
--   index(): BigNum;
-- /**
-- * @param {TransactionHash} transaction_id
-- * @param {BigNum} index
-- * @returns {TransactionInput}
-- */
--   static new(transaction_id: TransactionHash, index: BigNum): TransactionInput;
-- }

newtype TransactionInput = TransactionInput
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionInputObject) TransactionInputObject
      , new :: EffectMth1 TransactionInputObject TransactionInputObject
      )
  )

derive instance Newtype TransactionInput _

transactionInput
  :: { from_bytes :: TransactionInput -> (Cbor TransactionInputObject) -> Effect TransactionInputObject
     , new :: TransactionInput -> TransactionInputObject -> Effect TransactionInputObject
     }
transactionInput = mkNewtypedFFI (Proxy :: Proxy TransactionInput)

newtype TransactionInputObject = TransactionInputObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , transaction_id :: EffectMth0 TransactionHashObject
      , index :: EffectMth0 BigNumObject
      )
  )

derive instance Newtype TransactionInputObject _

transactionInputObject
  :: { free :: TransactionInputObject -> Effect Unit
     , transaction_id :: TransactionInputObject -> Effect TransactionHashObject
     , index :: TransactionInputObject -> Effect BigNumObject
     }
transactionInputObject = mkNewtypedFFI (Proxy :: Proxy TransactionInputObject)

-- export class TransactionHash {
--   free(): void;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionHash}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionHash;
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
-- * @returns {TransactionHash}
-- */
--   static from_bech32(bech_str: string): TransactionHash;
-- /**
-- * @returns {string}
-- */
--   to_hex(): string;
-- /**
-- * @param {string} hex
-- * @returns {TransactionHash}
-- */
--   static from_hex(hex: string): TransactionHash;
-- }

newtype TransactionHash = TransactionHash
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionHashObject) TransactionHashObject
      , from_bech32 :: EffectMth1 String TransactionHashObject
      , from_hex :: EffectMth1 String TransactionHashObject
      )
  )

derive instance Newtype TransactionHash _

transactionHash
  :: { from_bytes :: TransactionHash -> (Cbor TransactionHashObject) -> Effect TransactionHashObject
     , from_bech32 :: TransactionHash -> String -> Effect TransactionHashObject
     , from_hex :: TransactionHash -> String -> Effect TransactionHashObject
     }
transactionHash = mkNewtypedFFI (Proxy :: Proxy TransactionHash)

newtype TransactionHashObject = TransactionHashObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 (Cbor TransactionHashObject)
      , to_bech32 :: EffectMth1 String Bech32
      , to_hex :: EffectMth0 (CborHex TransactionHashObject)
      )
  )

derive instance Newtype TransactionHashObject _

transactionHashObject
  :: { free :: TransactionHashObject -> Effect Unit
     , to_bytes :: TransactionHashObject -> Effect (Cbor TransactionHashObject)
     , to_bech32 :: TransactionHashObject -> String -> Effect Bech32
     , to_hex :: TransactionHashObject -> Effect (CborHex TransactionHashObject)
     }
transactionHashObject = mkNewtypedFFI (Proxy :: Proxy TransactionHashObject)

-- export class BigNum {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {BigNum}
-- */
--   static from_bytes(bytes: Uint8Array): BigNum;
-- /**
-- * @param {string} string
-- * @returns {BigNum}
-- */
--   static from_str(string: string): BigNum;
-- /**
-- * @returns {string}
-- */
--   to_str(): string;
-- /**
-- * @returns {BigNum}
-- */
--   static zero(): BigNum;
-- /**
-- * @returns {boolean}
-- */
--   is_zero(): boolean;
-- /**
-- * @param {BigNum} other
-- * @returns {BigNum}
-- */
--   checked_mul(other: BigNum): BigNum;
-- /**
-- * @param {BigNum} other
-- * @returns {BigNum}
-- */
--   checked_add(other: BigNum): BigNum;
-- /**
-- * @param {BigNum} other
-- * @returns {BigNum}
-- */
--   checked_sub(other: BigNum): BigNum;
-- /**
-- * returns 0 if it would otherwise underflow
-- * @param {BigNum} other
-- * @returns {BigNum}
-- */
--   clamped_sub(other: BigNum): BigNum;
-- /**
-- * @param {BigNum} other
-- * @returns {BigNum}
-- */
--   checked_div(other: BigNum): BigNum;
-- /**
-- * @param {BigNum} other
-- * @returns {BigNum}
-- */
--   checked_div_ceil(other: BigNum): BigNum;
-- /**
-- * @param {BigNum} rhs_value
-- * @returns {number}
-- */
--   compare(rhs_value: BigNum): number;
-- }

newtype BigNum = BigNum
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor BigNumObject) BigNumObject
      , from_str :: EffectMth1 String BigNumObject
      )
  )

derive instance Newtype BigNum _

bigNum
  :: { from_bytes :: BigNum -> (Cbor BigNumObject) -> Effect BigNumObject
     , from_str :: BigNum -> String -> Effect BigNumObject
     }
bigNum = mkNewtypedFFI (Proxy :: Proxy BigNum)

newtype BigNumObject = BigNumObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 (Cbor BigNumObject)
      , to_str :: EffectMth0 String
      , is_zero :: EffectMth0 Boolean
      , checked_mul :: EffectMth1 BigNum BigNum
      , checked_add :: EffectMth1 BigNum BigNum
      , checked_sub :: EffectMth1 BigNum BigNum
      , clamped_sub :: EffectMth1 BigNum BigNum
      , checked_div :: EffectMth1 BigNum BigNum
      , checked_div_ceil :: EffectMth1 BigNum BigNum
      , compare :: EffectMth1 BigNum Number
      )
  )

derive instance Newtype BigNumObject _

bigNumObject
  :: { free :: BigNumObject -> Effect Unit
     , to_bytes :: BigNumObject -> Effect (Cbor BigNumObject)
     , to_str :: BigNumObject -> Effect String
     , is_zero :: BigNumObject -> Effect Boolean
     , checked_mul :: BigNumObject -> BigNum -> Effect BigNum
     , checked_add :: BigNumObject -> BigNum -> Effect BigNum
     , checked_sub :: BigNumObject -> BigNum -> Effect BigNum
     , clamped_sub :: BigNumObject -> BigNum -> Effect BigNum
     , checked_div :: BigNumObject -> BigNum -> Effect BigNum
     , checked_div_ceil :: BigNumObject -> BigNum -> Effect BigNum
     , compare :: BigNumObject -> BigNum -> Effect Number
     }
bigNumObject = mkNewtypedFFI (Proxy :: Proxy BigNumObject)

-- export class BigInt {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {BigInt}
-- */
--   static from_bytes(bytes: Uint8Array): BigInt;
-- /**
-- * @returns {BigNum | undefined}
-- */
--   as_u64(): BigNum | undefined;
-- /**
-- * @returns {Int | undefined}
-- */
--   as_int(): Int | undefined;
-- /**
-- * @param {string} string
-- * @returns {BigInt}
-- */
--   static from_str(string: string): BigInt;
-- /**
-- * @returns {string}
-- */
--   to_str(): string;
-- }

newtype BigInt = BigInt
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor BigIntObject) BigIntObject
      , from_str :: EffectMth1 String BigIntObject
      )
  )

derive instance Newtype BigInt _

bigInt ::
  { from_bytes :: BigInt -> (Cbor BigIntObject) -> Effect BigIntObject
  , from_str :: BigInt -> String -> Effect BigIntObject
  }
bigInt = mkNewtypedFFI (Proxy :: Proxy BigInt)

newtype BigIntObject = BigIntObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 Uint8Array
      , as_u64 :: EffectMth0 (Opt BigNumObject)
      , as_int :: EffectMth0 (Opt BigIntObject)
      , to_str :: EffectMth0 String
      )
  )

derive instance Newtype BigIntObject _

bigIntObject ::
  { free :: BigIntObject -> Effect Unit
  , to_bytes :: BigIntObject -> Effect Uint8Array
  , as_u64 :: BigIntObject -> Effect (Opt BigNumObject)
  , as_int :: BigIntObject -> Effect (Opt BigIntObject)
  , to_str :: BigIntObject -> Effect String
  }
bigIntObject = mkNewtypedFFI (Proxy :: Proxy BigIntObject)

-- export class ScriptDataHash {
--   free(): void;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {ScriptDataHash}
-- */
--   static from_bytes(bytes: Uint8Array): ScriptDataHash;
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
-- * @returns {ScriptDataHash}
-- */
--   static from_bech32(bech_str: string): ScriptDataHash;
-- /**
-- * @returns {string}
-- */
--   to_hex(): string;
-- /**
-- * @param {string} hex
-- * @returns {ScriptDataHash}
-- */
--   static from_hex(hex: string): ScriptDataHash;
-- }
newtype ScriptDataHash = ScriptDataHash
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor ScriptDataHashObject) ScriptDataHashObject
      , from_bech32 :: EffectMth1 String ScriptDataHashObject
      , from_hex :: EffectMth1 String ScriptDataHashObject
      )
  )

derive instance Newtype ScriptDataHash _

scriptDataHash ::
  { from_bytes :: ScriptDataHash -> (Cbor ScriptDataHashObject) -> Effect ScriptDataHashObject
  , from_bech32 :: ScriptDataHash -> String -> Effect ScriptDataHashObject
  , from_hex :: ScriptDataHash -> String -> Effect ScriptDataHashObject
  }
scriptDataHash = mkNewtypedFFI (Proxy :: Proxy ScriptDataHash)

newtype ScriptDataHashObject = ScriptDataHashObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 Uint8Array
      , to_bech32 :: EffectMth1 String String
      , to_hex :: EffectMth0 (CborHex ScriptDataHashObject)
      )
  )

derive instance Newtype ScriptDataHashObject _

scriptDataHashObject ::
  { free :: ScriptDataHashObject -> Effect Unit
  , to_bytes :: ScriptDataHashObject -> Effect Uint8Array
  , to_bech32 :: ScriptDataHashObject -> String -> Effect String
  , to_hex :: ScriptDataHashObject -> Effect (CborHex ScriptDataHashObject)
  }
scriptDataHashObject = mkNewtypedFFI (Proxy :: Proxy ScriptDataHashObject)


foreign import data Redeemer :: Type

foreign import data RedeemerObject :: Type

foreign import data Redeemers :: Type

foreign import data RedeemersObject :: Type

foreign import data PlutusList :: Type

foreign import data PlutusListObject :: Type

