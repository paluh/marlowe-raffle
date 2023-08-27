module Component.UseWithdrawal.TxBuilder where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex(..), allocate, asksLib, cborToCborHex, plutusVasilCostModels, runGarbageCollector)
import CardanoMultiplatformLib (GarbageCollector, Lib) as CML
import CardanoMultiplatformLib.Lib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (ScriptDataHashObject, TransactionObject, TransactionWitnessSetObject) as CML
import CardanoMultiplatformLib.Transaction (TransactionObject, scriptDataHashObject, transaction, transactionObject, transactionWitnessSet, transactionWitnessSetObject)
import CardanoMultiplatformLib.Types (CborHex, JsonString, cborToCborHex) as CML
import CardanoMultiplatformLib.Types (cborHexToCbor, jsonStringFromJson, jsonStringToString)
import Contrib.Cardano as C
import Control.Monad.Except (ExceptT(..), except, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, isNull, jsonNull, jsonParser, stringify)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, un)
import Data.Traversable (for)
import Data.Undefined.NoProblem as NoProblem
import Debug (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn3)
import Foreign.Object (Object)
import Foreign.Object as Object
import HexString (Hex, hexToString)
import Marlowe.Runtime.Web.Types (AnUTxO(..), TxOut(..), TxOutRef(..))

-- Reference Json object for an transaction:
--
-- body:
--   inputs:
--     # this is collateral: 16.993272 ADA
--     - transaction_id: 1164e038f4f7a620aa7ef4c1cc762c4166792e5545a9852f322a3a2465d74268
--       index: '0'
--     # this is role token: 4.0 ADA
--     - transaction_id: 53243962f3019cc9118818f57ab641add17d166a87a4f9ea68ac330d1e96f7e4
--       index: '1'
--     # this is the txOutRef we want to withdraw from: 1.31455 ADA
--     - transaction_id: ecf8f8a584935e379dd8419af39a3fa83b1da1c85b97b4bd7d2c8d1089e91345
--       index: '2'
--   outputs:
--     - address: addr_test1vq0acgkfkgeeuezdy2fn2y5mxhn9zcvrjesxxen4k2d2t2qdwp3ce
--       amount:
--         coin: '20.594148'
--         multiasset: null
--       datum_option: null
--       script_ref: null
--     - address: addr_test1vq0acgkfkgeeuezdy2fn2y5mxhn9zcvrjesxxen4k2d2t2qdwp3ce
--       amount:
--         coin: '1.340410'
--         multiasset:
--           74b199c8abded51c21aff264f6f962e40d9c8bff0c5ed3611cce7087:
--             5630312e5072696365546f6b656e3031: '1'
--             5630312e5072696365546f6b656e3032: '1'
--           9b1f1f4d412b47a1bacf2b9635b02f703770acedcfd40236a1080323:
--             '57697468647261776572': '1'
--       datum_option: null
--       script_ref: null
--   fee: '373264'
--   collateral:
--     - transaction_id: 1164e038f4f7a620aa7ef4c1cc762c4166792e5545a9852f322a3a2465d74268
--       index: '0'
--   required_signers:
--     - 1fdc22c9b2339e644d229335129b35e65161839660636675b29aa5a8
--   network_id: null
--   total_collateral: '559896'
--   collateral_return:
--     address: addr_test1vq0acgkfkgeeuezdy2fn2y5mxhn9zcvrjesxxen4k2d2t2qdwp3ce
--     amount:
         --      16.993272
--       coin: '16433376'
--       multiasset: null
--     datum_option: null
--     script_ref: null
--   reference_inputs:
--     - transaction_id: 001c4e145938d2d7afe1f3a17360dc4cf9efffab4fe9e76f3ac6351c9e937537
--       index: '2'
--   script_data_hash: null # let's test null - that was original value: 5e0fe2245fae63ab56cdac27ecc34d021a06032a51fc182585f5da976f9a1e2d
--   ttl: null
--   certs: null
--   withdrawals: null
--   update: null
--   auxiliary_data_hash: null
--   validity_start_interval: null
--   mint: null
-- witness_set:
--   vkeys: null
--   native_scripts: null
--   bootstraps: null
--   plutus_v1_scripts: null
--   plutus_data:
--     elems:
--       - >-
--         {"constructor":0,"fields":[{"bytes":"9b1f1f4d412b47a1bacf2b9635b02f703770acedcfd40236a1080323"},{"bytes":"57697468647261776572"}]}
--     definite_encoding: true
--   redeemers:
--     - tag: Spend
--       index: '2'
--       data: '{"constructor":0,"fields":[]}'
--       ex_units:
--         mem: '2237238'
--         steps: '609068390'
--   plutus_v2_scripts: null
--
-- is_valid: true
-- auxiliary_data: null
--
-- Let's construct nearly the same structure given three utxos.
--
-- We can use this handy trick that in PureScript we can encode records of simpel types out of the box:
--
-- x = encodeJson { x: { y: 8 }}
--
-- Unfortunatelly we have to encode every place which contains nulls as well.
--
-- Additionally we want to build the transaction in the same order of fields as above.

newtype PayoutReferenceScriptTxOutRef = PayoutReferenceScriptTxOutRef TxOutRef

derive instance Newtype PayoutReferenceScriptTxOutRef _

derive newtype instance EncodeJson PayoutReferenceScriptTxOutRef

buildTx
  :: CardanoMultiplatformLib.Lib
  -> { bech32 :: Bech32, pubKeyHash :: Hex }
  -> C.NonAdaAssetId
  -> { collaterals :: Array AnUTxO, roleToken :: AnUTxO, payout :: AnUTxO }
  -> PayoutReferenceScriptTxOutRef
  -> Maybe VKeysJson
  -> Effect (Either String (CborHex TransactionObject))
buildTx cml address roleTokenInfo utxos referenceTxOutRef possibleVKeysJson = runExceptT do
  txJson <- ExceptT $ buildTxJson cml address roleTokenInfo utxos referenceTxOutRef possibleVKeysJson
  lift $ runGarbageCollector cml do
    _Transaction <- asksLib _."Transaction"
    transactionObj <- allocate $ transaction.from_json _Transaction (jsonStringFromJson txJson)
    cbor <- liftEffect $ transactionObject.to_bytes transactionObj
    json <- liftEffect $ transactionObject.to_json transactionObj
    traceM json
    pure $ cborToCborHex cbor

newtype VKeysJson = VKeysJson Json
derive instance Newtype VKeysJson _

buildTxJson
  :: CML.Lib
  -> { bech32 :: Bech32, pubKeyHash :: Hex }
  -> C.NonAdaAssetId
  -> { collaterals :: Array AnUTxO, roleToken :: AnUTxO, payout :: AnUTxO }
  -> PayoutReferenceScriptTxOutRef
  -> Maybe VKeysJson
  -> Effect (Either String Json)
buildTxJson cml address roleTokenInfo { collaterals, roleToken, payout } referenceTxOutRef possibleVKeysJson = runExceptT do
  let
    origInputs = collaterals <> [ roleToken, payout ]
    encodeTxOutRef (TxOutRef { txId, txIx }) =
      encodeJson { transaction_id: txId, index: show txIx }
    encodeInput (AnUTxO { txOutRef }) = encodeTxOutRef txOutRef
    inputs = map encodeInput origInputs
    utxoValue (AnUTxO { txOut: TxOut { value } }) = value
    utxoTxOutRef (AnUTxO { txOutRef }) = txOutRef
    redeemerIndex = fromMaybe 0 $ Array.elemIndex (utxoTxOutRef payout) $ Array.sort $ origInputs <#> utxoTxOutRef

    encodeValue value = do
      let
        C.Lovelace loveLace = C.selectLovelace value
      nonAdaValue <- C.valueFromNestedMaps $ Map.delete "" $ C.valueToNestedMaps value
      pure
        { coin: BigInt.toString loveLace
        , multiasset:
            if nonAdaValue == mempty then encodeJson jsonNull
            else encodeJson nonAdaValue
        }

  let
    -- `exUnits` declaration affects the final fee amount and through that the total collateral amount.
    -- These were the original values from our reference transaction (with 2 tokens):
    --   fee: '373264'
    --       ex_units:
    --         mem: '2237238'
    --         steps: '609068390'
    exUnits = { mem: "3237238", steps: "809068390" }
    feeInt = 500000 -- 3593849
    fee = C.lovelaceFromInt feeInt

    -- * Total collateral is fee dependent and has to fullfil ledger rule (current collateralPercent is 150):
    --    totalColateral * 100 >= txfee txb * (collateralPercent pp)
    -- So in the current setup we have to really provide collateral which is 1.5 times bigger than the fee.
    totalCollateral = feeInt * 150 / 100
    totalCollateralValue = C.lovelaceToValue $ C.lovelaceFromInt totalCollateral

  traceM "totalCollateral"
  traceM $ show totalCollateral


  amount <- except do
    let
      value = C.subtractValues (foldMap utxoValue origInputs) (C.lovelaceToValue fee)
    encodeValue value

  let
    output =
      { address: address.bech32
      , amount
      , datum_option: jsonNull
      , script_ref: jsonNull
      }

  -- Like above but the pieces of the elems array should be grabbed from roleTokenInfo
  let
    witnessSetJson = encodeJson
      { vkeys: maybe jsonNull (un VKeysJson) possibleVKeysJson
      , native_scripts: jsonNull
      , bootstraps: jsonNull
      , plutus_v1_scripts: jsonNull
      , plutus_data:
          { elems:
              map (stringify <<< encodeJson)
                [ { constructor: 0
                  , fields: do
                      let
                        C.NonAdaAssetId policyId assetName = roleTokenInfo
                      [ { bytes: C.policyIdToHexString policyId }
                      , { bytes: C.assetNameToHexString assetName }
                      ]
                  }
                ]
          , definite_encoding: true
          }
      , redeemers:
          [ { tag: "Spend"
            , index: show redeemerIndex
            , data: stringify <<< encodeJson $ { constructor: 0, fields: ([] :: Array Json) }
            , ex_units: exUnits
            }
          ]
      , plutus_v2_scripts: jsonNull
      }
  scriptDataHashStr <- do
    possibleHash <- liftEffect $ runGarbageCollector cml do
      witnessSetObject <- witnessSetJsonToWitnessSet witnessSetJson
      witnessSetToScriptDataHash witnessSetObject
    case possibleHash of
      Nothing -> throwError "Could not compute script data hash"
      Just (CborHex h) -> do
        pure $ hexToString h

  collateralReturnAmount <- except do
    encodeValue $ C.subtractValues (foldMap utxoValue collaterals) totalCollateralValue

  let
    json =  encodeJson
      { body:
          { inputs: inputs -- I think that there is internal bug which reorders the inputs -- let me check ;-)
          , outputs: [ output ]
          , fee: C.lovelaceToString fee
          , collateral: map encodeInput collaterals
          , required_signers: [ hexToString address.pubKeyHash ]
          , network_id: jsonNull
          , total_collateral: show totalCollateral
          , collateral_return: do
              { address: address.bech32
              , amount: collateralReturnAmount
              , datum_option: jsonNull
              , script_ref: jsonNull
              }
          , reference_inputs:
              [ encodeTxOutRef $ un PayoutReferenceScriptTxOutRef referenceTxOutRef ]
          , script_data_hash: scriptDataHashStr
          , ttl: jsonNull
          , certs: jsonNull
          , withdrawals: jsonNull
          , update: jsonNull
          , auxiliary_data_hash: jsonNull
          , validity_start_interval: jsonNull
          , mint: jsonNull
          }
      , witness_set: witnessSetJson
      , is_valid: encodeJson true
      , auxiliary_data: jsonNull
      }
  pure json

witnessSetJsonToWitnessSet
  :: Json
  -> CML.GarbageCollector CML.TransactionWitnessSetObject
witnessSetJsonToWitnessSet witnessSetJson = do
  witnessSetClass <- asksLib _."TransactionWitnessSet"
  allocate $ transactionWitnessSet.from_json witnessSetClass (jsonStringFromJson witnessSetJson)

witnessSetToScriptDataHash
  :: CML.TransactionWitnessSetObject
  -> CML.GarbageCollector (Maybe (CborHex CML.ScriptDataHashObject))
witnessSetToScriptDataHash witnessSetObj = do
  costModels <- plutusVasilCostModels
  -- MEMORY LEAK:
  possibleRedeemersObj <- liftEffect $ transactionWitnessSetObject.redeemers witnessSetObj
  possiblePlutusDataObj <- liftEffect $ transactionWitnessSetObject.plutus_data witnessSetObj

  -- script_data_hash: 5e0fe2245fae63ab56cdac27ecc34d021a06032a51fc182585f5da976f9a1e2d
  for (NoProblem.toMaybe possibleRedeemersObj) \redeemersObj -> do
    hash_script_data <- asksLib _."hash_script_data"
    scriptDataHashObj <- allocate $ runEffectFn3 hash_script_data redeemersObj costModels.pv2 possiblePlutusDataObj
    liftEffect $ scriptDataHashObject.to_hex scriptDataHashObj

-- We decode both args into JSON, then we grab "body.witness_set" and merge "vkeys" of the two JSON objects.
addWitnessSetVKeyToTx
  :: CML.Lib
  -> CborHex CML.TransactionWitnessSetObject
  -> CML.CborHex CML.TransactionObject
  -> Effect (Either String (CML.CborHex CML.TransactionObject))
addWitnessSetVKeyToTx cml txWitnessSetHex txHex = runExceptT $ do
  let
    txWitnessSetHexToJson :: Effect CML.JsonString
    txWitnessSetHexToJson = runGarbageCollector cml do
      _TransactionWitnessSet <- asksLib _."TransactionWitnessSet"
      let
        cbor = cborHexToCbor txWitnessSetHex
      txWitnessSetObj <- allocate $ transactionWitnessSet.from_bytes _TransactionWitnessSet cbor
      liftEffect $ transactionWitnessSetObject.to_json txWitnessSetObj

    txHexToJson :: Effect CML.JsonString
    txHexToJson = runGarbageCollector cml do
      _Transaction <- asksLib _."Transaction"
      let
        cbor = cborHexToCbor txHex
      txObj <- allocate $ transaction.from_bytes _Transaction cbor
      liftEffect $ transactionObject.to_json txObj

  txWitnessSetJsonStr <- liftEffect txWitnessSetHexToJson
  txJsonStr <- liftEffect txHexToJson
  let
    possibleTxJson' :: Either String Json
    possibleTxJson' = do
      txJson <- jsonParser' $ jsonStringToString txJsonStr
      txWitnessSetJson <- jsonParser' $ jsonStringToString txWitnessSetJsonStr

      txObj <- decodeJson' txJson
      (newVkeys :: Array Json) <- do
        obj <- decodeJson' txWitnessSetJson
        fold <$> lookupOptDecode "vkeys" obj
      witnessSet <- lookupDecode "witness_set" txObj
      (origVkeys :: Array Json) <- fold <$> lookupOptDecode "vkeys" witnessSet

      let
        updatedVkeys = origVkeys <> newVkeys
        witnessSet' = encodeInsert "vkeys" updatedVkeys witnessSet
      pure $ encodeJson $ encodeInsert "witness_set" witnessSet' txObj

  txJson' <- except $ lmap (\err -> "Witness set update failed on Json operations: " <> err) possibleTxJson'
  cbor <- liftEffect $ runGarbageCollector cml do
    _Transaction <- asksLib _."Transaction"
    txObj <- allocate $ transaction.from_json _Transaction $ jsonStringFromJson txJson'
    liftEffect $ transactionObject.to_bytes txObj
  pure $ CML.cborToCborHex cbor

decodeJson' :: forall t. DecodeJson t => Json -> Either String t
decodeJson' = lmap show <<< decodeJson

jsonParser' :: String -> Either String Json
jsonParser' = lmap show <<< jsonParser

lookupDecode :: forall b. DecodeJson b => String -> Object Json -> Either String b
lookupDecode n obj = do
  case Object.lookup n obj of
    Just json -> do
      let
        reportErr err =
          "Could not decode key " <> n <> show err <> " in an object: " <> stringify (encodeJson obj)
      lmap reportErr $ decodeJson json
    Nothing -> Left $ "Could not find key " <> n <> "in an object: " <> stringify (encodeJson obj)

lookupOptDecode :: forall b. DecodeJson b => String -> Object Json -> Either String (Maybe b)
lookupOptDecode n obj = do
  case (Object.lookup n obj) of
    Just json -> do
      if isNull json
      then pure Nothing
      else do
        let
          reportErr err =
            "Could not decode key " <> n <> show err <> " in an object: " <> stringify (encodeJson obj)
        map Just $ lmap reportErr $ decodeJson json
    Nothing -> pure Nothing

encodeInsert :: forall a. EncodeJson a => String -> a -> Object Json -> Object Json
encodeInsert k v obj = Object.insert k (encodeJson v) obj

