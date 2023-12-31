module WithdrawalWidget where

-- import Prelude
-- 
-- import CardanoMultiplatformLib (Bech32, CborHex(..), addressObject, allocate, allocateOpt, asksLib, bech32FromString, cborHexToHex, cborToCborHex, plutusVasilCostModels, pubKeyHashFromBech32, runGarbageCollector, transactionFromCbor, valueMapFromValueObject)
-- import CardanoMultiplatformLib (GarbageCollector, Lib, cborToCborHex, toCoinCbor) as CML
-- import CardanoMultiplatformLib.Lib as CardanoMultiplatformLib
-- import CardanoMultiplatformLib.Transaction (ScriptDataHashObject, TransactionObject, TransactionWitnessSetObject) as CML
-- import CardanoMultiplatformLib.Transaction (TransactionHashObject, TransactionObject, TransactionUnspentOutputObject, bigNumObject, dataHashObject, datumObject, scriptDataHashObject, transaction, transactionHashObject, transactionInputObject, transactionObject, transactionOutputObject, transactionUnspentOutput, transactionUnspentOutputObject, transactionWitnessSet, transactionWitnessSetObject)
-- import CardanoMultiplatformLib.Types (cborHexToCbor, cborToUint8Array, jsonStringFromJson, jsonStringToString)
-- import Component.ContractDetails as ContractDetails
-- import Component.SplittedDeposit.CreateContract as SplittedDeposit.CreateContract
-- import Component.Types (MessageHub(..), MkComponentM)
-- import Component.UseWithdrawal.Blockfrost as B
-- import Contrib.Cardano as C
-- import Contrib.Cardano.CML as C.CML
-- import Contrib.Fetch (fetchEither)
-- import Contrib.ReactSyntaxHighlighter (yamlSyntaxHighlighter)
-- import Control.Monad.Except (ExceptT(..), catchError, except, runExceptT, throwError)
-- import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
-- import Control.Monad.Reader.Class (asks)
-- import Control.Monad.Trans.Class (lift)
-- import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, fromObject, jsonNull, jsonParser, stringify, toObject, (.:), (.:?))
-- import Data.Array (find)
-- import Data.Array as Array
-- import Data.BigInt.Argonaut as BigInt
-- import Data.Either (Either(..), fromRight, hush, note)
-- import Data.Foldable (fold, foldMap, for_)
-- import Data.Function (on)
-- import Data.Int as Int
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..), fromMaybe, maybe)
-- import Data.Newtype (class Newtype, un)
-- import Data.String as String
-- import Data.Time.Duration (Minutes(..), fromDuration)
-- import Data.Traversable (for)
-- import Data.Undefined.NoProblem as NoProblem
-- import Debug (traceM)
-- import Effect (Effect)
-- import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Effect.Uncurried (EffectFn1, runEffectFn1, runEffectFn3)
-- import Effect.Unsafe (unsafePerformEffect)
-- import Fetch (Method(..))
-- import Foreign (Foreign)
-- import Foreign.Object as Object
-- import HexString (Hex, hexToString)
-- import Marlowe.Runtime.Registry (ReferenceScriptUtxo(..))
-- import Marlowe.Runtime.Web.Types (AnUTxO(..), DatumHash(..), TxId(..), TxOut(..), TxOutRef(..))
-- import Partial.Unsafe (unsafeCrashWith)
-- import React.Basic.DOM (text) as DOOM
-- import React.Basic.DOM.Simplified.Generated as DOM
-- import React.Basic.Hooks (JSX, component, useEffect, useEffectOnce, useState', (/\))
-- import React.Basic.Hooks as R
-- import React.Basic.Hooks.Aff (useAff)
-- import Unsafe.Coerce (unsafeCoerce)
-- import Utils.React.Basic.Hooks (useLoopAff, useStateWithRef')
-- import Wallet as Wallet
-- import WalletContext (WalletContext(..))
-- import WalletContext as WalletContext
-- 
-- foreign import txCborHex :: CborHex TransactionObject
-- 
-- allowedStatusCodes :: Array Int
-- allowedStatusCodes = [ 200, 201, 202, 206, 400, 401, 403, 404, 500 ]
-- 
-- type RoleTokenInfo =
--   { policyId :: C.PolicyId
--   , assetName :: C.AssetName
--   }
-- 
-- -- Example datum value response - first element is policyId, second is assetName:
-- -- {
-- --   "json_value": {
-- --     "fields": [
-- --       {
-- --         "bytes": "9af4e092022f414473cabc793d50d630f4a458aff0b215b2d5eeb6a1"
-- --       },
-- --       {
-- --         "bytes": "5630352e57697468647261776572"
-- --       }
-- --     ],
-- --     "constructor": 0
-- --   }
-- -- }
-- data PayoutInfo
--   = FetchFromBlockfrost
--       { projectId :: B.ProjectId }
--   | AlreadyFetched
--       { utxosJson :: Json
--       , datumJson :: Json
--       }
-- 
-- derive instance Eq PayoutInfo
-- 
-- instance DecodeJson PayoutInfo where
--   decodeJson json = do
--     obj <- decodeJson json
--     case Object.lookup "projectId" obj of
--       Just _ -> FetchFromBlockfrost <$> do
--         projectId <- obj .: "projectId"
--         pure { projectId }
--       Nothing -> AlreadyFetched <$> do
--         utxosJson <- obj .: "utxosJson"
--         datumJson <- obj .: "datumJson"
--         pure { utxosJson, datumJson }
-- 
-- instance EncodeJson PayoutInfo where
--   encodeJson = case _ of
--     FetchFromBlockfrost { projectId } -> encodeJson $ Object.fromFoldable
--       [ "projectId" /\ encodeJson projectId ]
--     AlreadyFetched { utxosJson, datumJson } -> encodeJson $ Object.fromFoldable
--       [ "utxosJson" /\ encodeJson utxosJson
--       , "datumJson" /\ encodeJson datumJson
--       ]
-- 
-- type Props =
--   { wallet :: Wallet.Api
--   , network :: B.Network
--   , txOutRef :: TxOutRef
--   , payoutInfoJson :: Json
--   , onError :: EffectFn1 String Unit
--   , onSuccess :: EffectFn1 String Unit
--   }
-- 
-- oneAdaInLovelace :: C.Lovelace
-- oneAdaInLovelace = case C.Lovelace <$> BigInt.fromString "1000000" of
--   Just l -> l
--   Nothing -> unsafeCrashWith "oneAdaInLovelace"
-- 
-- twoAdaInLovelace :: C.Lovelace
-- twoAdaInLovelace = case C.Lovelace <$> BigInt.fromString "2000000" of
--   Just l -> l
--   Nothing -> unsafeCrashWith "twoAdaInLovelace"
-- 
-- fourAdaInLovelace :: C.Lovelace
-- fourAdaInLovelace = case C.Lovelace <$> BigInt.fromString "4000000" of
--   Just l -> l
--   Nothing -> unsafeCrashWith "fourAdaInLovelace"
-- 
-- mkComponent :: MkComponentM (Props -> JSX)
-- mkComponent = do
--   MessageHub msgHubProps <- asks _.msgHub
--   contractDetails <- ContractDetails.mkComponent
--   createContractComponent <- SplittedDeposit.CreateContract.mkComponent
--   walletInfoCtx <- asks _.walletInfoCtx
--   slotting <- asks _.slotting
--   let
--     txCbor = cborHexToCbor txCborHex
-- 
--   cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
--   txBodyJson <- map jsonStringToString $ liftEffect $ runGarbageCollector cardanoMultiplatformLib $ do
--     txBody <- transactionFromCbor txCbor
--     -- txBodyObjectClass <- asksLib _."TransactionBodyObject"
--     liftEffect $ transactionObject.to_json txBody
-- 
--   liftEffect $ component "WithdrawalWidget" \{ wallet, network, txOutRef, payoutInfoJson, onError, onSuccess } -> R.do
--     let
--       ReferenceScriptUtxo payoutReferenceTxOutRef = payoutReferenceInputForNetwork network
--     possiblePayoutInfo /\ possiblePayoutInfoRef /\ setPayoutInfo <- useStateWithRef' Nothing
--     useEffectOnce do
--       case decodeJson payoutInfoJson of
--         Right payoutInfo -> setPayoutInfo $ payoutInfo
--         Left err -> do
--           runEffectFn1 onError $ "Failed to decode payoutInfo: " <> show err
--           pure unit
--       pure $ pure unit
-- 
--     possiblePayoutUTxO /\ possiblePayoutUTxORef /\ setPayoutUTxO <- useStateWithRef' Nothing
-- 
--     -- We want to call `fetchPayoutUTxO`
--     useAff possiblePayoutInfo do
--       (liftEffect $ R.readRef possiblePayoutInfoRef) >>= case _ of
--         Nothing -> pure unit
--         Just req -> do
--           res <- case req of
--             FetchFromBlockfrost { projectId } -> do
--               fetchPayoutUTxO cardanoMultiplatformLib txOutRef projectId network
--             AlreadyFetched { utxosJson, datumJson } -> do
--               pure $ hush $ parsePayoutUTxOs cardanoMultiplatformLib (PayoutTxOutRef txOutRef) { datumJson, utxos }
--           liftEffect $ case res of
--             Nothing -> runEffectFn1 onError $ "Failed to fetch or decode payout info"
--             Just payoutUTxO -> setPayoutUTxO $ Just payoutUTxO
-- 
--     possibleWalletContext /\ possibleWalletContextRef /\ setWalletContext <- useStateWithRef' Nothing
--     useLoopAff unit (fromDuration $ Minutes 1.0) $
--       WalletContext.walletContext cardanoMultiplatformLib wallet >>= (setWalletContext >>> liftEffect)
-- 
--     let
--       possibleWithdrawer = do
--         PayoutUTxO payoutUTxO <- possiblePayoutUTxO
--         WalletContext walletContext <- possibleWalletContext
--         let
--           roleAssetId = C.AssetId
--             payoutUTxO.roleToken.policyId
--             payoutUTxO.roleToken.assetName
--         pure $
--           if C.selectAsset walletContext.balance roleAssetId /= mempty then true
--           else false
-- 
--     possibleRoleTokenUTxO /\ setRoleTokenUTxO <- useState' Nothing
--     useAff possibleWithdrawer do
--       (liftEffect $ R.readRef possiblePayoutUTxORef) >>= case _ of
--         Nothing -> pure unit
--         Just (PayoutUTxO { roleToken }) -> do
--           res <- selectRoleTokenTxOutRef cardanoMultiplatformLib roleToken wallet
--           liftEffect $ case res of
--             Nothing -> runEffectFn1 onError $ "Failed to select role token"
--             Just utxo -> setRoleTokenUTxO $ Just utxo
-- 
--     possibleCollaterals /\ setCollaterals <- useState' Nothing
--     useAff possibleWithdrawer do
--       res <- getCollateralUTxOs cardanoMultiplatformLib fourAdaInLovelace wallet
--       liftEffect $ case res of
--         Nothing -> runEffectFn1 onError $ "Failed to get collateral"
--         Just collaterals -> setCollaterals $ Just collaterals
-- 
--     possibleChangeAddress /\ setChangeAddress <- useState' Nothing
--     useEffect possibleWithdrawer do
--       R.readRef possibleWalletContextRef >>= case _ of
--         Nothing -> pure unit
--         Just (WalletContext { changeAddress }) -> do
--           possibleCborHex <- liftEffect $ pubKeyHashFromBech32 cardanoMultiplatformLib $ changeAddress
--           setChangeAddress $ { bech32: changeAddress, pubKeyHash: _ } <<< cborHexToHex <$> possibleCborHex
--       pure $ pure unit
-- 
--     let
--       ctx = do
--         roleTokenUTxO <- possibleRoleTokenUTxO
--         collaterals <- possibleCollaterals
--         changeAddress <- possibleChangeAddress
--         payoutUTxO <- possiblePayoutUTxO
--         pure
--           { roleTokenUTxO
--           , collaterals
--           , changeAddress
--           , payoutUTxO
--           }
--     useAff ctx do
--       for_ ctx \{ roleTokenUTxO, collaterals, changeAddress, payoutUTxO } -> do
--         let
--           PayoutUTxO { utxo: payoutUTxO', roleToken: roleTokenInfo } = payoutUTxO
--           -- FIXME!!!
--         possibleTx <- liftEffect $ buildTx
--           cardanoMultiplatformLib
--           changeAddress
--           roleTokenInfo
--           { collaterals, roleToken: roleTokenUTxO, payout: payoutUTxO' }
--           payoutReferenceTxOutRef
--           Nothing
--         for possibleTx \tx -> do
--           possibleWitnessSet <- Wallet.signTx wallet tx true
--           possibleJsonString <- for possibleWitnessSet \cborHex -> liftEffect $ runGarbageCollector cardanoMultiplatformLib do
--             _WitnessSet <- asksLib _."TransactionWitnessSet"
--             let
--               cbor = cborHexToCbor cborHex
--             witnessSetObj <- allocate $ transactionWitnessSet.from_bytes _WitnessSet cbor
--             liftEffect $ transactionWitnessSetObject.to_json witnessSetObj
--           let
--             vkeysJson = do
--               jsonStr <- jsonStringToString <$> hush possibleJsonString
--               json <- (jsonParser jsonStr # hush)
--               obj <- toObject json
--               vkeys <- Object.lookup "vkeys" obj
--               pure $ VKeysJson vkeys
--           possibleTx' <- liftEffect $ buildTx
--             cardanoMultiplatformLib
--             changeAddress
--             roleTokenInfo
--             { collaterals, roleToken: roleTokenUTxO, payout: payoutUTxO' }
--             payoutReferenceTxOutRef
--             vkeysJson
--           for possibleTx' \tx' -> do
--             -- res <- Wallet.submitTx wallet tx'
--             -- traceM res
--             (liftEffect $ R.readRef possiblePayoutInfoRef) >>= case _ of
--               Nothing -> pure unit
--               Just req -> do
--                 case req of
--                   FetchFromBlockfrost { projectId } -> do
--                     res <- B.submitTx projectId network tx'
--                     traceM res
--                   _ -> traceM "Not submitting tx"
-- 
--     pure $ do
--       case ctx of
--         Nothing -> DOOM.text "Loading..."
--         Just { roleTokenUTxO, collaterals, changeAddress, payoutUTxO } -> DOM.ul {}
--           [ DOM.li {} ([] :: Array JSX)
--               -- let
--               --   PayoutUTxO { utxo: payoutUTxO', roleToken: roleTokenInfo } = payoutUTxO
--               --   -- FIXME!!!
--               --   refTxOutRef = TxOutRef
--               --     { txId: TxId "1164e038f4f7a620aa7ef4c1cc762c4166792e5545a9852f"
--               --     , txIx: 0
--               --     }
--               --   json = buildTxJson changeAddress roleTokenInfo { collateral, roleToken: roleTokenUTxO, payout: payoutUTxO' } refTxOutRef
--               -- yamlSyntaxHighlighter json {}
--           , DOM.li {} do
--               let
--                 json = fromRight (fromObject Object.empty) $ jsonParser txBodyJson
--               yamlSyntaxHighlighter json {}
--           , DOM.li {} do -- $ DOOM.text $ stringify $ encodeJson txOutRef
--               let
--                 json = encodeJson txOutRef
--               yamlSyntaxHighlighter json {}
--           , DOM.li {} $ do
--               let
--                 json = encodeJson collaterals
--               yamlSyntaxHighlighter json {}
--           ]
-- 
-- -- DOOM.text $ stringify $ encodeJson payoutUTxO
-- 
-- -- Okey so this is an reference "Json" object:
-- --
-- -- body:
-- --   inputs:
-- --     # this is collateral: 16.993272 ADA
-- --     - transaction_id: 1164e038f4f7a620aa7ef4c1cc762c4166792e5545a9852f322a3a2465d74268
-- --       index: '0'
-- --     # this is role token: 4.0 ADA
-- --     - transaction_id: 53243962f3019cc9118818f57ab641add17d166a87a4f9ea68ac330d1e96f7e4
-- --       index: '1'
-- --     # this is the txOutRef we want to withdraw from: 1.31455 ADA
-- --     - transaction_id: ecf8f8a584935e379dd8419af39a3fa83b1da1c85b97b4bd7d2c8d1089e91345
-- --       index: '2'
-- --   outputs:
-- --     - address: addr_test1vq0acgkfkgeeuezdy2fn2y5mxhn9zcvrjesxxen4k2d2t2qdwp3ce
-- --       amount:
-- --         coin: '20.594148'
-- --         multiasset: null
-- --       datum_option: null
-- --       script_ref: null
-- --     - address: addr_test1vq0acgkfkgeeuezdy2fn2y5mxhn9zcvrjesxxen4k2d2t2qdwp3ce
-- --       amount:
-- --         coin: '1.340410'
-- --         multiasset:
-- --           74b199c8abded51c21aff264f6f962e40d9c8bff0c5ed3611cce7087:
-- --             5630312e5072696365546f6b656e3031: '1'
-- --             5630312e5072696365546f6b656e3032: '1'
-- --           9b1f1f4d412b47a1bacf2b9635b02f703770acedcfd40236a1080323:
-- --             '57697468647261776572': '1'
-- --       datum_option: null
-- --       script_ref: null
-- --   fee: '373264'
-- --   collateral:
-- --     - transaction_id: 1164e038f4f7a620aa7ef4c1cc762c4166792e5545a9852f322a3a2465d74268
-- --       index: '0'
-- --   required_signers:
-- --     - 1fdc22c9b2339e644d229335129b35e65161839660636675b29aa5a8
-- --   network_id: null
-- --   total_collateral: '559896'
-- --   collateral_return:
-- --     address: addr_test1vq0acgkfkgeeuezdy2fn2y5mxhn9zcvrjesxxen4k2d2t2qdwp3ce
-- --     amount:
--          --      16.993272
-- --       coin: '16433376'
-- --       multiasset: null
-- --     datum_option: null
-- --     script_ref: null
-- --   reference_inputs:
-- --     - transaction_id: 001c4e145938d2d7afe1f3a17360dc4cf9efffab4fe9e76f3ac6351c9e937537
-- --       index: '2'
-- --   script_data_hash: null # let's test null - that was original value: 5e0fe2245fae63ab56cdac27ecc34d021a06032a51fc182585f5da976f9a1e2d
-- --   ttl: null
-- --   certs: null
-- --   withdrawals: null
-- --   update: null
-- --   auxiliary_data_hash: null
-- --   validity_start_interval: null
-- --   mint: null
-- -- witness_set:
-- --   vkeys: null
-- --   native_scripts: null
-- --   bootstraps: null
-- --   plutus_v1_scripts: null
-- --   plutus_data:
-- --     elems:
-- --       - >-
-- --         {"constructor":0,"fields":[{"bytes":"9b1f1f4d412b47a1bacf2b9635b02f703770acedcfd40236a1080323"},{"bytes":"57697468647261776572"}]}
-- --     definite_encoding: true
-- --   redeemers:
-- --     - tag: Spend
-- --       index: '2'
-- --       data: '{"constructor":0,"fields":[]}'
-- --       ex_units:
-- --         mem: '2237238'
-- --         steps: '609068390'
-- --   plutus_v2_scripts: null
-- --
-- -- is_valid: true
-- -- auxiliary_data: null
-- --
-- -- Let's construct nearly the same structure given three utxos.
-- --
-- -- We can use this handy trick that in PureScript we can encode records of simpel types out of the box:
-- --
-- -- x = encodeJson { x: { y: 8 }}
-- --
-- -- Unfortunatelly we have to encode every place which contains nulls as well.
-- --
-- -- Additionally we want to build the transaction in the same order of fields as above.
-- 
-- 
-- -- export interface TransactionJSON {
-- --   auxiliary_data?: AuxiliaryDataJSON | null;
-- --   body: TransactionBodyJSON;
-- --   is_valid: boolean;
-- --   witness_set: TransactionWitnessSetJSON;
-- -- }
-- -- export type TransactionBodiesJSON = TransactionBodyJSON[];
-- -- export interface TransactionBodyJSON {
-- --   auxiliary_data_hash?: string | null;
-- --   certs?: CertificatesJSON | null;
-- --   collateral?: TransactionInputsJSON | null;
-- --   collateral_return?: TransactionOutputJSON | null;
-- --   fee: string;
-- --   inputs: TransactionInputsJSON;
-- --   mint?: MintJSON | null;
-- --   network_id?: NetworkIdJSON | null;
-- --   outputs: TransactionOutputsJSON;
-- --   reference_inputs?: TransactionInputsJSON | null;
-- --   required_signers?: Ed25519KeyHashesJSON | null;
-- --   script_data_hash?: string | null;
-- --   total_collateral?: string | null;
-- --   ttl?: string | null;
-- --   update?: UpdateJSON | null;
-- --   validity_start_interval?: string | null;
-- --   withdrawals?: {
-- --     [k: string]: ProtocolParamUpdateJSON;
-- --   } | null;
-- -- }
