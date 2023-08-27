module Contrib.Cardano.Wallet where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex(..), addressObject, allocate, allocateOpt, asksLib, bech32FromString, cborHexToHex, cborToCborHex, plutusVasilCostModels, pubKeyHashFromBech32, runGarbageCollector, transactionFromCbor, valueMapFromValueObject)
import CardanoMultiplatformLib (GarbageCollector, Lib, cborToCborHex, toCoinCbor) as CML
import CardanoMultiplatformLib.Lib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (ScriptDataHashObject, TransactionObject, TransactionWitnessSetObject) as CML
import CardanoMultiplatformLib.Transaction (TransactionHashObject, TransactionObject, TransactionUnspentOutputObject, bigNumObject, dataHashObject, datumObject, scriptDataHashObject, transaction, transactionHashObject, transactionInputObject, transactionObject, transactionOutputObject, transactionUnspentOutput, transactionUnspentOutputObject, transactionWitnessSet, transactionWitnessSetObject)
import CardanoMultiplatformLib.Types (cborHexToCbor, cborToUint8Array, jsonStringFromJson, jsonStringToString)
import Component.ContractDetails as ContractDetails
import Component.SplittedDeposit.CreateContract as SplittedDeposit.CreateContract
import Component.Types (MessageHub(..), MkComponentM)
import Component.UseWithdrawal.Blockfrost as B
import Contrib.Cardano as C
import Contrib.Cardano.CML as C.CML
import Contrib.Fetch (fetchEither)
import Contrib.ReactSyntaxHighlighter (yamlSyntaxHighlighter)
import Control.Monad.Except (ExceptT(..), catchError, except, runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, fromObject, jsonNull, jsonParser, stringify, toObject, (.:), (.:?))
import Data.Array (find)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..), fromRight, hush, note)
import Data.Foldable (fold, foldMap, foldr, for_)
import Data.Function (on)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, un)
import Data.String as String
import Data.Time.Duration (Minutes(..), fromDuration)
import Data.Traversable (for)
import Data.Undefined.NoProblem as NoProblem
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Fetch (Method(..))
import Foreign (Foreign)
import Foreign.Object as Object
import HexString (Hex, hexToString)
import Marlowe.Runtime.Registry (ReferenceScriptUtxo(..))
import Marlowe.Runtime.Web.Types (AnUTxO(..), DatumHash(..), TxId(..), TxOut(..), TxOutRef(..))
import Partial.Unsafe (unsafeCrashWith)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, useEffect, useEffectOnce, useState', (/\))
import React.Basic.Hooks as R
import React.Basic.Hooks.Aff (useAff)
import Unsafe.Coerce (unsafeCoerce)
import Utils.React.Basic.Hooks (useLoopAff, useStateWithRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))
import WalletContext as WalletContext

getCollateralUTxOs :: CML.Lib -> C.Lovelace -> Wallet.Api -> Aff (Maybe (Array AnUTxO))
getCollateralUTxOs cardanoMultiplatformLib lovelace@(C.Lovelace lovelaceValue) wallet = do
  let
    utxoLovelace (AnUTxO { txOut: TxOut { value } }) = C.selectLovelace value
    tryWallet = do
      coinCborHex <- liftEffect $ runGarbageCollector cardanoMultiplatformLib do
        coinCbor <- CML.toCoinCbor lovelaceValue
        pure $ CML.cborToCborHex coinCbor
      possibleUTxOs <- (hush <$> Wallet.getCollateral wallet coinCborHex)
      for possibleUTxOs \utxos -> for utxos \utxoCborHex -> liftEffect $ runGarbageCollector cardanoMultiplatformLib do
        C.CML.transactionUnspentOutputCBorHexToUTxO utxoCborHex

    fetchPureLovelaceUTxOs = do
      possibleUTxOs <- Wallet.getUtxos wallet
      let
        utxos = fromRight [] (map fold possibleUTxOs)
        check (AnUTxO { txOut: TxOut { value } }) = C.isLovelaceOnly value
      -- decode all the utxos and then filter by value
      liftEffect $ runGarbageCollector cardanoMultiplatformLib do
        Array.filter check <$> for utxos \(utxo :: CborHex TransactionUnspentOutputObject) ->
          C.CML.transactionUnspentOutputCBorHexToUTxO utxo

  unsorted <- tryWallet -- `catchError` \_ -> Just <$> fetchPureLovelaceUTxOs
  let
    step utxo accum@{ utxos, total } = do
      if total > lovelace
        then accum
        else do
          let
            total' = total <> utxoLovelace utxo
          { utxos: Array.cons utxo utxos, total: total' }
    takeEnough = _.utxos <<< foldr step { utxos: [], total: mempty }
    collaterals = takeEnough <<< Array.sortBy (compare `on` utxoLovelace) <$> unsorted
  traceM "collaterals"
  traceM collaterals
  traceM $ foldMap utxoLovelace <$> collaterals
  pure collaterals

