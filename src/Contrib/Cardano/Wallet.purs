module Contrib.Cardano.Wallet where

import Prelude

import CardanoMultiplatformLib (CborHex, runGarbageCollector)
import CardanoMultiplatformLib (Lib, cborToCborHex, toCoinCbor) as CML
import CardanoMultiplatformLib.Transaction (TransactionUnspentOutputObject)
import Contrib.Cardano as C
import Contrib.Cardano.CML as C.CML
import Control.Monad.Except (catchError, throwError)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Either (fromRight, hush)
import Data.Foldable (fold, foldMap, foldr, length)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Debug (traceM)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Marlowe.Runtime.Web.Types (AnUTxO(..), TxOut(..))
import Wallet as Wallet

getCollateralUTxOs :: CML.Lib -> C.Lovelace -> Wallet.Api -> Aff (Maybe (Array AnUTxO))
getCollateralUTxOs cardanoMultiplatformLib lovelace@(C.Lovelace lovelaceValue) wallet = do
  let
    -- 20 ADA is the minimum collateral
    maxLovelaceValue = C.Lovelace $ BigInt.fromInt 50_000_000
  when (lovelace > maxLovelaceValue) do
    throwError $ error $ "Collateral must be less than " <> show (unwrap maxLovelaceValue) <> " lovelace"

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
        check (AnUTxO { txOut: TxOut { value } }) = C.isLovelaceOnly value && C.selectLovelace value >= lovelace && C.selectLovelace value < maxLovelaceValue
      -- decode all the utxos and then filter by value
      liftEffect $ runGarbageCollector cardanoMultiplatformLib do
        Array.filter check <$> for utxos \(utxo :: CborHex TransactionUnspentOutputObject) ->
          C.CML.transactionUnspentOutputCBorHexToUTxO utxo

    fetchAnyUTxOs = do
      possibleUTxOs <- Wallet.getUtxos wallet
      let
        utxos = fromRight [] (map fold possibleUTxOs)
        -- decode all the utxos and then filter by value
        check =
          (\l -> l >= lovelace && l < maxLovelaceValue)
          <<< utxoLovelace
      liftEffect $ runGarbageCollector cardanoMultiplatformLib do
        Array.filter check <$> for utxos \(utxo :: CborHex TransactionUnspentOutputObject) ->
          C.CML.transactionUnspentOutputCBorHexToUTxO utxo

  unsorted <- do
    utxos <- tryWallet `catchError` \_ -> pure Nothing
    if utxos == Nothing || utxos == Just []
      then do
        pureLovelaceUTxOs <- fetchPureLovelaceUTxOs
        Just <$> if length pureLovelaceUTxOs > 0
          then do
            traceM "Using pure lovelace utxos"
            pure pureLovelaceUTxOs
          else do
            traceM "Using any utxos"
            anyUTxOs <- fetchAnyUTxOs
            traceM $ anyUTxOs
            pure anyUTxOs
      else do
        pure utxos
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
  traceM $ foldMap utxoLovelace <$> collaterals
  pure collaterals

