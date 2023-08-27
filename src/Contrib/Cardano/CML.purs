module Contrib.Cardano.CML where

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
import Data.Foldable (fold, foldMap, for_)
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

txIdFromCborHex :: CborHex TransactionHashObject -> TxId
txIdFromCborHex cborHex = TxId $ hexToString $ cborHexToHex cborHex

-- Let's extract from the above part which constructs TxOutRef
transactionUnspentOutputToUTxO :: TransactionUnspentOutputObject -> CML.GarbageCollector AnUTxO
transactionUnspentOutputToUTxO unspentTxOutObj = do
  _TransactionUnspentOutput <- asksLib _."TransactionUnspentOutput"
  inputObj <- allocate $ transactionUnspentOutputObject.input unspentTxOutObj
  txHashObj <- allocate $ transactionInputObject.transaction_id inputObj
  txHashCborHex <- liftEffect $ transactionHashObject.to_hex txHashObj

  txOutObj <- allocate $ transactionUnspentOutputObject.output unspentTxOutObj
  valueObj <- allocate $ transactionOutputObject.amount txOutObj
  value <- fromRight mempty <<< C.valueFromNestedMaps <$> valueMapFromValueObject valueObj

  possibleDatumObj <- allocateOpt $ transactionOutputObject.datum txOutObj
  datumHash <- case NoProblem.toMaybe possibleDatumObj of
    Nothing -> pure $ Nothing
    Just datumObj -> do
      dataHashObj <- allocate $ datumObject.as_data_hash datumObj
      CborHex dataHashHex <- liftEffect $ dataHashObject.to_hex dataHashObj
      pure $ Just $ DatumHash $ hexToString dataHashHex

  addressObj <- allocate $ transactionOutputObject.address txOutObj
  address <- liftEffect $ addressObject.to_bech32 addressObj NoProblem.undefined

  let
    txId = txIdFromCborHex txHashCborHex
  bigNumObj <- allocate $ transactionInputObject.index inputObj
  txIxStr <- liftEffect $ bigNumObject.to_str bigNumObj
  let
    txIx = fromMaybe 0 $ Int.fromString txIxStr
    txOutRef = TxOutRef { txId, txIx }

    txOut = TxOut { address, value, datumHash }
  pure $ AnUTxO { txOutRef, txOut }

transactionUnspentOutputCBorHexToUTxO :: CborHex TransactionUnspentOutputObject -> CML.GarbageCollector AnUTxO
transactionUnspentOutputCBorHexToUTxO cborHex = do
  _TransactionUnspentOutput <- asksLib _."TransactionUnspentOutput"
  unspentTxOutObj <- allocate $ transactionUnspentOutput.from_bytes _TransactionUnspentOutput (cborHexToCbor cborHex)
  transactionUnspentOutputToUTxO unspentTxOutObj

