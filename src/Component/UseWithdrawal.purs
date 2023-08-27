module Component.UseWithdrawal where

import Prelude

import Component.UseWithdrawal.Blockfrost as Blockfrost
import Component.UseWithdrawal.Machine as M
import Contrib.Effect.Exception (errorToJson)
import Contrib.React.Basic.Hooks.UseMooreMachine (UseMooreMachine, useMooreMachine)
import Data.Argonaut (class EncodeJson, Json, encodeJson, jsonNull)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Foreign.Internal.Stringify (unsafeStringify)
import Marlowe.Runtime.Registry (ReferenceScriptUtxo(..))
import Marlowe.Runtime.Web.Types (TxId(..), TxOutRef(..))
import Partial.Unsafe (unsafeCrashWith)
import React.Basic.Hooks (Hook)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Wallet as Wallet

type HookApply hooks (newHook :: Type -> Type) = newHook hooks

infixl 0 type HookApply as &

payoutReferenceInputForNetwork :: Blockfrost.Network -> ReferenceScriptUtxo
payoutReferenceInputForNetwork network
  | network == Blockfrost.mainnet = ReferenceScriptUtxo $ TxOutRef { txId: TxId "672399f7d551d6e06fda70769f830e4e3783495c6250567c6ae97ecc788ad5a4", txIx: 2 }
  | network == Blockfrost.preprod = ReferenceScriptUtxo $ TxOutRef { txId: TxId "9a8a6f387a3330b4141e1cb019380b9ac5c72151c0abc52aa4266245d3c555cd", txIx: 2 }
  | network == Blockfrost.preview = ReferenceScriptUtxo $ TxOutRef { txId: TxId "69bfdb7cd911e930bfa073a8c45121e7690939d7680196181731d0dd609ecb73", txIx: 2 }
  | true = unsafeCrashWith "Blockfrost.payoutReferenceInput: Unknown network"

type Props =
  { wallet :: Wallet.Api
  , network :: Blockfrost.Network
  , txOutRef :: TxOutRef
  , blockfrostProjectId :: Blockfrost.ProjectId
  }

type HookError =
  { tag :: String
  , msg :: String
  , info :: Json
  }

noInfoError :: String -> String -> HookError
noInfoError tag msg = { tag, msg, info: jsonNull }

data HookStatus
  = AwaitingWithdrawal
    { withdraw :: Effect Unit
    , lastAttemptResult :: Maybe (Either HookError TxId)
    }
  | ProcessingWithdrawal String

instance EncodeJson HookStatus where
  encodeJson = case _ of
    AwaitingWithdrawal { lastAttemptResult } -> encodeJson
      { status: "AwaitingWithdrawal"
      , lastAttemptResult: case lastAttemptResult of
          Nothing -> pure jsonNull
          Just (Left err) ->
            Just $ encodeJson { value: err, tag: "Error" }
          Just (Right txId) ->
            Just $ encodeJson { value: txId, tag: "Success" }
      }
    ProcessingWithdrawal txId -> encodeJson
      { status: "ProcessingWithdrawal"
      , txId
      }
type UseWithdrawalHooks hooks =
  UseMooreMachine M.State M.Action M.State hooks

newtype UseWithdrawal hooks = UseWithdrawal (UseWithdrawalHooks hooks)

derive instance Newtype (UseWithdrawal hooks) _

machineStateToHookStatus
  :: Effect Unit
 -> M.State
 -> HookStatus
machineStateToHookStatus trigger = case _ of
  M.AwaitingTrigger -> AwaitingWithdrawal
    { withdraw: trigger
    , lastAttemptResult: Nothing
    }
  M.Initializing _ Nothing -> ProcessingWithdrawal "Initializing"
  M.Initializing _ (Just err) -> AwaitingWithdrawal
    { withdraw: trigger
    , lastAttemptResult: Just $ Left $ noInfoError "InitializeError" err
    }
  M.FetchingPayoutUTxO _ Nothing -> ProcessingWithdrawal "FetchingPayoutUTxO"
  M.FetchingPayoutUTxO _ (Just err) -> AwaitingWithdrawal
    { withdraw: trigger
    , lastAttemptResult: Just $ Left $ noInfoError "FetchPayoutUTxOError" err
    }
  M.FindingRoleTokenUTxO _ Nothing -> ProcessingWithdrawal "FindingRoleTokenUTxO"
  M.FindingRoleTokenUTxO _ (Just err) -> AwaitingWithdrawal
    { withdraw: trigger
    , lastAttemptResult: Just $ Left $ noInfoError "FindRoleTokenUTxOError" err
    }
  M.GrabbingCollateralUTxOs _ Nothing -> ProcessingWithdrawal "GrabbingCollateralUTxOs"
  M.GrabbingCollateralUTxOs _ (Just err) -> AwaitingWithdrawal
    { withdraw: trigger
    , lastAttemptResult: Just $ Left $ noInfoError "GrabCollateralUTxOsError" err
    }
  M.PayoutUTxOStatusChecking _ Nothing -> ProcessingWithdrawal "PayoutUTxOStatusChecking"
  M.PayoutUTxOStatusChecking _ (Just err) -> AwaitingWithdrawal
    { withdraw: trigger
    , lastAttemptResult: Just $ Left do
      case err of
        M.PayoutUTxOStatusCheckingError msg ->
          noInfoError "PayoutUTxOStatusCheckError.PayoutUTxOStatusCheckingError" msg
        M.PayoutUTxOAlreadySpent txId ->
          { tag: "PayoutUTxOStatusCheckError.PayoutUTxOAlreadySpent"
          , msg: "Payout UTxO already spent"
          , info: encodeJson txId
          }
    }
  M.BuildingTx _ Nothing -> ProcessingWithdrawal "BuildingTx"
  M.BuildingTx _ (Just err) -> AwaitingWithdrawal
    { withdraw: trigger
    , lastAttemptResult: Just $ Left $ noInfoError "BuildTxError" err
    }
  M.SigningTx _ Nothing -> ProcessingWithdrawal "SigningTx"
  M.SigningTx _ (Just err) -> AwaitingWithdrawal do
    let
      err' = case err of
        M.UserAborted -> noInfoError "SigningTxError.UserAborted" "User aborted signing operation"
        M.SignTxOperationError walletError ->
          { tag: "SigningTxError.SignTxOperationError"
          , msg: "Some error occured while signing the transaction"
          , info: unsafeCoerce walletError
          }
    { withdraw: trigger
    , lastAttemptResult: Just $ Left $ err'
    }
  M.SubmittingTx _ Nothing -> ProcessingWithdrawal "SubmittingTx"
  M.SubmittingTx _ (Just err) -> AwaitingWithdrawal do
    let
      err' = case err of
        M.WalletSubmitTxError { msg, info } ->
          { tag: "SubmittingTxError.WalletSubmitTxError"
          , msg
          , info
          }
        M.WitnessKeySetupFailed msg -> noInfoError "SubmittingTxError.WitnessKeySetupFailed" msg
        M.BlockfrostSubmitTxError { msg, info } ->
          { tag: "SubmittingTxError.BlockfrostSubmitTxError"
          , msg
          , info
          }

    { withdraw: trigger
    , lastAttemptResult: Just $ Left $ err'
    }
  M.TxCreated txId -> AwaitingWithdrawal
    { withdraw: trigger
    , lastAttemptResult: Just $ Right txId
    }
  M.DriverFailure { state, error } -> AwaitingWithdrawal do
    let
      err' =
        { tag: "DriverFailure"
        , info: errorToJson error
        , msg: "An error during driver execution with state: " <> unsafeStringify state
        }

    { withdraw: trigger
    , lastAttemptResult: Just $ Left $ err'
    }

useWithdrawal :: Props -> Hook UseWithdrawal HookStatus
useWithdrawal a = React.coerceHook React.do
  let
    triggerAction = M.Trigger
      { txOutRef: a.txOutRef
      , blockfrostProjectId: a.blockfrostProjectId
      , network: a.network
      , wallet: a.wallet
      }
    spec =
      { initialState: M.AwaitingTrigger
      , driver: M.driver
      , output: identity
      , step: M.step
      }
  { state, applyAction } <- useMooreMachine spec
  pure $ machineStateToHookStatus (applyAction triggerAction) state
