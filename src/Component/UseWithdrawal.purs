module Component.UseWithdrawal where

import Prelude

import Component.UseWithdrawal.Blockfrost as Blockfrost
import Component.UseWithdrawal.Machine as M
import Contrib.Effect.Exception (errorToJson)
import Contrib.React.Basic.Hooks.UseMooreMachine (UseMooreMachine, useMooreMachine)
import Data.Argonaut (Json, encodeJson, jsonNull)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Foreign.Internal.Stringify (unsafeStringify)
import Marlowe.Runtime.Registry (ReferenceScriptUtxo(..))
import Marlowe.Runtime.Web.Types (TxId(..), TxOutRef(..))
import Partial.Unsafe (unsafeCrashWith)
import React.Basic.Hooks (Hook, UseEffect, useEffect)
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
  = Initializing String
  | InitializationFailed HookError
  | AwaitingWithdrawalTrigger (Effect Unit)
  | ProcessingWithdrawal String
  | WithdrawalFailed
    { error :: HookError
    , retry :: Effect Unit
    }
  | WithdrawalSucceeded TxId
  | FatalError HookError

foreign import data TsHookStatus :: Type

-- Let's encode HookStatus as TS/JS friendly type
encodeTsHookStatus :: HookStatus -> TsHookStatus
encodeTsHookStatus = do
  let
    coerceToTsHookStatus :: forall a. a -> TsHookStatus
    coerceToTsHookStatus = unsafeCoerce
  case _ of
    Initializing msg -> coerceToTsHookStatus
      { status: "Initializing"
      , step: msg
      }
    InitializationFailed error -> coerceToTsHookStatus
      { status: "InitializationFailed"
      , error: encodeJson error
      }
    AwaitingWithdrawalTrigger trigger -> coerceToTsHookStatus
      { status: "AwaitingWithdrawalTrigger"
      , trigger
      }
    ProcessingWithdrawal msg -> coerceToTsHookStatus
      { status: "ProcessingWithdrawal"
      , msg: msg
      }
    WithdrawalFailed { error, retry } -> coerceToTsHookStatus
      { status: "WithdrawalFailed"
      , error: encodeJson error
      , retry
      }
    WithdrawalSucceeded txId -> coerceToTsHookStatus
      { status: "WithdrawalSucceeded"
      , txId: encodeJson txId
      }
    FatalError error -> coerceToTsHookStatus
      { status: "FatalError"
      , error: encodeJson error
      }

type UseWithdrawalHooks hooks =
  UseMooreMachine M.State M.Action M.State hooks

newtype UseWithdrawal hooks =
  UseWithdrawal ((UseWithdrawalHooks hooks) & UseEffect Unit)

derive instance Newtype (UseWithdrawal hooks) _

machineStateToHookStatus
  :: Effect Unit
 -> M.State
 -> HookStatus
machineStateToHookStatus trigger = case _ of
  M.Initializing _ Nothing -> Initializing "Setup"
  M.Initializing _ (Just err) -> InitializationFailed $
    noInfoError "InitializeError" err
  M.FetchingPayoutUTxO _ Nothing -> Initializing "FetchingPayoutUTxO"
  M.FetchingPayoutUTxO _ (Just err) -> InitializationFailed  $
    noInfoError "FetchPayoutUTxOError" err
  M.FindingRoleTokenUTxO _ Nothing -> Initializing "FindingRoleTokenUTxO"
  M.FindingRoleTokenUTxO _ (Just err) -> InitializationFailed $
    noInfoError "FindRoleTokenUTxOError" err
  M.PayoutUTxOStatusChecking _ Nothing -> Initializing "PayoutUTxOStatusChecking"
  M.PayoutUTxOStatusChecking _ (Just err) -> InitializationFailed
    case err of
      M.PayoutUTxOStatusCheckingError msg ->
        noInfoError "PayoutUTxOStatusCheckError" msg
      M.PayoutUTxOAlreadySpent txId ->
        { tag: "PayoutUTxOAlreadySpentError"
        , msg: "Payout UTxO already spent"
        , info: encodeJson txId
        }
  M.AwaitingWithdrawalTrigger _ -> AwaitingWithdrawalTrigger trigger
  M.GrabbingCollateralUTxOs _ Nothing -> ProcessingWithdrawal "GrabbingCollateralUTxOs"
  M.GrabbingCollateralUTxOs _ (Just err) -> WithdrawalFailed
    { error: noInfoError "GrabCollateralUTxOsError" err
    , retry: trigger
    }
  M.BuildingTx _ Nothing -> ProcessingWithdrawal "BuildingTx"
  M.BuildingTx _ (Just err) -> WithdrawalFailed
    { error: noInfoError "BuildTxError" err
    , retry: trigger
    }
  M.SigningTx _ Nothing -> ProcessingWithdrawal "SigningTx"
  M.SigningTx _ (Just err) -> WithdrawalFailed
    { error: case err of
        M.UserAborted -> noInfoError "UserAbortedError" "User aborted signing operation"
        M.SignTxOperationError walletError ->
          { tag: "SignTxOperationError"
          , msg: "Some error occured while signing the transaction"
          , info: unsafeCoerce walletError
          }
    , retry: trigger
    }
  M.SubmittingTx _ Nothing -> ProcessingWithdrawal "SubmittingTx"
  M.SubmittingTx _ (Just err) -> WithdrawalFailed do
    let
      error = case err of
        M.WalletSubmitTxError { msg, info } ->
          { tag: "WalletSubmitTxError"
          , msg
          , info
          }
        M.WitnessKeySetupFailed msg -> noInfoError "WitnessKeySetupFailed" msg
        M.BlockfrostSubmitTxError { msg, info } ->
          { tag: "BlockfrostSubmitTxError"
          , msg
          , info
          }

    { retry: trigger
    , error
    }
  M.TxCreated txId -> WithdrawalSucceeded txId
  M.DriverFailure { state, error } -> FatalError do
    { tag: "FatalError"
    , info: errorToJson error
    , msg: "An unhandled exception during execution with internal state: " <> unsafeStringify state
    }

useWithdrawal :: Props -> Hook UseWithdrawal { status :: HookStatus, reset :: Props -> Effect Unit }
useWithdrawal ctx = React.coerceHook React.do
  let
    spec initialCtx =
      { initialState: M.Initializing initialCtx Nothing
      , driver: M.driver
      , output: identity
      , step: M.step
      }
  { state, applyAction, reset } <- useMooreMachine (spec ctx)

  useEffect unit do
    applyAction $ M.Trigger
    pure $ pure unit

  let
    reset' newCtx = do
      applyAction' <- reset $ Just (spec newCtx)
      applyAction' M.Trigger
      pure unit

  pure
    { status: do
        machineStateToHookStatus
            (applyAction M.WithdrawalTrigger)
            state
        -- encodeTsHookStatus status
    , reset: reset'
    }

