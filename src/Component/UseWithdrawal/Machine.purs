module Component.UseWithdrawal.Machine where

import Prelude

import CardanoMultiplatformLib (CborHex, allocate, asksLib, pubKeyHashFromBech32, runGarbageCollector, valueMapFromValueObject)
import CardanoMultiplatformLib (Lib, cborHexToHex, importLib) as CML
import CardanoMultiplatformLib.Lib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionUnspentOutputObject, TransactionWitnessSetObject, transactionOutputObject, transactionUnspentOutput, transactionUnspentOutputObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.UseWithdrawal.Blockfrost as B
import Component.UseWithdrawal.Blockfrost as Blockfrost
import Component.UseWithdrawal.TxBuilder (PayoutReferenceScriptTxOutRef(..), addWitnessSetVKeyToTx, buildTx)
import Contrib.Cardano as C
import Contrib.Cardano.CML as C.CML
import Contrib.Cardano.Wallet as C.Wallet
import Contrib.Data.Foldable (foldMFlipped)
import Contrib.Fetch (fetchErrorToJson)
import Contrib.React.Basic.Hooks.UseMooreMachine (MooreMachineDriver, MooreMachineStep)
import Control.Error.Util (exceptNoteA, hushT)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, (.:))
import Data.Array (find)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..), either, fromRight, hush, note)
import Data.Foldable (fold)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Traversable (for)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Effect
import Foreign.Internal.Stringify (unsafeStringify)
import Marlowe.Runtime.Web.Types (AnUTxO(..), DatumHash, TxId(..), TxOut(..), TxOutRef(..))
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Wallet as Wallet
import WalletContext (WalletContext(..))
import WalletContext as WalletContext

newtype PayoutUTxO = PayoutUTxO
  { roleToken ::
      { policyId :: C.PolicyId
      , assetName :: C.AssetName
      }
  , utxo :: AnUTxO
  }

derive instance Eq PayoutUTxO
derive newtype instance EncodeJson PayoutUTxO
derive newtype instance DecodeJson PayoutUTxO

payoutReferenceInputForNetwork :: B.Network -> Maybe PayoutReferenceScriptTxOutRef
payoutReferenceInputForNetwork network
  | network == B.mainnet = Just $ PayoutReferenceScriptTxOutRef $ TxOutRef { txId: TxId "074fc62f0eb2571ff816d2d76d3f6824bec0bb9f3c040a61942f3a1a5a92bd7a", txIx: 2 }
  | network == B.preprod = Just $ PayoutReferenceScriptTxOutRef $ TxOutRef { txId: TxId "c59678b6892ba0fbeeaaec22d4cbde17026ff614ed47cea02c47752e5853ebc8", txIx: 2 }
  | network == B.preview = Just $ PayoutReferenceScriptTxOutRef $ TxOutRef { txId: TxId "07ee392718487daeeb6b972e6813f527530eaf7184a31b001d8072a5ae76915d", txIx: 2 }
  -- New
  -- | network == B.mainnet = Just $ PayoutReferenceScriptTxOutRef $ TxOutRef { txId: TxId "672399f7d551d6e06fda70769f830e4e3783495c6250567c6ae97ecc788ad5a4", txIx: 2 }
  -- | network == B.preprod = Just $ PayoutReferenceScriptTxOutRef $ TxOutRef { txId: TxId "9a8a6f387a3330b4141e1cb019380b9ac5c72151c0abc52aa4266245d3c555cd", txIx: 2 }
  -- | network == B.preview = Just $ PayoutReferenceScriptTxOutRef $ TxOutRef { txId: TxId "69bfdb7cd911e930bfa073a8c45121e7690939d7680196181731d0dd609ecb73", txIx: 2 }
  | true = Nothing

type ExecutionCtxBase r =
  { wallet :: Wallet.Api
  , network :: Blockfrost.Network
  , blockfrostProjectId :: Blockfrost.ProjectId
  | r
  }

type ExecutionCtx r = ExecutionCtxBase
  ( walletContext :: WalletContext
  , payoutReferenceInput :: PayoutReferenceScriptTxOutRef
  , cml :: CML.Lib
  | r
  )

data PayoutUTxOStatusCheckingError
  = PayoutUTxOStatusCheckingError String
  | PayoutUTxOAlreadySpent TxId

data SigningTxError
  = UserAborted
  | SignTxOperationError
      (Variant (Wallet.SignTxError + ()))

data SubmittingTxError
  = WalletSubmitTxError { msg :: String, info :: Json }
  | WitnessKeySetupFailed String
  | BlockfrostSubmitTxError { msg :: String, info :: Json }

data State
  = Initializing
      (ExecutionCtxBase (txOutRef :: TxOutRef))
      (Maybe String)
  | FetchingPayoutUTxO
      (ExecutionCtx (txOutRef :: TxOutRef))
      (Maybe String)
  | FindingRoleTokenUTxO
      (ExecutionCtx (payoutUTxO :: PayoutUTxO))
      (Maybe String)
  | PayoutUTxOStatusChecking
      (ExecutionCtx (payoutUTxO :: PayoutUTxO, roleTokenUTxO :: AnUTxO))
      (Maybe PayoutUTxOStatusCheckingError)
  | AwaitingWithdrawalTrigger
      (ExecutionCtx (payoutUTxO :: PayoutUTxO, roleTokenUTxO :: AnUTxO))
  | GrabbingCollateralUTxOs
      (ExecutionCtx (payoutUTxO :: PayoutUTxO, roleTokenUTxO :: AnUTxO))
      (Maybe String)
  | BuildingTx
      (ExecutionCtx (payoutUTxO :: PayoutUTxO, roleTokenUTxO :: AnUTxO, collateralUTxOs :: NonEmptyArray AnUTxO))
      (Maybe String)
  | SigningTx
      (ExecutionCtx (tx :: CborHex TransactionObject))
      (Maybe SigningTxError)
  | SubmittingTx
      (ExecutionCtx (tx :: CborHex TransactionObject, txWitnessSet :: CborHex TransactionWitnessSetObject))
      (Maybe SubmittingTxError)
  | TxCreated TxId
  | DriverFailure
    { state :: State
    , error :: Effect.Error
    }

data Action
  = Trigger
  | InitializationError String
  | InitializationSuccess
      { walletContext :: WalletContext
      , payoutReferenceInput :: PayoutReferenceScriptTxOutRef
      , cml :: CML.Lib
      }
  | FetchPayoutUTxOError String
  | FetchPayoutUTxOSuccess PayoutUTxO
  | FindRoleTokenUTxOError String
  | FindRoleTokenUTxOSuccess AnUTxO
  | WithdrawalTrigger
  | GrabCollateralUTxOs AnUTxO
  | GrabCollateralUTxOsError String
  | GrabCollateralUTxOsSuccess (NonEmptyArray AnUTxO)
  | PayoutUTxOStatusCheckError PayoutUTxOStatusCheckingError
  | PayoutUTxOStatusCheckSuccess
  | BuildTxError String
  | BuildTxSuccess (CborHex TransactionObject)
  | SigningTxError SigningTxError
  | SigningTxSuccess (CborHex TransactionWitnessSetObject)
  | SubmittingTxError SubmittingTxError
  | SubmittingTxSuccess TxId
  | DriverFailed { error :: Effect.Error, state :: State }

-- type MooreMachineStep state action = state -> action -> state

insert'
  :: forall r1 r2 @l a
   . IsSymbol l
  => Row.Lacks l r1
  => Row.Cons l a r1 r2
  => a
  -> { | r1 }
  -> { | r2 }
insert' = Record.insert (Proxy :: Proxy l)

delete'
  :: forall r1 r2 @l a
   . IsSymbol l
  => Row.Lacks l r1
  => Row.Cons l a r1 r2
  => Record r2
  -> Record r1
delete' = Record.delete (Proxy :: Proxy l)

step :: MooreMachineStep State Action
step (Initializing ctx _) Trigger = Initializing ctx Nothing
step (Initializing ctx _) (InitializationError err) = Initializing ctx (Just err)
step (Initializing ctx _) (InitializationSuccess { cml, walletContext, payoutReferenceInput }) = do
  let
    ctx' :: ExecutionCtx (txOutRef :: TxOutRef)
    ctx' =
      insert' @"walletContext" walletContext
      $ insert' @"payoutReferenceInput" payoutReferenceInput
      $ insert' @"cml" cml ctx
  FetchingPayoutUTxO ctx' Nothing
step (FetchingPayoutUTxO ctx _) (FetchPayoutUTxOError err) = FetchingPayoutUTxO ctx (Just err)
step (FetchingPayoutUTxO ctx _) (FetchPayoutUTxOSuccess payoutUTxO) = do
  let
    ctx' :: ExecutionCtx (payoutUTxO :: PayoutUTxO)
    ctx' = insert' @"payoutUTxO" payoutUTxO $ delete' @"txOutRef" ctx
  FindingRoleTokenUTxO ctx' Nothing
step (FindingRoleTokenUTxO ctx _) (FindRoleTokenUTxOError err) =
  FindingRoleTokenUTxO ctx (Just err)
step (FindingRoleTokenUTxO ctx _) (FindRoleTokenUTxOSuccess roleTokenUTxO) =
  PayoutUTxOStatusChecking (insert' @"roleTokenUTxO" roleTokenUTxO ctx) Nothing
step (PayoutUTxOStatusChecking ctx _) (PayoutUTxOStatusCheckError err) =
  PayoutUTxOStatusChecking ctx (Just err)
step (PayoutUTxOStatusChecking ctx _) PayoutUTxOStatusCheckSuccess =
  AwaitingWithdrawalTrigger ctx
step (AwaitingWithdrawalTrigger ctx) WithdrawalTrigger =
  GrabbingCollateralUTxOs ctx Nothing
step (GrabbingCollateralUTxOs ctx _) (GrabCollateralUTxOsError err) =
  GrabbingCollateralUTxOs ctx (Just err)
step (GrabbingCollateralUTxOs ctx _) (GrabCollateralUTxOsSuccess collateralUTxOs) =
  BuildingTx (insert' @"collateralUTxOs" collateralUTxOs ctx) Nothing
step (BuildingTx ctx _) (BuildTxError err) =
  BuildingTx ctx (Just err)
step (BuildingTx ctx _) (BuildTxSuccess tx) = do
  let
    ctx' :: ExecutionCtx (tx :: CborHex TransactionObject)
    ctx' =
      { tx
      , wallet: ctx.wallet
      , walletContext: ctx.walletContext
      , network: ctx.network
      , payoutReferenceInput: ctx.payoutReferenceInput
      , cml: ctx.cml
      , blockfrostProjectId: ctx.blockfrostProjectId
      }
  SigningTx ctx' Nothing
step (SigningTx ctx _) (SigningTxError err) =
  SigningTx ctx (Just err)
step (SigningTx ctx _) (SigningTxSuccess txWitnessSet) = do
  let
    ctx' = insert' @"txWitnessSet" txWitnessSet ctx
  SubmittingTx ctx' Nothing
step (SubmittingTx ctx _) (SubmittingTxError err) =
  SubmittingTx ctx (Just err)
step (SubmittingTx _ _) (SubmittingTxSuccess txId) =
  TxCreated $ txId
step _ (DriverFailed failureCtx) = DriverFailure failureCtx
step state _ = state

runExceptT' :: forall m b e a. Monad m => (e -> b) -> (a -> b) -> ExceptT e m a -> m b
runExceptT' errAction successAction em = do
  runExceptT em <#> either errAction successAction

driver :: MooreMachineDriver State Action
driver state = do
  (actualDriver state) <#> \go ->
    go `catchError` \error -> do
      pure $ DriverFailed { state, error }

actualDriver :: MooreMachineDriver State Action
actualDriver (Initializing ctx Nothing) = Just do
  possibleCml <- CML.importLib
  case possibleCml of
    Just cml -> do
      possibleWalletContext <- WalletContext.walletContext cml ctx.wallet
      case possibleWalletContext of
        Just walletContext -> do
          let
            possiblePayoutReferenceInput = payoutReferenceInputForNetwork ctx.network
          case possiblePayoutReferenceInput of
            Just payoutReferenceInput -> pure $ InitializationSuccess { walletContext, payoutReferenceInput, cml }
            Nothing -> pure $ InitializationError "Failed to initialize payout reference input"
        Nothing -> pure $ InitializationError "Failed to initialize wallet context"
    Nothing -> pure $ InitializationError "Failed to import CML"
actualDriver (FetchingPayoutUTxO ctx Nothing) = Just do
  fetchPayoutUTxO ctx.cml ctx.txOutRef ctx.blockfrostProjectId ctx.network <#> case _ of
    Nothing -> FetchPayoutUTxOError "UTxO not found or parsing failed"
    Just payoutUTxO -> FetchPayoutUTxOSuccess payoutUTxO
actualDriver (FindingRoleTokenUTxO ctx Nothing) = Just do
  let
    roleTokenInfo = do
      let
        PayoutUTxO { roleToken } = ctx.payoutUTxO
      roleToken
  selectRoleTokenTxOutRef ctx.cml roleTokenInfo ctx.wallet >>= case _ of
    Nothing -> pure $ FindRoleTokenUTxOError $ "Role token not found" <> show roleTokenInfo
    Just utxo -> pure $ FindRoleTokenUTxOSuccess utxo
actualDriver (GrabbingCollateralUTxOs ctx Nothing) = Just do
  let
    twoAdaInLovelace :: C.Lovelace
    twoAdaInLovelace = case C.Lovelace <$> BigInt.fromString "2000000" of
      Just l -> l
      Nothing -> unsafeCrashWith "twoAdaInLovelace"
    -- threeAdaInLovelace :: C.Lovelace
    -- threeAdaInLovelace = case C.Lovelace <$> BigInt.fromString "3000000" of
    --   Just l -> l
    --   Nothing -> unsafeCrashWith "threeAdaInLovelace"
    -- eightAdaInLovelace :: C.Lovelace
    -- eightAdaInLovelace = case C.Lovelace <$> BigInt.fromString "8000000" of
    --   Just l -> l
    --   Nothing -> unsafeCrashWith "eightAdaInLovelace"
  C.Wallet.getCollateralUTxOs ctx.cml twoAdaInLovelace ctx.wallet >>= (_ >>= NonEmptyArray.fromArray) >>> case _ of
    Nothing -> pure $ GrabCollateralUTxOsError "Collateral UTxOs not found"
    Just utxos -> pure $ GrabCollateralUTxOsSuccess utxos
actualDriver (PayoutUTxOStatusChecking ctx Nothing) =
  Just $ either PayoutUTxOStatusCheckError (const PayoutUTxOStatusCheckSuccess) <$> runExceptT do
    let
      { payoutUTxO, blockfrostProjectId, network, cml } = ctx
      PayoutUTxO { utxo, roleToken } = payoutUTxO
      AnUTxO { txOutRef: TxOutRef { txId: payoutTxId }} = utxo

    possibleConsumingTxId <- withExceptT (PayoutUTxOStatusCheckingError <<< unsafeStringify) $ do
        let
          roleTokenAssetId = C.NonAdaAssetId roleToken.policyId roleToken.assetName
        -- In order to detect if a given UTxO was consumed we have to (?!) in our case
        -- fetch all the txs of the role token and check if the txId of the payout UTxO
        txs <- Array.reverse <<< Array.fromFoldable <$>
          B.foldAssetTxs blockfrostProjectId network roleTokenAssetId (\res -> B.Continue <<< flip List.Cons res) List.Nil

        foldMFlipped Nothing txs case _, _ of
          Just res, _ -> pure $ Just res
          Nothing, assetTxId -> do
            { inputs } <- B.fetchTxUTxOs cml blockfrostProjectId network assetTxId
            let
              check (AnUTxO { txOutRef: TxOutRef { txId: inputTxId }}) = inputTxId == payoutTxId
            if Array.any check inputs
              then pure $ Just assetTxId
              else pure $ Nothing
    case possibleConsumingTxId of
      Just txId -> throwError $ PayoutUTxOAlreadySpent txId
      Nothing -> pure unit
actualDriver (BuildingTx ctx Nothing) = Just $ runExceptT' BuildTxError BuildTxSuccess do
    let
      { payoutUTxO: PayoutUTxO { roleToken, utxo: payoutUTxO }} = ctx
      { cml, walletContext, roleTokenUTxO, collateralUTxOs } = ctx
      { payoutReferenceInput } = ctx
      WalletContext { changeAddress } = walletContext
      roleTokenAssetId = C.NonAdaAssetId roleToken.policyId roleToken.assetName

    pubKeyHash <- flip exceptNoteA "bech32 to hash conversion failed" $ liftEffect $  pubKeyHashFromBech32 cml changeAddress
    ExceptT $ liftEffect $ buildTx
      cml
      { bech32: changeAddress, pubKeyHash: CML.cborHexToHex pubKeyHash }
      roleTokenAssetId
      { collaterals: collateralUTxOs, roleToken: roleTokenUTxO, payout: payoutUTxO }
      payoutReferenceInput
      Nothing
actualDriver (SigningTx ctx Nothing) = Just do
  runExceptT' (SigningTxError <<< SignTxOperationError) SigningTxSuccess do
    ExceptT $ Wallet.signTx ctx.wallet ctx.tx true
actualDriver (SubmittingTx ctx Nothing) = Just do
  let
    { blockfrostProjectId, tx, cml, txWitnessSet } = ctx
  runExceptT' SubmittingTxError SubmittingTxSuccess do
    tx <-
      withExceptT (WitnessKeySetupFailed <<< unsafeStringify) $ ExceptT $ liftEffect $ addWitnessSetVKeyToTx cml txWitnessSet tx
    let
      submitThroughBlockfrost :: _ TxId
      submitThroughBlockfrost = do
        let
          fromBlockfrostErr fetchError =  BlockfrostSubmitTxError { msg: "Fetching error", info: fetchErrorToJson fetchError }
        resJson <- withExceptT fromBlockfrostErr $ ExceptT $ B.submitTx blockfrostProjectId ctx.network tx
        txId <- do
          except $ lmap (const $ BlockfrostSubmitTxError { msg: "Response parsing failed", info: resJson }) $ decodeJson resJson
        pure txId

      submitThroughWallet = do
        let
          { wallet } = ctx
          fromWalletError variant = WalletSubmitTxError
            { msg: "Wallet error", info: unsafeCoerce variant }
        txIdCborHex <- withExceptT fromWalletError $ ExceptT $ Wallet.submitTx wallet tx
        pure $ C.CML.txIdFromCborHex txIdCborHex
        -- ExceptT $ map (lmap unsafeStringify) $ Wallet.submitTx wallet tx'
    submitThroughWallet `catchError` \err -> do
      Console.error "wallet submission failed:"
      Console.error $ unsafeStringify err
      Console.error "trying blockfrost submission..."
      submitThroughBlockfrost

actualDriver (TxCreated _) = Nothing
-- All the cases with errors
actualDriver _ = Nothing

-- Actual dirty work

newtype PayoutTxOutRef = PayoutTxOutRef TxOutRef

type RoleTokenInfo =
  { policyId :: C.PolicyId
  , assetName :: C.AssetName
  }

fetchPayoutUTxO :: CML.Lib -> TxOutRef -> B.ProjectId -> B.Network -> Aff (Maybe PayoutUTxO)
fetchPayoutUTxO cml txOutRef projectId network = runMaybeT do
  let
    TxOutRef { txId } = txOutRef
  { outputs: utxos } <- hushT $ B.fetchTxUTxOs cml projectId network txId
  datumHash <- MaybeT $ pure $ hush $ parsePayoutDatumHash (PayoutTxOutRef txOutRef) utxos
  datumJson <- MaybeT $ B.fetchDatum projectId network datumHash
  MaybeT $ pure $ hush $ parsePayoutUTxOs cml (PayoutTxOutRef txOutRef) { datumJson, utxos }

-- Given { datumJson, utxosJson } returns a list of PayoutUTxO
parsePayoutUTxOs :: CML.Lib -> PayoutTxOutRef -> { datumJson :: Json, utxos :: Array AnUTxO } -> Either JsonDecodeError PayoutUTxO
parsePayoutUTxOs cardanoSerializationlib (PayoutTxOutRef (TxOutRef { txIx, txId })) { datumJson, utxos } = do
  datum <- parseRoleTokenInfo datumJson
  utxo <- note (TypeMismatch "UTxO not found - invalid index") $ find (\(AnUTxO { txOutRef: TxOutRef txOutRef }) -> txOutRef.txIx == txIx) utxos
  pure $ PayoutUTxO
    { roleToken: datum
    , utxo
    }

parseRoleTokenInfo :: Json -> Either JsonDecodeError RoleTokenInfo
parseRoleTokenInfo json = do
  rootObj <- decodeJson json
  jsonValueObj <- rootObj .: "json_value" >>= decodeJson
  jsonValueObj .: "fields" >>= case _ of
    [ policyIdObj, assetNameObj ] -> do
      policyId <- do
        policyIdStr <- policyIdObj .: "bytes"
        note (TypeMismatch "Invalid policyId") $ C.policyIdFromHexString $ policyIdStr
      assetName <- do
        assetNameStr <- assetNameObj .: "bytes"
        note (TypeMismatch "Invalid assetName") $ C.assetNameFromHexString $ assetNameStr
      pure $ { policyId, assetName }
    _ -> Left $ TypeMismatch "Invalid RoleTokenInfo"

parsePayoutDatumHash :: PayoutTxOutRef -> Array AnUTxO -> Either JsonDecodeError DatumHash
parsePayoutDatumHash (PayoutTxOutRef (TxOutRef { txIx })) utxos = do
  AnUTxO { txOut: TxOut txOut } <- note (TypeMismatch "UTxO not found - invalid index") $
    find (\(AnUTxO { txOutRef: TxOutRef txOutRef }) -> txOutRef.txIx == txIx) utxos
  case txOut.datumHash of
    Just datumHash -> pure datumHash
    Nothing -> Left $ TypeMismatch "DatumHash not found"

selectRoleTokenTxOutRef :: CardanoMultiplatformLib.Lib -> RoleTokenInfo -> Wallet.Api -> Aff (Maybe AnUTxO)
selectRoleTokenTxOutRef cardanoMultiplatformLib { policyId, assetName } wallet = do
  possibleUTxOs <- Wallet.getUtxos wallet
  let
    utxos = fromRight [] (map fold possibleUTxOs)
    assetId = C.AssetId policyId assetName
  liftEffect $ runGarbageCollector cardanoMultiplatformLib do
    _TransactionUnspentOutput <- asksLib _."TransactionUnspentOutput"
    -- We just lack `UTxO` type in `Contrib.Cardano`.
    Array.head <<< Array.catMaybes <$> for utxos \(utxo :: CborHex TransactionUnspentOutputObject) -> do
      let
        utxo' = cborHexToCbor utxo
      unspentTxOutObj <- allocate $ transactionUnspentOutput.from_bytes _TransactionUnspentOutput utxo'
      txOutObj <- allocate $ transactionUnspentOutputObject.output unspentTxOutObj
      valueObj <- allocate $ transactionOutputObject.amount txOutObj
      value <- fromRight mempty <<< C.valueFromNestedMaps <$> valueMapFromValueObject valueObj
      if C.selectAsset value assetId > mempty then do
        Just <$> C.CML.transactionUnspentOutputToUTxO unspentTxOutObj
      else
        pure Nothing



