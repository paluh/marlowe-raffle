module Component.UseWithdrawal.Blockfrost where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex, bech32FromString, cborHexToHex)
import CardanoMultiplatformLib (Lib, bech32ToString) as CML
import CardanoMultiplatformLib.Transaction (TransactionObject) as CML
import CardanoMultiplatformLib.Types (cborHexToCbor, cborToUint8Array)
import Contrib.Argonaut.Checked (JsonDecodeErrorR, decodeJsonV, toChecked)
import Contrib.Argonaut.Checked as AC
import Contrib.Cardano as C
import Contrib.CardanoMultiplatformLib.ScriptHash (ScriptHashObject) as CML
import Contrib.Data.EitherV (EitherV)
import Contrib.Fetch (FetchError, FetchErrorR, fetchEither, fetchV)
import Control.Monad.Except (except)
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, (.:), (.:?))
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either, hush, note)
import Data.Foldable (foldr)
import Data.FormURLEncoded.Query as Q
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Traversable (for)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Unsafe (unsafePerformEffect)
import Fetch (Method(..))
import Foreign (Foreign)
import HexString (hexToString)
import Marlowe.Runtime.Web.Types (AnUTxO(..), DatumHash(..), TxId(..), TxOut(..), TxOutRef(..))
import Partial.Unsafe (unsafeCrashWith)
import React.Basic.Hooks ((/\))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Network :: Type

instance Eq Network where
  eq n1 n2 = toStr n1 == toStr n2
    where
    toStr :: Network -> String
    toStr = unsafeCoerce

preprod :: Network
preprod = unsafeCoerce "preprod"

preview :: Network
preview = unsafeCoerce "preview"

mainnet :: Network
mainnet = unsafeCoerce "mainnet"

apiURL :: Network -> String
apiURL network
  | network == preprod = "https://cardano-preprod.blockfrost.io/api/v0"
  | network == preview = "https://cardano-testnet.blockfrost.io/api/v0"
  | network == mainnet = "https://cardano-mainnet.blockfrost.io/api/v0"
  | true = unsafeCrashWith "Blockfrost.apiURL: Unknown network"

newtype ProjectId = ProjectId String

derive instance Eq ProjectId
derive instance Ord ProjectId
derive instance Newtype ProjectId _
derive newtype instance DecodeJson ProjectId
derive newtype instance EncodeJson ProjectId

allowedStatusCodes :: Array Int
allowedStatusCodes = [ 200, 201, 202, 206 ] -- , 400, 401, 403, 404, 500 ]

-- https://cardano-preprod.blockfrost.io/api/v0/scripts/datum/e92e4793a070c15b8327ab17
fetchDatum :: ProjectId -> Network -> DatumHash -> Aff (Maybe Json)
fetchDatum (ProjectId projectId) network (DatumHash datumHash) = do
  let
    url = apiURL network <> "/scripts/datum/" <> datumHash
    headers = { project_id: projectId }

  res <- hush <$> fetchEither url { headers } allowedStatusCodes identity
  for res \{ json: readForeign } -> do
    readForeign <#> (unsafeCoerce :: Foreign -> Json)

parseBlockfrostUnit :: String -> Maybe C.AssetId
parseBlockfrostUnit "lovelace" = pure C.AdaAssetId
parseBlockfrostUnit combinedHex = do
  let
    { before: policyIdStr, after: assetNameStr } = String.splitAt 56 combinedHex
  policyId <- C.policyIdFromHexString policyIdStr
  assetName <- C.assetNameFromHexString assetNameStr
  Just $ C.AssetId policyId assetName

encodeBlockfrostUnit :: C.AssetId -> String
encodeBlockfrostUnit C.AdaAssetId = "lovelace"
encodeBlockfrostUnit (C.AssetId policyId assetName) = C.policyIdToHexString policyId <> C.assetNameToHexString assetName

type BlockfrostAmount = Array { unit :: String, quantity :: String }

parseBlockfrostAmount :: BlockfrostAmount -> Maybe C.Value
parseBlockfrostAmount amount = map C.valueFromFoldable $ for amount \{ unit, quantity } -> do
  assetId <- parseBlockfrostUnit unit
  v <- BigInt.fromString quantity
  pure $ assetId /\ C.Quantity v

-- In some contexts we get utxo with a tx_hash field, in others we don't.
parseBlockfrostUTxO :: CML.Lib -> (Maybe TxId) -> Json -> Either JsonDecodeError AnUTxO
parseBlockfrostUTxO cardanoSerializationlib possibleTxId json = do
  obj <- decodeJson json
  addressStr <- obj .: "address"
  address <- note (TypeMismatch "Invalid Bech32") $ unsafePerformEffect $ bech32FromString cardanoSerializationlib addressStr
  amount <- obj .: "amount"
  value <- note (TypeMismatch "Invalid amount value") $ parseBlockfrostAmount amount
  datumHash <- (obj .:? "data_hash") <#> map DatumHash
  txIx <- obj .: "output_index"
  txId <- case possibleTxId of
    Just txId -> pure txId
    Nothing -> TxId <$> obj .: "tx_hash"
  let
    txOutRef = TxOutRef { txId, txIx }
    txOut = TxOut { address, value, datumHash }
  pure $ AnUTxO { txOutRef, txOut }

data TODO = TODO

newtype PageNumber = PageNumber Int

foldPages :: forall a i r. (PageNumber -> ExceptV r Aff (Array i)) -> (a -> i -> FoldResult a) -> a -> ExceptV r Aff a
foldPages fetchPage f a = do
  let
    go accum currPageNumber = do
      page <- fetchPage (PageNumber currPageNumber)
      if Array.null page then pure accum
      else do
        let
          step item pageAccum = do
            case pageAccum of
              StopFolding _ -> pageAccum
              Continue accum' -> f accum' item
          res = foldr step (Continue accum) page
        case res of
          StopFolding accum' -> pure accum'
          Continue accum' -> go accum' (currPageNumber + 1)
  go a 1

fetchTxUTxOs
  :: forall r
   . CML.Lib
  -> ProjectId
  -> Network
  -> TxId
  -- -> Aff (Maybe { inputs :: TxOutputs, outputs :: TxOutputs })
  -> ExceptV (AssetHistoryErrorR + r) Aff { inputs :: Array AnUTxO, outputs :: Array AnUTxO }
fetchTxUTxOs cml (ProjectId projectId) network txId@(TxId txIdStr) = do
  let
    url = apiURL network <> "/txs/" <> txIdStr <> "/utxos"
    headers = { project_id: projectId }

  { json: affForeign } <- fetchV url { headers } allowedStatusCodes
  json <- liftAff $ affForeign <#> (unsafeCoerce :: Foreign -> Json)

  let
    parseUTxOs :: String -> Maybe TxId -> EitherV (AssetHistoryErrorR + r) (Array AnUTxO)
    parseUTxOs prop possibleTxId = toChecked do
      obj <- decodeJson json
      outputs <- obj .: prop
      for outputs $ parseBlockfrostUTxO cml possibleTxId

  except do
    outputs <- parseUTxOs "outputs" (Just txId)
    inputs <- parseUTxOs "inputs" Nothing
    pure { inputs, outputs }

-- /assets/{asset}/history
-- {
-- "tx_hash": "9c190bc1ac88b2ab0c05a82d7de8b71b67a9316377e865748a89d4426c0d3005",
-- "amount": "5",
-- "action": "burned"
-- },

-- /assets/{asset}/txs
-- [
-- "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b",
-- "52e748c4dec58b687b90b0b40d383b9fe1f24c1a833b7395cdf07dd67859f46f",
-- "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b"
-- ]
-- data NonAdaAssetId = NonAdaAssetId PolicyId AssetName

type AssetHistoryErrorR r = (FetchErrorR + JsonDecodeErrorR + r)

fetchAssetTxs
  :: forall r
   . ProjectId
  -> Network
  -> C.NonAdaAssetId
  -> PageNumber
  -> ExceptV (AssetHistoryErrorR + r) Aff (Array TxId)
fetchAssetTxs (ProjectId projectId) network (C.NonAdaAssetId policyId assetName) (PageNumber pageNumber) = do
  let
    query = Q.unsafeEncode $ Q.fromHomogeneous $
      { page: [ show pageNumber ] }
    url = apiURL network <> "/assets/" <> encodeBlockfrostUnit (C.AssetId policyId assetName) <> "/txs?" <> query
    headers = { project_id: projectId }
  { json: readForeign } <- fetchV url { headers } allowedStatusCodes
  json <- liftAff $ readForeign <#> (unsafeCoerce :: Foreign -> Json)
  except $ decodeJsonV json

foldAssetTxs
  :: forall a r
   . ProjectId
  -> Network
  -> C.NonAdaAssetId
  -> (a -> TxId -> FoldResult a)
  -> a
  -> ExceptV (AssetHistoryErrorR + r) Aff a
foldAssetTxs projectId network assetId f a = do
  let
    fetchPage = fetchAssetTxs projectId network assetId
  foldPages fetchPage f a

submitTx :: ProjectId -> Network -> CborHex CML.TransactionObject -> Aff (Either FetchError Json)
submitTx (ProjectId projectId) network cborHex = do
  let
    url = apiURL network <> "/tx/submit"
    headers = { project_id: projectId, "Content-Type": "application/cbor" }
    method = POST
    body = cborToUint8Array $ cborHexToCbor cborHex
  res <- fetchEither url { headers, body, method } allowedStatusCodes identity
  for res \{ json: readForeign } -> do
    readForeign <#> (unsafeCoerce :: Foreign -> Json)

-- This endpoint is restricted to "hosted variant"
evaluateTx :: ProjectId -> Network -> CborHex CML.TransactionObject -> Aff (Either FetchError Json)
evaluateTx (ProjectId projectId) network cborHex = do
  let
    url = apiURL network <> "/utils/txs/evaluate"
    headers = { project_id: projectId, "Content-Type": "application/cbor" }
    method = POST
    body = cborToUint8Array $ cborHexToCbor cborHex
  res <- fetchEither url { headers, body, method } allowedStatusCodes identity
  for res \{ json: readForeign } -> do
    readForeign <#> (unsafeCoerce :: Foreign -> Json)

-- /addresses/{address}/utxos
{- 
    [
    ￼{
    "address": "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz",
    "tx_hash": "39a7a284c2a0948189dc45dec670211cd4d72f7b66c5726c08d9b3df11e44d58",
    "output_index": 0,
    "amount": ￼[],
    "block": "7eb8e27d18686c7db9a18f8bbcfe34e3fed6e047afaa2d969904d15e934847e6",
    "data_hash": "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710",
    "inline_datum": null,
    "reference_script_hash": null
    },
    ..
    ]
-}

type AddressUTxOsErrorR r = (FetchErrorR + JsonDecodeErrorR + r)

fetchAddressUTxOs :: forall r. CML.Lib -> ProjectId -> Network -> Bech32 -> PageNumber -> ExceptV (AddressUTxOsErrorR + r) Aff (Array AnUTxO)
fetchAddressUTxOs cml (ProjectId projectId) network address (PageNumber pageNumber) = do
  let
    query = Q.unsafeEncode $ Q.fromHomogeneous $
      { page: [ show pageNumber ] }

    url = apiURL network <> "/addresses/" <> CML.bech32ToString address <> "/utxos" <> "?" <> query
    headers = { project_id: projectId }
  { json: readForeign } <- fetchV url { headers } allowedStatusCodes
  json <- liftAff $ readForeign <#> (unsafeCoerce :: Foreign -> Json)
  except $ AC.toChecked do
    utxos <- decodeJson json
    for utxos $ parseBlockfrostUTxO cml Nothing

-- Allows short circuiting the fold. Fold will stop if the result is `Finished`.
data FoldResult a = Continue a | StopFolding a

foldAddressUTxOs :: forall a r. CML.Lib -> ProjectId -> Network -> Bech32 -> (a -> AnUTxO -> FoldResult a) -> a -> ExceptV (AddressUTxOsErrorR + r) Aff a
foldAddressUTxOs cml projectId network address f a = do
  let
    fetchPage = fetchAddressUTxOs cml projectId network address
  foldPages fetchPage f a

-- /scripts/{script_hash}/redeemers
--  [
--  ￼{
--  "tx_hash": "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0",
--  "tx_index": 0,
--  "purpose": "spend",
--  "redeemer_data_hash": "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
--  "datum_hash": "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
--  "unit_mem": "1700",
--  "unit_steps": "476468",
--  "fee": "172033"
--  },
--  ...
--  ]

-- Let's ignore some fields for now
type BlockfrostScriptRedeemer =
  { tx_hash :: String
  , tx_index :: Int
  -- , purpose :: String
  -- , redeemer_data_hash :: String
  -- , datum_hash :: String
  -- , unit_mem :: String
  -- , unit_steps :: String
  -- , fee :: String
  }

data Purpose = Mint | Spend

newtype ExUnits = ExUnits
  { mem :: Int
  , steps :: Int
  }

newtype ScriptRedeemer = ScriptRedeemer
  { txOutRef :: TxOutRef
  -- , purpose :: Purpose
  -- , redeemerDataHash :: String
  -- , datumHash :: String
  -- , exUnits :: ExUnits
  -- , fee :: C.Lovelace
  }

parseBlockfrostScriptRedeemer :: forall r. BlockfrostScriptRedeemer -> ExceptV r Aff ScriptRedeemer
parseBlockfrostScriptRedeemer { tx_hash, tx_index } = do
  let
    txOutRef = TxOutRef { txId: TxId tx_hash, txIx: tx_index }
  pure $ ScriptRedeemer { txOutRef }

-- Let's take page as usual

-- pubKeyHashFromBech32 :: Lib -> Bech32 -> Effect (Maybe (CborHex Ed25519KeyHashObject))

fetchScriptRedeemers
  :: forall r
   . ProjectId
  -> Network
  -> (CborHex CML.ScriptHashObject)
  -> PageNumber
  -> ExceptV (AddressUTxOsErrorR + r) Aff (Array ScriptRedeemer)
fetchScriptRedeemers (ProjectId projectId) network scriptHash (PageNumber pageNumber) = do
  let
    query = Q.unsafeEncode $ Q.fromHomogeneous $
      { page: [ show pageNumber ] }
    hashStr = hexToString <<< cborHexToHex $ scriptHash

    url = apiURL network <> "/scripts/" <> hashStr <> "/redeemers" <> "?" <> query
    headers = { project_id: projectId }
  { json: readForeign } <- fetchV url { headers } allowedStatusCodes
  json <- liftAff $ readForeign <#> (unsafeCoerce :: Foreign -> Json)
  redeemers <- except $ decodeJsonV json
  for redeemers parseBlockfrostScriptRedeemer

foldScriptRedeemers
  :: forall a r
   . ProjectId
  -> Network
  -> (CborHex CML.ScriptHashObject)
  -> (a -> ScriptRedeemer -> FoldResult a)
  -> a
  -> ExceptV (AddressUTxOsErrorR + r) Aff a
foldScriptRedeemers projectId network scriptHash f a = do
  let
    fetchPage pageNumber = fetchScriptRedeemers projectId network scriptHash pageNumber
  foldPages fetchPage f a

-- TODO: Expose something similar to: https://github.com/Plutonomicon/cardano-transaction-lib/blob/174324f5b52c6a08631bfbd6877b9d79647d1698/src/Internal/Service/Blockfrost.purs
-- data BlockfrostEndpoint = ScriptRedeemer
