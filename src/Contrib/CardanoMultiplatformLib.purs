module CardanoMultiplatformLib
  ( askLib
  , asksLib
  , importLib
  , module Exports
  , GarbageCollector
  , allocate
  , allocateOpt
  , bech32FromCbor
  , bech32FromCborHex
  , bech32FromString
  , plutusVasilCostModels
  , pubKeyHashFromBech32
  , runGarbageCollector
  , toCoinCbor
  , scriptHashFromBech32
  , transactionWitnessSetFromBytes
  , transactionFromCbor
  , transactionBodyFromCbor
  , valueFromCbor
  , valueMapFromValueObject
  ) where

import Prelude

import CardanoMultiplatformLib.Address (Address, AddressObject, addressObject, address) as Exports
import CardanoMultiplatformLib.Address (AddressObject, addressObject, stakeCredentialObject)
import CardanoMultiplatformLib.Address as Address
import CardanoMultiplatformLib.CostModel (CostModelObject(..), Costmdls(..), CostmdlsObject(..), costModel, costModelObject, costmdls, costmdlsObject)
import CardanoMultiplatformLib.CostModel as Exports
import CardanoMultiplatformLib.Ed25519KeyHash (Ed25519KeyHashObject(..), ed25519KeyHashObject)
import CardanoMultiplatformLib.Lib (Lib)
import CardanoMultiplatformLib.Lib (Lib) as Exports
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (BigNumObject(..), TransactionBodyObject, TransactionObject, TransactionWitnessSetObject, ValueObject, assetNameObject, assetNamesObject, assetsObject, bigNum, bigNumObject, multiAssetObject, value, valueObject)
import CardanoMultiplatformLib.Transaction as Transaction
import CardanoMultiplatformLib.Types (Bech32, Cbor, CborHex, bech32ToString, cborHexToCbor, cborHexToHex, cborToCborHex, unsafeBech32)
import CardanoMultiplatformLib.Types (CborHex(..), Bech32, cborToCborHex, cborHexToHex, bech32ToString) as Exports
import Contrib.CardanoMultiplatformLib.ScriptHash (ScriptHashObject(..), scriptHashObject, scriptHashesObject)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (encodeJson, stringify)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt.Argonaut
import Data.Foldable (sequence_)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (Opt, toMaybe)
import Data.Undefined.NoProblem as NoProblem
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Effect.Exception
import Effect.Ref (Ref)
import Effect.Ref as Ref
import HexString (hexToString)
import HexString as HexString
import JS.Object (EffectMth0, JSObject, runEffectMth0)
import Partial.Unsafe (unsafeCrashWith)
import Promise.Aff (Promise, toAff)
import Type.Prelude (Proxy(..))
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel (utf8)

-- TODO: Move to Lib module
foreign import importLibImpl :: Effect (Nullable (Promise Lib))

importLib :: Aff (Maybe Lib)
importLib = liftEffect importLibImpl >>= Nullable.toMaybe >>> case _ of
  Nothing -> pure Nothing
  Just promise ->
    (Just <$> toAff promise) `catchError` const (pure Nothing)

type Ctx = { lib :: Lib, frees :: Ref (List (Effect Unit)) }

-- | StateT is not sufficient because it is not exception
-- | safe. We need to use `Ref` to store the release actions.
-- |
-- | FIXME: We should probably introduce a scope phantom
-- | to avoid leaks.
newtype GarbageCollector a = GarbageCollector
  (ReaderT Ctx Effect a)

derive newtype instance Functor GarbageCollector
derive newtype instance Apply GarbageCollector
derive newtype instance Applicative GarbageCollector
derive newtype instance Bind GarbageCollector
derive newtype instance Monad GarbageCollector
derive newtype instance MonadEffect GarbageCollector

-- This is not complately safe API.
-- DON'T RETURN multiplatform values because they are deallocated!
runGarbageCollector :: forall a. Lib -> GarbageCollector a -> Effect a
runGarbageCollector lib (GarbageCollector action) = do
  freesRef <- Ref.new List.Nil
  let
    release = do
      frees <- Ref.read freesRef
      sequence_ frees
    run = do
      a <- runReaderT action { frees: freesRef, lib }
      release
      pure a
  run `catchError` \err -> do
    release
    throwError err

-- | The API allocates objects which provide `free` method.
-- | We use it to release the resources.
type UnmanagedObject r = JSObject (free :: EffectMth0 Unit | r)

allocate :: forall r t. Newtype t (UnmanagedObject r) => Effect t -> GarbageCollector t
allocate alloc = GarbageCollector do
  freesRef <- asks _.frees
  obj <- liftEffect alloc
  let
    jsobj = unwrap obj
    _free = Proxy :: Proxy "free"
  liftEffect $ Ref.modify_ (List.Cons (runEffectMth0 _free jsobj)) freesRef
  pure obj

allocateOpt :: forall r t. Newtype t (UnmanagedObject r) => Effect (Opt t) -> GarbageCollector (Opt t)
allocateOpt alloc = GarbageCollector do
  freesRef <- asks _.frees
  possibleObj <- liftEffect alloc
  case toMaybe possibleObj of
    Just obj -> do
      let
        jsobj = unwrap obj
        _free = Proxy :: Proxy "free"
      liftEffect $ Ref.modify_ (List.Cons (runEffectMth0 _free jsobj)) freesRef
    Nothing -> pure unit
  pure possibleObj

askLib :: GarbageCollector Lib
askLib = GarbageCollector do
  asks _.lib

asksLib :: forall a. (Lib.Props -> a) -> GarbageCollector a
asksLib f = askLib <#> (f <<< Lib.props)

transactionWitnessSetFromBytes :: Cbor TransactionWitnessSetObject -> GarbageCollector TransactionWitnessSetObject
transactionWitnessSetFromBytes twCbor = do
  { "TransactionWitnessSet": tws } <- GarbageCollector $ asks (Lib.props <<< _.lib)
  allocate $ Transaction.transactionWitnessSet.from_bytes tws twCbor

type ValueMap = Map String (Map String BigInt.Argonaut.BigInt)

bigNumObjToBigInt :: BigNumObject -> GarbageCollector BigInt.Argonaut.BigInt
bigNumObjToBigInt bigNumObj = do
    numStr <- liftEffect $ bigNumObject.to_str bigNumObj
    case BigInt.Argonaut.fromString numStr of
      Nothing -> liftEffect $ throwError $ Effect.Exception.error $ "CardanoMultiplatformLib.valueFromCbor: Failed to parse BigInt: " <> numStr
      Just num -> pure num

valueMapFromValueObject :: ValueObject -> GarbageCollector ValueMap
valueMapFromValueObject valObj = do
  textDecoder <- liftEffect $ TextDecoder.new utf8
  possibleMultiAssetObj <- allocateOpt $ valueObject.multiasset valObj
  coinObj <- allocate $ valueObject.coin valObj
  lovelace <- bigNumObjToBigInt coinObj
  let
    lovelaceOnly = Map.singleton "" (Map.singleton "" lovelace)
  Map.unionWith (Map.unionWith (+)) lovelaceOnly <$> case toMaybe possibleMultiAssetObj of
    Nothing -> pure Map.empty
    Just multiAssetObj -> do
      scriptHashesObj <- allocate $ multiAssetObject.keys multiAssetObj
      len <- liftEffect $ scriptHashesObject.len scriptHashesObj
      Map.fromFoldable <$> forWithIndex (Array.replicate len unit) \idx _ -> do
        scriptHashObj <- allocate $ scriptHashesObject.get scriptHashesObj idx
        hex <- liftEffect $ scriptHashObject.to_hex scriptHashObj
        let
          policyId = (hexToString <<< cborHexToHex $ hex)
        possibleAssets <- allocateOpt $ multiAssetObject.get multiAssetObj scriptHashObj
        case toMaybe possibleAssets of
          Nothing -> pure $ policyId /\ Map.empty
          Just assetsObj -> do
            assetNamesObj <- allocate $ assetsObject.keys assetsObj
            assetNamesLen <- liftEffect $ assetNamesObject.len assetNamesObj
            ((policyId /\ _) <<< Map.fromFoldable) <$> forWithIndex (Array.replicate assetNamesLen unit) \idx' _ -> do
              assetNameObj <- allocate $ assetNamesObject.get assetNamesObj idx'

              nameUint8Array <- liftEffect $ assetNameObject.name assetNameObj

              -- assetName <- liftEffect $ TextDecoder.decode nameUint8Array textDecoder
              let
                assetName = hexToString (HexString.encode nameUint8Array)
              bigNumObj <- allocate $ multiAssetObject.get_asset multiAssetObj scriptHashObj assetNameObj
              numStr <- liftEffect $ bigNumObject.to_str bigNumObj
              case BigInt.Argonaut.fromString numStr of
                Nothing -> liftEffect $ throwError $ Effect.Exception.error $ "CardanoMultiplatformLib.valueFromCbor: Failed to parse BigInt: " <> numStr
                Just num -> pure $ assetName /\ num

toCoinCbor :: BigInt.Argonaut.BigInt -> GarbageCollector (Cbor BigNumObject)
toCoinCbor num = do
  { "BigNum": bigNumClass } <- GarbageCollector $ asks (Lib.props <<< _.lib)
  bigNum <- allocate $ bigNum.from_str bigNumClass (BigInt.Argonaut.toString num)
  liftEffect $ bigNumObject.to_bytes bigNum

valueFromCbor :: Cbor ValueObject -> GarbageCollector ValueMap
valueFromCbor cbor = do
  { "Value": valueClass } <- GarbageCollector $ asks (Lib.props <<< _.lib)
  valObj <- allocate $ value.from_bytes valueClass cbor
  valueMapFromValueObject valObj

bech32FromCbor :: Cbor AddressObject -> Opt String -> GarbageCollector Bech32
bech32FromCbor cbor prefix = do
  { "Address": addrClass } <- GarbageCollector $ asks (Lib.props <<< _.lib)
  addrObject <- allocate $ Address.address.from_bytes addrClass cbor
  liftEffect $ addressObject.to_bech32 addrObject prefix

transactionBodyFromCbor :: Cbor TransactionBodyObject -> GarbageCollector TransactionBodyObject
transactionBodyFromCbor cbor = do
  { "TransactionBody": txBodyClass } <- GarbageCollector $ asks (Lib.props <<< _.lib)
  allocate $ Transaction.transactionBody.from_bytes txBodyClass cbor

transactionFromCbor :: Cbor TransactionObject -> GarbageCollector TransactionObject
transactionFromCbor cbor = do
  txClass <- asksLib _."Transaction"
  allocate $ Transaction.transaction.from_bytes txClass cbor

bech32FromCborHex :: CborHex AddressObject -> Opt String -> GarbageCollector Bech32
bech32FromCborHex cborHex prefix = do
  let
    cbor = cborHexToCbor cborHex
  bech32FromCbor cbor prefix

bech32FromString :: Lib -> String -> Effect (Maybe Bech32)
bech32FromString lib addrStr = do
  let
    { "Address": addressClass } = Lib.props lib
  Address.address.is_valid_bech32 addressClass addrStr >>=
    if _ then
      pure $ Just $ unsafeBech32 addrStr
    else
      pure Nothing

-- When you construct data script hash you should use only the required cost model.
plutusVasilCostModels :: GarbageCollector
  { pv1AndPv2 :: CostmdlsObject
  , pv1 :: CostmdlsObject
  , pv2 :: CostmdlsObject
  }
plutusVasilCostModels = do
  let
    v1CostModel =
            [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4
            , 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100
            , 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525
            , 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62
            , 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32
            , 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924
            , 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32
            , 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500
            , 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 806990, 30482, 4, 1927926
            , 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32
            , 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 57996947, 18975, 10
          ]
    plutusV1 =
      { language: "PlutusV1"
      , op_costs: map show v1CostModel
      }
    v2CostModel =
          [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4,
            23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100,
            23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525,
            14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000 , 216773, 62,
            1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32,
            1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924,
            473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32,
            76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500,
            453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990,
            30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182,
            32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35892428, 10,
            57996947, 18975, 10, 38887044, 32947, 10
          ]

    plutusV2 =
      { language: "PlutusV2"
      , op_costs: map show v2CostModel
      }
  costModelsClass <- asksLib _."Costmdls"
  pv1 <- allocate $ costmdls.from_json costModelsClass $ stringify $ encodeJson
      { "PlutusV1": plutusV2 }
  pv2 <- allocate $ costmdls.from_json costModelsClass $ stringify $ encodeJson
      { "PlutusV2": plutusV2 }
  pv1AndPv2 <- allocate $ costmdls.from_json costModelsClass $ stringify $ encodeJson
      { "PlutusV1": plutusV1
      , "PlutusV2": plutusV2
      }
  pure { pv1, pv2, pv1AndPv2 }

pubKeyHashFromBech32 :: Lib -> Bech32 -> Effect (Maybe (CborHex Ed25519KeyHashObject))
pubKeyHashFromBech32 lib bech32 = runGarbageCollector lib do
  let
    bech32str = bech32ToString bech32
  addressClass <- asksLib _."Address"
  addrObj <- allocate $ Address.address.from_bech32 addressClass bech32str
  possibleStakeCredObj <- allocateOpt $ Address.addressObject.payment_cred addrObj
  join <$> for (NoProblem.toMaybe possibleStakeCredObj) \stakeCredObj -> do
    h <- allocateOpt $ stakeCredentialObject.to_keyhash stakeCredObj
    for (NoProblem.toMaybe h) \h' ->
      liftEffect $ ed25519KeyHashObject.to_hex h'

scriptHashFromBech32 :: Lib -> Bech32 -> Effect (Maybe (CborHex ScriptHashObject))
scriptHashFromBech32 lib bech32 = runGarbageCollector lib do
  let
    bech32str = bech32ToString bech32
  addressClass <- asksLib _."Address"
  addrObj <- allocate $ Address.address.from_bech32 addressClass bech32str
  possibleStakeCredObj <- allocateOpt $ Address.addressObject.payment_cred addrObj
  join <$> for (NoProblem.toMaybe possibleStakeCredObj) \stakeCredObj -> do
    h <- allocateOpt $ stakeCredentialObject.to_scripthash stakeCredObj
    for (NoProblem.toMaybe h) \h' ->
      liftEffect $ scriptHashObject.to_hex h'
