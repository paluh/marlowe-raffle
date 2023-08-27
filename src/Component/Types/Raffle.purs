module Component.Types.Raffle
  ( mkRaffleContracts
  , rolesConfigFromPlan
  , OnChainContractsThread(..)
  , OnChainContractThreadContinuation(..)
  , RaffleOrganizer(..)
  , RaffleDeploymentPlan(..)
  , PricePolicy(..)
  , RaffleOnChainStatus(..)
  , ContractStatus(..)
  , RaffleRoundContract(..)
  , PriceFundingContracts(..)
  , RaffleContracts(..)
  , initialDepositTagsFromPlan
  , unsafeDecodeAssetName
  ) where

import Prelude

import CardanoMultiplatformLib (Bech32, bech32ToString)
import Component.Types.AppTags (AppTags(..), RaffleRound(..), RaffleTags(..), RaffleId)
import Component.Types.ContractInfo (ContractInfo(..), MarloweInfo(..))
import Contrib.Cardano as C
import Contrib.Data.Array.NonEmpty (chunks) as NonEmptyArray
import Contrib.Web.SubtleCrypto as SubtleCrypto
import Control.Monad.Except (throwError)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (cons', fromArray, sort, toArray, uncons, zip) as NonEmptyArray
import Data.Either (Either(..), note)
import Data.Foldable (foldM, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (un)
import Data.String as String
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import HexString (utf8HexToString)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Types as Runtime
import Partial.Unsafe (unsafeCrashWith)

-- We use first deposit transaction which possibly mints all the tokens.

newtype RaffleOrganizer = RaffleOrganizer Bech32
derive instance Eq RaffleOrganizer
derive instance Ord RaffleOrganizer
derive newtype instance Show RaffleOrganizer

newtype RaffleDeploymentPlan = RaffleDeploymentPlan
  { organizer :: RaffleOrganizer
  , contractsTimeout :: V1.Timeout
  , participants :: NonEmptyArray Bech32
  --  Either
  --    (Sha2Hex Sha256)
  --    (NonEmptyArray Bech32)
  , priceTokensCurrency :: Maybe C.PolicyId
  , rounds :: NonEmptyArray RaffleRound
  }

sortContractInfosByStep :: Array ContractInfo -> Array ContractInfo
sortContractInfosByStep = do
  let
    raffleTags (ContractInfo { tags: AppTags { raffleTags }}) = raffleTags

    orderProjection (InitialPriceChunkDeposit {}) = 1 /\ 0
    orderProjection (PriceChunkDeposit { roundNo }) = 1 /\ roundNo
    orderProjection (Raffle { roundNo }) = 2 /\ roundNo
  Array.sortWith (orderProjection <<< raffleTags)

-- FIXME: Just a placeholder for now.
data OnChainContractsThread
  = OnChainPriceChunkDeposit
    { threadContinuation :: OnChainContractThreadContinuation }
  | OnChainRaffle
    { threadContinuation :: OnChainContractThreadContinuation }

data OnChainContractThreadContinuation
  = OngoingContract
  | ClosedWithoutSuccessor
  | ClosedWithSuccessor OnChainContractsThread


foldOnChainRaffle :: Array ContractInfo -> Either String RaffleOnChainStatus
foldOnChainRaffle contractInfos = do
  let
    contractInfos' = Array.reverse $ sortContractInfosByStep contractInfos
    isOngoing (ContractInfo { marloweInfo: possibleMarloweContract }) = do
       MarloweInfo { currentContract } <- note "No marlowe info found" possibleMarloweContract
       pure $ isNothing currentContract

    step :: _ -> _ -> Either String (Maybe (Either OnChainContractsThread RaffleOnChainStatus))
    step = case _, _ of
      nextStep, ci@(ContractInfo { contractId, tags: AppTags { raffleTags }, marloweInfo: possibleMarloweInfo}) -> do
        threadContinuation <- case nextStep of
          Just (Right _) -> throwError "Next step should not be folded on chain status"
          Just (Left next) -> isOngoing ci >>= if _
              then throwError "Ongoing contract found but next step is executed?"
              else pure $ ClosedWithSuccessor next
          Nothing -> do
            isOngoing ci <#> if _
              then OngoingContract
              else ClosedWithoutSuccessor

        MarloweInfo { currencySymbol: possibleCurrencySymbol } <- note "No marlowe info found" possibleMarloweInfo
        currencySymbol <- note "No currency symbol found" possibleCurrencySymbol
        pricePolicyId <- note "No policy id found" $ C.policyIdFromHexString currencySymbol

        let
          res = case raffleTags of
            InitialPriceChunkDeposit onChainInfo -> Right $ RaffleOnChainStatus
              { pricePolicyId: pricePolicyId
              , raffleId: contractId
              , minted: onChainInfo.minted
              , onChainContract: OnChainPriceChunkDeposit { threadContinuation }
              }
            PriceChunkDeposit {} -> Left $ OnChainPriceChunkDeposit { threadContinuation }
            Raffle {} -> Left $ OnChainRaffle { threadContinuation }
        pure $ Just $ res
  foldM step Nothing contractInfos' >>= case _ of
    Nothing -> throwError "No initial deposit found"
    Just (Left res) -> throwError "No initial deposit found"
    Just (Right res) -> pure res

-- raffleOnChainInfo (ContractInfo { contractId, tags, currentContract }) = do
--   case tags of
--     InitialPriceChunkDeposit raffleOnChainInfo -> do
--       pure (contractId /\ raffleOnChainInfo /\ isNothing currentContract)
--     _ -> Left "No initial deposit found"

-- raffleId /\ onChainInfo <- note "No initial deposit dound" $ findMap raffleOnChainInfo contractInfos
    

-- -- We asssume that provide contract infos contain the same raffle id.
-- foldOnChainRaffles :: Array ContractInfo -> Array RaffleOnChainStatus
-- foldOnChainRaffles contractInfos = do
--   let
--     raffleContractInfos = 
--       Map.fromFoldableWith (<>) $ contractInfo <#> \ci@(ContractInfo { contractId, tags }) -> case tags of
--         InitialPriceChunkDeposit {} -> contractId /\ ci
--         PriceChunkDeposit { raffleId } -> raffleId /\ ci
--         Raffle { raffleId } -> raffleId /\ ci
-- 


data RaffleOnChainStatus
  = RaffleOnChainStatus
    { pricePolicyId :: C.PolicyId
    , raffleId :: RaffleId
    , minted :: Boolean
    , onChainContract :: OnChainContractsThread
    }

data ContractStatus status result
  = OffChain
  | Ongoing status
  | Closed result
derive instance (Eq status, Eq result) => Eq (ContractStatus status result)

data RaffleRoundContract
  = AwaitsPreviousRoundContract
    { priceAssetName :: C.AssetName
    }
  | RaffleInitialRoundContract
    { contractStatus :: ContractStatus Unit Unit
    , initilContract :: V1.Contract
    , priceAssetName :: C.AssetName
    }
derive instance Eq RaffleRoundContract

newtype PriceFundingContracts = PriceFundingContracts
  { contractStatus :: ContractStatus Unit Unit
  , initialContracts :: NonEmptyArray (V1.Contract /\ C.NonAdaAssets)
  , priceAssetName :: C.AssetName
  }
derive instance Eq PriceFundingContracts

data PricePolicy
  = PremintedPricePolicy C.PolicyId
  | MintPricePolicy RaffleOrganizer
derive instance Eq PricePolicy

newtype RaffleContracts = RaffleContracts
  { pricePolicy :: PricePolicy
  , rounds :: NonEmptyArray (RaffleRoundContract /\ PriceFundingContracts)
  }
derive instance Eq RaffleContracts

maxDeposits :: Int
maxDeposits = 5


--  = RaffleOnChainStatus
--    { pricePolicyId :: C.PolicyId
--    , raffleId :: RaffleId
--    , minted :: Boolean
--    , onChainContract :: OnChainContractsThread
--    }

mkRaffleContracts :: RaffleDeploymentPlan -> Maybe RaffleOnChainStatus -> Either String RaffleContracts
mkRaffleContracts (RaffleDeploymentPlan plan) possibleStatus = do
  pricePolicy <- case possibleStatus, plan of
    Just (RaffleOnChainStatus { minted: true, pricePolicyId: policyId }), _ -> pure $ PremintedPricePolicy policyId
    Just _, { priceTokensCurrency: Just policyId } -> pure $ PremintedPricePolicy policyId
    Just _, { priceTokensCurrency: Nothing } -> throwError "Component.Types.raffleContratcs: Invalid initialization"
    Nothing, _ -> pure $ MintPricePolicy plan.organizer

    -- StartedWithMiting policyId _, _ -> pure $ PremintedPricePolicy policyId
    -- StartedWithoutMinting _, { priceTokensCurrency: Just policyId } -> pure $ PremintedPricePolicy policyId
    -- StartedWithoutMinting _, { priceTokensCurrency: Nothing } -> throwError "Component.Types.raffleContratcs: Invalid initialization"
    -- NotStarted, _ -> pure $ MintPricePolicy plan.organizer

  let
    -- FIXME: We should use information from deployment status and zip that into the round contract list.
    rounds = case NonEmptyArray.uncons plan.rounds of
      { head: RaffleRound head, tail } -> do
        let
          head' = RaffleInitialRoundContract
            { contractStatus: OffChain
            , initilContract: V1.Close
            , priceAssetName: head.priceAssetName
            }
        NonEmptyArray.cons' head' $ tail <#> \(RaffleRound round) ->
          AwaitsPreviousRoundContract { priceAssetName: round.priceAssetName }

  priceFunding <- for plan.rounds \(RaffleRound round) -> do
    let
      RaffleOrganizer organizerAddress = plan.organizer
      organizer = V1.Address $ bech32ToString organizerAddress

      -- FIXME: NonAdaAsset should preserve an invariant that it is not empty.
      arr = Map.toUnfoldable $ un C.NonAdaAssets round.tokenBundle

    nArr <- case NonEmptyArray.fromArray arr of
      Nothing -> throwError "Component.Types.mkRaffleContracts: Invalid token bundle"
      Just nArr -> pure nArr

    let
      tokenBundleChunks :: NonEmptyArray (NonEmptyArray ((C.PolicyId /\ C.AssetName) /\ C.Quantity))
      tokenBundleChunks = NonEmptyArray.chunks maxDeposits $ NonEmptyArray.sort $ nArr

      initialContracts = tokenBundleChunks <#> \tokenBundleChunk -> do
        let
          step ((policyId /\ assetName) /\ C.Quantity quantity) accContract = do
            let
              policyIdStr = C.policyIdToHexString policyId
              recipient = V1.Role $ unsafeDecodeAssetName round.priceAssetName
              tokenName = unsafeDecodeAssetName assetName
              deposit = V1.Deposit recipient organizer (V1.Token policyIdStr tokenName) (V1.Constant quantity)
            V1.When [ V1.Case deposit accContract ] plan.contractsTimeout V1.Close
          contract = foldr step V1.Close tokenBundleChunk
        contract /\ (C.NonAdaAssets $ Map.fromFoldable tokenBundleChunk)

    pure $ PriceFundingContracts
      { contractStatus: OffChain
      , initialContracts
      , priceAssetName: round.priceAssetName
      }
  pure $ RaffleContracts
    { rounds: NonEmptyArray.zip rounds priceFunding
    , pricePolicy
    }

initialDepositTagsFromPlan :: RaffleDeploymentPlan -> Aff (Maybe RaffleTags)
initialDepositTagsFromPlan (RaffleDeploymentPlan plan) = do
  possibleParticipantsFingerprint <- do
    let
      participantsStr = String.joinWith "," $ NonEmptyArray.toArray $ plan.participants <#> bech32ToString
    SubtleCrypto.digestString SubtleCrypto.sha256 participantsStr

  for possibleParticipantsFingerprint \participantsFingerprint -> pure $ do
    InitialPriceChunkDeposit
      { rounds: plan.rounds
      , minted: isNothing plan.priceTokensCurrency
      , participantsFingerprint
      }

rolesConfigFromPlan :: RaffleDeploymentPlan -> Runtime.RolesConfig
rolesConfigFromPlan (RaffleDeploymentPlan { organizer, priceTokensCurrency, rounds }) = case priceTokensCurrency of
  Just policyId -> Runtime.UsePolicy $ Runtime.PolicyId $ C.policyIdToHexString policyId
  Nothing -> Runtime.Mint do
    let
      roleNames = rounds <#> \(RaffleRound { priceAssetName: n }) -> n
      organizerAddr = case organizer of RaffleOrganizer addr -> addr
    Map.fromFoldable $ roleNames <#> \an -> do
      let
        name' = unsafeDecodeAssetName an
      (name' /\ Runtime.RoleTokenSimple organizerAddr)

unsafeDecodeAssetName :: C.AssetName -> String
unsafeDecodeAssetName (C.AssetName h) = case utf8HexToString h of
  Nothing -> unsafeCrashWith "Component.Types.rolesConfigFromPlan: Invalid encoding of asset name?"
  Just name -> name
