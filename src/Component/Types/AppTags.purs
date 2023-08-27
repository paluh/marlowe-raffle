module Component.Types.AppTags where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson, stringify)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import JS.Unsafe.Stringify (unsafeStringify)
import Marlowe.Runtime.Web.Types as Runtime
import Contrib.Cardano as C
import Contrib.Web.SubtleCrypto (Sha256, Sha2Hex)
import Data.Array.NonEmpty (NonEmptyArray)

type PriceAssetName = String

type RaffleId = Runtime.ContractId

newtype RaffleRound = RaffleRound
  { priceAssetName :: C.AssetName
  , tokenBundle :: C.NonAdaAssets
  }
derive instance Eq RaffleRound

instance EncodeJson RaffleRound where
  encodeJson (RaffleRound {priceAssetName, tokenBundle}) =
    encodeJson
      [ encodeJson priceAssetName
      , encodeJson tokenBundle
      ]
instance DecodeJson RaffleRound where
  decodeJson json = decodeJson json >>= case _ of
    [priceAssetNameJson, tokenBundleJson] -> do
      priceAssetName <- decodeJson priceAssetNameJson
      tokenBundle <- decodeJson tokenBundleJson
      pure $ RaffleRound { priceAssetName, tokenBundle }
    _ -> Left $ TypeMismatch $ "RaffleRound decoding failed for json: " <> stringify json

type RaffleOnChainInfo =
  { rounds :: NonEmptyArray RaffleRound
  , minted :: Boolean
  , participantsFingerprint :: Sha2Hex Sha256
  }


data RaffleTags
  = InitialPriceChunkDeposit RaffleOnChainInfo
  | PriceChunkDeposit
    { roundNo :: Int
    , priceAssetName :: PriceAssetName
    , raffleId :: RaffleId
    }
  | Raffle
    { raffleId :: RaffleId
    , roundNo :: Int
    }

instance EncodeJson RaffleTags where
  encodeJson = case _ of
    PriceChunkDeposit {roundNo, priceAssetName, raffleId} ->
      encodeJson
        [ encodeJson "deposit"
        , encodeJson
          [ encodeJson roundNo
          , encodeJson priceAssetName
          , encodeJson raffleId
          ]
        ]
    InitialPriceChunkDeposit { rounds, minted, participantsFingerprint } -> do
      let
        minted' = if minted
          then 1
          else 0
      encodeJson
        [ encodeJson "init"
        , encodeJson
          [ encodeJson rounds
          , encodeJson minted'
          , encodeJson participantsFingerprint
          ]
        ]
    Raffle { raffleId, roundNo } ->
      encodeJson
        [ encodeJson "raffle"
        , encodeJson
          [ encodeJson raffleId
          , encodeJson roundNo
          ]
        ]

instance DecodeJson RaffleTags where
  decodeJson json = decodeJson json >>= case _ of
    [tagJson, payloadJson] -> do
      tag <- decodeJson tagJson
      payload <- decodeJson payloadJson
      case tag, payload of
        "deposit", [ roundJson, priceAssetNameJson, raffleIdJson ] ->
          PriceChunkDeposit <$> do
            roundNo <- decodeJson roundJson
            priceAssetName <- decodeJson priceAssetNameJson
            raffleId <- decodeJson raffleIdJson
            pure { roundNo, priceAssetName, raffleId }
        "init", [ roundsJson, mintedJson, participantsFingerprintJson ] ->
          InitialPriceChunkDeposit <$> do
            rounds <- decodeJson roundsJson
            mintedInt <- decodeJson mintedJson
            minted <- case mintedInt of
              0 -> pure false
              1 -> pure true
              _ -> Left $ TypeMismatch "Invalid minted value"
            participantsFingerprint <- decodeJson participantsFingerprintJson
            pure { rounds, minted, participantsFingerprint }
        "raffle", [ raffleIdJson, roundJson ] ->
          Raffle <$> do
            raffleId <- decodeJson raffleIdJson
            roundNo <- decodeJson roundJson
            pure { raffleId, roundNo }
        _, _ -> Left $ TypeMismatch "Invalid raffle JSON"
    _ -> Left $ TypeMismatch "Invalid raffle JSON"

newtype ExtraTags = ExtraTags (Array String)
derive instance Newtype ExtraTags _

-- Remove empty string
-- TODO: validate string length.
mkExtraTags :: Array String -> ExtraTags
mkExtraTags = ExtraTags <<< Array.filter (_ /= "")

newtype AppTags = AppTags { extraTags :: ExtraTags, raffleTags :: RaffleTags }
derive instance Newtype AppTags _

instance EncodeJson AppTags where
  encodeJson (AppTags { extraTags: ExtraTags extraTags, raffleTags }) = encodeJson
    [ encodeJson extraTags
    , encodeJson raffleTags
    ]

instance DecodeJson AppTags where
  decodeJson json = decodeJson json >>= case _ of
    [extraTagsJson, raffleTagsJson] -> AppTags <$> do
        extraTags <- decodeJson extraTagsJson
        raffleTags <- decodeJson raffleTagsJson
        pure { extraTags: mkExtraTags extraTags, raffleTags }
    _ -> Left $ TypeMismatch $ "Invalid AppTags:" <> unsafeStringify json

raffleAppTag :: String
raffleAppTag = "marffaello-v1"

fromRuntimeTags :: Runtime.Tags -> Maybe AppTags
fromRuntimeTags (Runtime.Tags tagsMap) = do
  tagsJson <- raffleAppTag `Map.lookup` tagsMap
  hush $ decodeJson tagsJson

toRuntimeTags :: AppTags -> Runtime.Tags
toRuntimeTags appTags = Runtime.Tags $ Map.singleton raffleAppTag $ encodeJson appTags

