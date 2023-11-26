module Main where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.App (mkApp)
import Component.MessageHub (mkMessageHub)
import Component.Types (Slotting(..))
import Component.Types.AppTags (raffleAppTag)
import Contrib.Data.Argonaut (JsonParser)
import Contrib.Effect as Effect
import Contrib.JsonBigInt as JsonBigInt
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.BigInt.Argonaut as BigInt
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Streaming (PollingInterval(..), RequestInterval(..))
import Marlowe.Runtime.Web.Streaming as Streaming
import Marlowe.Runtime.Web.Types (ServerURL(..))
import Partial.Unsafe (unsafePartial)
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Config =
  { marloweWebServerUrl :: ServerURL
  , develMode :: Boolean
  , network :: String
  }

decodeConfig :: JsonParser Config
decodeConfig json = do
  obj <- decodeJson json
  marloweWebServerUrl <- obj .: "marloweWebServerUrl"
  develMode <- obj .: "develMode"
  network <- obj .: "network"
  pure
    { marloweWebServerUrl: ServerURL marloweWebServerUrl
    , develMode
    , network
    }

main :: Json -> Effect Unit
main configJson = do
  config <- Effect.liftEither $ decodeConfig configJson

  JsonBigInt.patchers.patchStringify
  JsonBigInt.patchers.patchParse

  let
    logger :: String -> Effect Unit
    logger =
      if config.develMode then Console.log
      else const (pure unit)
    runtime = Marlowe.Runtime.Web.runtime config.marloweWebServerUrl
    -- FIXME: Slotting numbers have to be provided by Marlowe Runtime
    slotting =
      case config.network of
        "mainnet" -> Slotting { slotLength: BigInt.fromInt 1000 , slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1591566291000" }
        _ -> Slotting { slotLength: BigInt.fromInt 1000 , slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1666656000000" }

  doc :: HTMLDocument <- document =<< window
  container :: Element <- maybe (throw "Could not find element with id 'app-root'") pure =<<
    (getElementById "app-root" $ toNonElementParentNode doc)
  reactRoot <- createRoot container
  launchAff_ do
    contractStream <- do
      let
        reqInterval = RequestInterval (Milliseconds 50.0)
        pollInterval = PollingInterval (Milliseconds 60_000.0)
        -- filterContracts getContractResponse = case un ContractHeader getContractResponse.resource of
        --  { block: Nothing } -> true
        --  { block: Just (BlockHeader { blockNo: BlockNumber blockNo }) } -> blockNo > 909000 -- 904279
        filterContracts _ = true
        maxPages = Nothing -- Just (MaxPages 1)
        params = { tags: [ raffleAppTag ] }
      Streaming.mkContractsWithTransactions pollInterval reqInterval params filterContracts maxPages config.marloweWebServerUrl

    CardanoMultiplatformLib.importLib >>= case _ of
      Nothing -> liftEffect $ logger "Cardano serialization lib loading failed"
      Just cardanoMultiplatformLib -> do
        walletInfoCtx <- liftEffect $ createContext Nothing
        appCtx <- liftEffect $ createContext { isDesktop: true }
        msgHubComponent /\ msgHub <- liftEffect $ mkMessageHub
        let
          mkAppCtx =
            { cardanoMultiplatformLib
            , appCtx
            , walletInfoCtx
            , logger
            , contractStream
            , msgHub
            , runtime
            , aboutMarkdown: "placeholder"
            , slotting
            }

        app <- liftEffect $ runReaderT mkApp mkAppCtx
        liftEffect $ renderRoot reactRoot $ msgHubComponent [ app unit ]

