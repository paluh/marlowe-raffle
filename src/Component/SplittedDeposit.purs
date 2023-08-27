module App.Pages.SplittedDeposit where

import Prelude

import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs as ApplyInputs
import Component.ApplyInputs.Machine as ApplyInputs.Machine
import Component.BodyLayout as BodyLayout
import Component.BodyLayout as BodyLayout
import Component.ContractDetails as ContractDetails
import Component.ContractTemplates.ContractForDifferencesWithOracle as ContractForDifferencesWithOracle
import Component.ContractTemplates.Escrow as Escrow
import Component.ContractTemplates.Swap as Swap
import Component.CreateContract (runLiteTag)
import Component.CreateContract as CreateContract
import Component.SplittedDeposit.CreateContract as SplittedDeposit.CreateContract
import Component.Types (ContractInfo(..), MessageContent(..), MessageHub(..), MkComponentM, Slotting(..), WalletInfo)
import Component.Types (ContractInfo, MessageContent(..), MessageHub(..), MkComponentM)
import Component.Types.AppTags (AppTags(..), ExtraTags(..))
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table (orderingHeader) as Table
import Component.Widgets (buttonWithIcon)
import Component.Widgets (buttonWithIcon, linkWithIcon)
import Component.Withdrawals as Withdrawals
import Contrib.Data.JSDate (toLocaleDateString, toLocaleTimeString) as JSDate
import Contrib.Fetch (FetchError)
import Contrib.Polyform.FormSpecBuilder (evalBuilder')
import Contrib.Polyform.FormSpecs.StatelessFormSpec (renderFormSpec)
import Contrib.React.Svg (loadingSpinnerLogo)
import Contrib.ReactBootstrap.DropdownButton (dropdownButton)
import Contrib.ReactBootstrap.DropdownItem (dropdownItem)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (FormControlSizing(..), StatelessBootstrapFormSpec, textInput)
import Control.Alt ((<|>))
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, encodeJson, stringify)
import Data.Array as Array
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant (Instant, instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either, hush)
import Data.Foldable (any, fold, or)
import Data.Foldable (fold, null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Function (on)
import Data.JSDate (fromDateTime) as JSDate
import Data.List (concat, intercalate)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (un)
import Data.Set as Set
import Data.String (contains, length)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration as Duration
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Now as Now
import Effect.Unsafe (unsafePerformEffect)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (put')
import Marlowe.Runtime.Web.Types (ContractHeader(ContractHeader), Payout(..), PutTransactionRequest(..), Runtime(..), ServerURL, SlotNumber(..), Tags(..), TransactionEndpoint, TransactionsEndpoint, TxOutRef, WithdrawalsEndpoint, toTextEnvelope, txOutRefToString)
import Marlowe.Runtime.Web.Types as Runtime
import Marlowe.Runtime.Web.Types as Runtime
import Polyform.Validator (liftFnM)
import React.Basic (fragment)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (br, div_, text) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, readRef, useContext, useState, useState', (/\))
import React.Basic.Hooks (JSX, component, (/\))
import React.Basic.Hooks as R
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import ReactBootstrap (overlayTrigger, tooltip)
import ReactBootstrap (overlayTrigger, tooltip)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Table (striped) as Table
import ReactBootstrap.Table (table)
import ReactBootstrap.Types (placement)
import ReactBootstrap.Types (placement)
import ReactBootstrap.Types as OverlayTrigger
import ReactBootstrap.Types as OverlayTrigger
import Utils.React.Basic.Hooks (useMaybeValue', useStateRef')
import Utils.React.Basic.Hooks (useMaybeValue, useStateRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))

type Props =
  { possibleContracts :: Maybe (Array ContractInfo) -- `Maybe` indicates if the contracts where fetched already
  }

data OrderBy
  = OrderByCreationDate
  | OrderByLastUpdateDate

derive instance Eq OrderBy

data ModalAction
  = NewContract Instant
  | ContractDetails
    { contract :: Maybe V1.Contract
    , state :: Maybe V1.State
    , initialContract :: V1.Contract
    , initialState :: V1.State
    , transactionEndpoints :: Array Runtime.TransactionEndpoint
    }
  -- | ContractDetails
  --  { contract :: Maybe V1.Contract
  --  , state :: Maybe V1.State
  --  , initialContract :: V1.Contract
  --  , initialState :: V1.State
  --  , transactionEndpoints :: Array Runtime.TransactionEndpoint
  --  }
  --| ApplyInputs TransactionsEndpoint ApplyInputs.Machine.MarloweContext
  -- | Withdrawal WithdrawalsEndpoint (NonEmptyArray.NonEmptyArray String) TxOutRef
  -- | ContractTemplate ContractTemplate
derive instance Eq ModalAction

mkSplittedDeposit :: MkComponentM (Props -> JSX)
mkSplittedDeposit = do
  MessageHub msgHubProps <- asks _.msgHub
  contractDetails <- ContractDetails.mkComponent
  createContractComponent <- SplittedDeposit.CreateContract.mkComponent
  walletInfoCtx <- asks _.walletInfoCtx
  slotting <- asks _.slotting

  liftEffect $ component "SplittedDeposit" \{ possibleContracts } -> R.do
    possibleModalAction /\ setModalAction /\ resetModalAction <- useMaybeValue Nothing -- (Just (NewContract $ unsafePerformEffect Now.now))
    possibleModalActionRef <- useStateRef' possibleModalAction
    possibleWalletInfo /\ possibleWalletContext <- R.do
      ctx <- R.useContext walletInfoCtx
      case ctx of
        Nothing -> pure (Nothing /\ Nothing)
        Just (wi /\ wc) -> pure (Just wi /\ Just wc)
    ordering /\ updateOrdering <- useState { orderBy: OrderByCreationDate, orderAsc: false }

    pure $
      case possibleModalAction, possibleWalletInfo, possibleWalletContext of
        Just (NewContract now), Just wi, Just wc -> createContractComponent
          { walletInfo: wi
          , walletContext: wc
          , onDismiss: resetModalAction
          , onSuccess: \_ -> do
              msgHubProps.add $ Success $ DOOM.text $ fold
                [ "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain."
                , "Contract status should change to 'Confirmed' at that point."
                ]
              resetModalAction
          , now
          }
        Just (ContractDetails { contract, state, initialContract, initialState, transactionEndpoints}), _, _ -> do
          let
            onClose = resetModalAction
          contractDetails { contract, onClose, state, transactionEndpoints, initialContract, initialState }
        _, _, _ -> BodyLayout.component
          { title: "Splitted deposit"
          , description: DOOM.text "This app allows you to split simple but too heavy deposit / payout scenario so Marlowe can handle it."
          , content: R.fragment
              [ DOM.div { className: "row p-4" } do
                  let
                    disabled = isNothing possibleWalletInfo
                    newContractButton = buttonWithIcon
                      { icon: unsafeIcon "file-earmark-plus h5 me-1"
                      , label: DOOM.text "Create Contract"
                      , extraClassNames: "font-weight-bold me-2"
                      , disabled
                      , onClick: do
                          now <- liftEffect $ Now.now
                          R.readRef possibleModalActionRef >>= case _ of
                            Nothing -> setModalAction $ NewContract now
                            _ -> pure unit
                      }
                  [ DOM.div { className: "col-12" } $ Array.singleton $ do
                      let
                        buttons = DOM.div { className: "text-end" }
                          [ newContractButton
                          -- , templateContractButton
                          ]
                      if disabled then do
                        let
                          tooltipJSX = tooltip
                            { placement: placement.left }
                            (DOOM.text "Connect to a wallet to add a contract")
                        overlayTrigger
                          { overlay: tooltipJSX
                          , placement: OverlayTrigger.placement.bottom
                          }
                          -- Disabled button doesn't trigger the hook,
                          -- so we wrap it in a `span`
                          buttons
                      else
                        buttons
                  ]
              , do
                  let
                    possibleContracts' = do
                      contracts <- possibleContracts
                      let
                        -- Quick and dirty hack to display just submited contracts as first
                        someFutureBlockNumber = Runtime.BlockNumber 19058430
                        sortedContracts = case ordering.orderBy of
                          OrderByCreationDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.createdAt)) contracts
                          OrderByLastUpdateDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.updatedAt)) contracts
                      pure $
                        if ordering.orderAsc then sortedContracts
                        else Array.reverse sortedContracts

                  case possibleContracts' of
                    Just [] -> DOOM.text "No contracts yet"
                    Just contracts | not (null contracts) -> DOM.div { className: "row" } $ DOM.div { className: "col-12 mt-3" } do
                      [ table { striped: Table.striped.boolean true, hover: true }
                          [ DOM.thead {} do
                              let
                                orderingTh = Table.orderingHeader ordering updateOrdering
                                th label = DOM.th { className: "text-center text-muted" } [ label ]
                              [ DOM.tr {}
                                  [ do
                                      let
                                        label = DOOM.fragment [ DOOM.text "Created" ] --, DOOM.br {},  DOOM.text "(Block number)"]
                                      orderingTh label OrderByCreationDate
                                  , do
                                      let
                                        label = DOOM.fragment [ DOOM.text "Updated" ] --, DOOM.br {},  DOOM.text "(Block number)"]
                                      orderingTh label OrderByLastUpdateDate
                                  , DOM.th { className: "text-center w-16rem" } $ DOOM.text "Contract Id"
                                  , th $ DOOM.text "Tags"
                                  , th $ DOOM.text "Actions"
                                  ]
                              ]
                          , DOM.tbody {} $ contracts <#> \ci@(ContractInfo { _runtime, marloweInfo, tags: contractTags }) ->
                              let
                                ContractHeader { contractId } = _runtime.contractHeader
                                tdCentered = DOM.td { className: "text-center" }
                                tdSlotInfo Nothing = tdCentered $ []
                                tdSlotInfo (Just slotNo) = do
                                  let
                                    slotNoInfo = do
                                      let
                                        dateTime = slotToTimestamp slotting slotNo
                                        jsDate = JSDate.fromDateTime dateTime
                                      [ DOOM.text $ JSDate.toLocaleDateString jsDate
                                      , DOOM.br {}
                                      , DOOM.text $ JSDate.toLocaleTimeString jsDate
                                      ]
                                  DOM.td { className: "text-center" } $ DOM.small {} slotNoInfo

                              in
                                DOM.tr { className: "align-middle" }
                                  [ tdSlotInfo $ _.slotNo <<< un Runtime.BlockHeader <$> ContractInfo.createdAt ci
                                  , tdSlotInfo $ _.slotNo <<< un Runtime.BlockHeader <$> ContractInfo.updatedAt ci
                                  , DOM.td { className: "text-center" } $ DOM.span { className: "d-flex" }
                                      [ DOM.a
                                          do
                                            let
                                              onClick = case marloweInfo of
                                                Just (MarloweInfo { state, currentContract, initialContract, initialState }) -> do
                                                  let
                                                    transactionEndpoints = _runtime.transactions <#> \(_ /\ transactionEndpoint) -> transactionEndpoint
                                                  setModalAction $ ContractDetails
                                                    { contract: currentContract
                                                    , state
                                                    , initialState: initialState
                                                    , initialContract: initialContract
                                                    , transactionEndpoints
                                                    }
                                                _ -> pure unit
                                            { className: "cursor-pointer text-decoration-none text-reset text-decoration-underline-hover truncate-text w-16rem d-inline-block"
                                            , onClick: handler_ onClick
                                            -- , disabled
                                            }
                                          [ text $ txOutRefToString contractId ]
                                      , DOM.a { href: "#", className: "cursor-pointer text-decoration-none text-decoration-underline-hover text-reset" } $
                                          Icons.toJSX $ unsafeIcon "clipboard-plus ms-1 d-inline-block"
                                      ]
                                  , tdCentered
                                      [ do
                                          let
                                            AppTags { extraTags: ExtraTags extraTags } = contractTags
                                          DOOM.text $ intercalate ", " extraTags
                                      ]
                                  , tdCentered []
                                  ]
                          ]
                      ]
                    _ ->
                      DOM.div
                        -- { className: "col-12 position-absolute top-0 start-0 w-100 h-100 d-flex justify-content-center align-items-center blur-bg z-index-sticky"
                        { className: "col-12 position-relative d-flex justify-content-center align-items-center blur-bg z-index-sticky"
                        }
                        $ loadingSpinnerLogo
                            {}
              ]
          }

slotToTimestamp :: Slotting -> SlotNumber -> DateTime
slotToTimestamp (Slotting { slotZeroTime, slotLength }) (SlotNumber n) = fromMaybe bottom do
  let
    slotNo = BigInt.fromInt n
    time = BigInt.toNumber $ slotZeroTime + (slotNo * slotLength)
  instant <- instantFromMillis time
  pure $ Instant.toDateTime instant

instantFromMillis :: Number -> Maybe Instant
instantFromMillis ms = instant (Duration.Milliseconds ms)

actionIconSizing :: String
actionIconSizing = " h4"
