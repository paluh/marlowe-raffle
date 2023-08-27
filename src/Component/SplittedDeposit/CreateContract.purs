module Component.SplittedDeposit.CreateContract where

import Prelude

import CardanoMultiplatformLib (bech32ToString)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Types (Bech32)
import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.CreateContract.Machine as Machine
import Component.MarloweYaml (marloweYaml)
import Component.Types (MkComponentM, PriceFundingContracts(..), RaffleContracts(..), RaffleDeploymentPlan(..), RaffleOnChainStatus(..), RaffleOrganizer(..), RaffleRound(..), WalletInfo, initialDepositTagsFromPlan, mkRaffleContracts, rolesConfigFromPlan)
import Component.Types.AppTags (AppTags(..), ExtraTags(..))
import Component.Types.AppTags as AppTags
import Component.Types.Raffle (unsafeDecodeAssetName)
import Component.Widgets (link, spinner)
import Component.Widgets.Form (multiAddressInput)
import Contrib.Cardano (assetNameFromString)
import Contrib.Cardano as C
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.Polyform.FormSpecBuilder (FieldIdPrefix(..), formSpecBuilderT, hoistFormSpec)
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecs.StatelessFormSpec (StatelessFormSpec, liftValidator)
import Contrib.Polyform.FormSpecs.StatelessFormSpec (renderFormSpec) as StatelessFormSpec
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatlessFormSpec
import Contrib.React.Basic.Hooks.UseMooreMachine (useMooreMachine)
import Contrib.React.MarloweGraph (marloweGraph)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (ChoiceFieldChoices(..), StatelessBootstrapFormSpec, UseChoiceField(..), choiceField, choiceField', dateTimeField, selectFieldChoice)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (booleanField) as StatelessFormSpec
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders as StatelessFormSpecBuilders
import Contrib.Web.SubtleCrypto (digestString, sha256)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (encodeJson)
import Data.Array as Array
import Data.Array.ArrayAL as AL
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..), hush, note)
import Data.Enum (class BoundedEnum, class Enum, enumFromTo, fromEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Eq (class Eq1)
import Data.Foldable (fold)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (un)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split, trim)
import Data.String as String
import Data.Time.Duration (Days(..), Seconds(..))
import Data.Time.Duration as Duration
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import HexString (stringToUtf8Hex)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError)
import Marlowe.Runtime.Web.Types (ContractEndpoint, PostContractsError, Tags(..))
import Marlowe.Runtime.Web.Types as Runtime
import Polyform.Batteries.UrlEncoded.Types.Errors as Errors
import Polyform.Validator (liftFn, liftFnEither, liftFnMaybe)
import Polyform.Validator (liftFnEither) as Validator
import React.Basic (fragment) as DOOM
import React.Basic.DOM (css)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM (text) as R
import React.Basic.DOM.Generated (br, div, div_, hr, p_, span_) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, fragment, useEffect, useState', (/\))
import React.Basic.Hooks (bind, discard, keyed, readRef) as R
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Tab (tab)
import ReactBootstrap.Tabs (tabs)
import ReactBootstrap.Tabs as Tabs
import ReactBootstrap.Types (eventKey)
import Text.CSV as CSV
import Utils.React.Basic.Hooks (useStateRef', useStateWithRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))

type Props =
  { onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , walletInfo :: WalletInfo Wallet.Api
  , walletContext :: WalletContext
  , now :: Instant
  }

newtype AutoRun = AutoRun Boolean

-- Up to 10 rounds
data RaffleRoundsNumber
  = OneRound
  | TwoRounds
  | ThreeRounds
  | FourRounds
  | FiveRounds
  | SixRounds
  | SevenRounds
  | EightRounds
  | NineRounds
  | TenRounds

derive instance Eq RaffleRoundsNumber
derive instance Ord RaffleRoundsNumber
derive instance Generic RaffleRoundsNumber _
instance Show RaffleRoundsNumber where
  show = genericShow

instance Enum RaffleRoundsNumber where
  succ = genericSucc
  pred = genericPred

instance Bounded RaffleRoundsNumber where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum RaffleRoundsNumber where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

raffleRoundsNumberToLabel :: RaffleRoundsNumber -> String
raffleRoundsNumberToLabel = case _ of
  OneRound -> "One round"
  TwoRounds -> "Two rounds"
  ThreeRounds -> "Three rounds"
  FourRounds -> "Four rounds"
  FiveRounds -> "Five rounds"
  SixRounds -> "Six rounds"
  SevenRounds -> "Seven rounds"
  EightRounds -> "Eight rounds"
  NineRounds -> "Nine rounds"
  TenRounds -> "Ten rounds"

reqValidator missingError = liftFnMaybe (const [ missingError ]) identity

reqValidator' = reqValidator "This field is required"

reqDateTimeField initial label helpText = dateTimeField initial (Just label) helpText reqValidator'

type RaffleConfigFormResult =
  { possiblePriceTokensCurrency :: Maybe C.PolicyId
  , participants :: NonEmptyArray Bech32
  , contractsTimeout :: V1.Timeout
  , extraTags :: ExtraTags
  , organizer :: RaffleOrganizer
  , roundsNumber :: RaffleRoundsNumber
  }

newtype Now = Now Instant

mkRaffleConfigFormSpec
  :: CardanoMultiplatformLib.Lib
  -> RaffleRoundsNumber
  -> Now
  -> RaffleOrganizer
  -> Array C.PolicyId
  -> StatelessBootstrapFormSpec Effect Query RaffleConfigFormResult
mkRaffleConfigFormSpec cardanoMultiplatformLib roundsNumber (Now now) (RaffleOrganizer organizerAddr) policyIds = FormSpecBuilder.evalBuilder Nothing $ do
  let
    initialTimeout = Instant.toDateTime <$> do
      let
        inOneYear = unInstant now <> Duration.fromDuration (Days 365.0)
      instant inOneYear
  ado
    -- organizer <- RaffleOrganizer <$> StatelessFormSpecBuilders.textInput
    --   { missingError: "Please provide an address where a role token should be sent after minting"
    --   , helpText: Just $ DOOM.div_
    --       [ DOOM.text $
    --           "Organizer address (by default your wallet address). Organizer will "
    --           <> "coordinate the raffle and will receive a role tokens if we need to mint them."
    --       ]
    --   , initial: bech32ToString organizerAddr
    --   , label: Just $ DOOM.text "Raffle organizer"
    --   , touched: false
    --   , validator: requiredV' $ Validator.liftFnMMaybe (const $ pure [ "Invalid address" ]) \str -> do
    --       bech32FromString cardanoMultiplatformLib str
    --   }
    roundsNumber <- do
      let
        choiceConfig a =
          { label: raffleRoundsNumberToLabel a
          , helpText: Nothing
          , disabled: false
          }
      choiceField'
        (UseSelectField choiceConfig)
        Nothing
        { label: Just $ DOOM.text "Number of raffle rounds"
        , initial: roundsNumber
        , touched: true
        }

    organizer <- RaffleOrganizer organizerAddr <$ StatelessFormSpec.booleanField
      { helpText: DOOM.div_
          [ DOOM.text $
              "Organizer will coordinate the raffle and will receive a role tokens if we need to mint them."
          ]
      , initial: true
      , label: DOOM.text "Use current wallet as raffle organizer"
      , touched: false
      , disabled: true
      }

    -- priceTokensCurrency <- currencySymbolInput
    --   { helpText: Just $ DOOM.text "Please provide a currency symbol for the price tokens. If you don't provide one we will mint the tokens ourselves."
    --   , initial: ""
    --   , label: Just $ DOOM.text "Price token currency symbol"
    --   }
    -- selectFieldChoice :: String -> String -> SelectFieldChoice
    -- selectFieldChoice label value = { disabled: false, helpText: Nothing, label, value }
    possiblePriceTokensCurrency <- choiceField do
      let
        choices = SelectFieldChoices
          { choices: AL.solo' (selectFieldChoice "" "") $ policyIds <#> \pId -> do
              let
                pIdStr = C.policyIdToHexString pId
              selectFieldChoice pIdStr pIdStr
          , monospace: true
          }
        validator = liftFnEither case _ of
          Nothing -> pure Nothing
          Just "" -> pure Nothing
          Just str -> note [ "Invalid policy id" ] $ Just <$> C.policyIdFromHexString str
      { choices
      , initial: ""
      , label: Just $ DOOM.text "Price token currency symbol"
      , helpText: Just $ DOOM.text "Please provide a currency symbol for the price tokens. If you don't provide one we will mint the tokens ourselves."
      , validator
      , touched: true
      }

    contractsTimeout <- Instant.fromDateTime <$> reqDateTimeField
      initialTimeout
      (DOOM.text "Contracts timeout")
      ( Just $ DOOM.p_
          [ DOOM.text "We use a single timeout to simplify configuration - in the case of the timeout:"
          , DOOM.br {}
          , DOOM.text "* All already deposited tokens will be paid out to the winner token in the case of deposit."
          , DOOM.br {}
          , DOOM.text "* All price raffle tokens would be paid back without finalization in the case of Raffle."
          ]
      )

    participants <- do
      let
        fieldId = "participants"
        arr = multiAddressInput cardanoMultiplatformLib
          { missingError: "Please provide participants list separating addresses with a comma, white space or a new line"
          , helpText: Just $ DOOM.text "Please provide a list of participants addresses. We will use your wallet address as a source of funds."
          , placeholder: "Participant addresses"
          , initial: bech32ToString organizerAddr
          , touched: true -- isJust possibleInitialContract
          , label: Just $ DOOM.text "Raffle participants"
          , rows: 3
          , name: Just $ FieldId fieldId
          }
        nonEmptyArr = formSpecBuilderT $ pure $ liftValidator do
          liftFnEither $ Array.uncons >>> case _ of
            Nothing -> Left $ Errors.singleton (Errors.ErrorId fieldId) [ "Please provide at least one participant" ]
            Just { head, tail } -> Right $ NonEmptyArray.cons' head tail
      arr >>> nonEmptyArr

    extraTags <- StatelessFormSpecBuilders.textInput
      { helpText: Just $ DOOM.text $
          "Tag can be used to uniquely identify particular raffle scenario. "
            <> "We will assign unique Id to the raffle contract bundle "
            <> "after the initial tx deployment but if you want to have human "
            <> "readable id then please provide one."
      , initial: ""
      , label: Just $ DOOM.text "Tag"
      , touched: false
      , validator: liftFn case _ of
          Nothing -> ExtraTags []
          Just tags -> ExtraTags $ Array.filter (_ /= "") $ map trim $ split (Pattern ",") tags
      }
    in
      { organizer
      , possiblePriceTokensCurrency
      , contractsTimeout
      , participants
      , extraTags
      , roundsNumber
      }

type ClientError' = ClientError PostContractsError

hoistMaybe :: forall m a. Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT <<< pure

machineProps connectedWallet cardanoMultiplatformLib runtime = do
  let
    env = { connectedWallet, cardanoMultiplatformLib, runtime }
  { initialState: Machine.initialState
  , step: Machine.step
  , driver: Machine.driver env
  , output: identity
  }

data CurrentRun
  = Automatic
  -- This boolean indicates whether we are actually performing the request
  -- at the moment. This is useful to avoid double clicking and show throbber
  | Manual Boolean

-- pure $ sender /\ sender /\ (V1.Token currencySymbol tokenName) /\ (V1.Constant amount)
newtype RaffleRoundConfigBase f = RaffleRoundConfigBase
  { priceBundle :: f (C.PolicyId /\ C.AssetName /\ C.Quantity)
  , priceTokenName :: String
  }
derive instance (Eq1 f) => Eq (RaffleRoundConfigBase f)

type RaffleRoundInitials = RaffleRoundConfigBase Array

type RaffleRoundConfig = RaffleRoundConfigBase NonEmptyArray

mkRaffleRoundsFormSpec
  :: { mintTokens :: Boolean, roundsInitials :: NonEmptyArray RaffleRoundInitials }
  -> StatelessFormSpec Effect (Array (Array JSX)) String Query (NonEmptyArray RaffleRoundConfig)
mkRaffleRoundsFormSpec { mintTokens, roundsInitials } = FormSpecBuilder.evalBuilder (Just $ FieldIdPrefix "raffle-rounds") $ do
  for roundsInitials \roundInitials -> hoistFormSpec (StatlessFormSpec.mapRender Array.singleton)
    let
      RaffleRoundConfigBase initials = roundInitials
    in
      ado
        priceTokenName <- StatelessFormSpecBuilders.textInput
          { missingError: "Please provide an name of price token for this round"
          , helpText: Just $ DOOM.div_
              [ DOOM.text $
                  if mintTokens then "Price token name which should be minted for this round winner and used for the final price withdrawal."
                  else "Price token name which you probably already minted or will mint for a provided currency."
              ]
          , initial: initials.priceTokenName
          , label: Just $ DOOM.text "Price token name"
          , touched: false
          , validator: requiredV' $ identity
          }

        priceBundle <- StatelessFormSpecBuilders.textArea
          { missingError: "Please provide token bundle CSV value"
          , helpText: Just $ DOOM.text "Please provide a list of tokens: currency symbol, token name, amount. We will use your wallet address as a source of funds."
          , placeholder: "currency symbol, token name, amount"
          , initial: String.joinWith "\n" $ initials.priceBundle <#> \(policyId /\ assetName /\ quantity) -> do
              let
                assetNameStr = unsafeDecodeAssetName assetName
              C.policyIdToHexString policyId <> "," <> assetNameStr <> "," <> BigInt.toString (un C.Quantity quantity)
          , label: Just $ DOOM.text "Price bundle CSV"
          , touched: false -- isJust possibleInitialContract
          , validator: requiredV' $ Validator.liftFnEither \csvString -> do
              csv <- lmap (const $ [ "Invalid CSV" ]) $ CSV.parse csvString
              tokens <- for csv case _ of
                [ policyIdStr, assetNameStr, quantityStr ] -> do
                  quantityValue <- note [ "Invalid amount" ] $ BigInt.fromString quantityStr
                  case C.policyIdFromHexString policyIdStr of
                    Just policyId -> do
                      let
                        assetNameHex = stringToUtf8Hex assetNameStr
                      pure $ policyId /\ C.AssetName assetNameHex /\ C.Quantity quantityValue
                    Nothing -> Left [ "Invalid policy ID" ]
                _ -> Left [ "Invalid CSV" ]
              case NonEmptyArray.fromArray tokens of
                Just tokens' -> pure tokens'
                Nothing -> Left [ "Token bundle cannot be empty" ]
          , rows: 5
          }

        in
          RaffleRoundConfigBase
            { priceBundle
            , priceTokenName
            }

type RaffleRoundsProps =
  { onRoundsConfigChange :: Maybe (NonEmptyArray RaffleRound) -> Effect Unit
  , walletInfo :: WalletInfo Wallet.Api
  , walletContext :: WalletContext
  , roundsInitials :: NonEmptyArray RaffleRoundInitials
  , mintTokens :: Boolean
  }

mkRaffleRoundsComponent :: MkComponentM (RaffleRoundsProps -> JSX)
mkRaffleRoundsComponent = do
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  liftEffect $ component "RoleTokensAssignment" \{ onRoundsConfigChange, walletInfo, walletContext, roundsInitials, mintTokens } -> R.do
    let
      formSpec = mkRaffleRoundsFormSpec { mintTokens, roundsInitials }
      handlePossibleResult = case _ of
        Just (V (Right raffleRoundConfig) /\ _) -> do
          let
            rounds = raffleRoundConfig <#> \(RaffleRoundConfigBase { priceBundle, priceTokenName }) -> do
              let
                tokenBundle = fold $ priceBundle <#> \(policyId /\ tokenName /\ quantity) ->
                  C.NonAdaAssets $ Map.singleton (policyId /\ tokenName) quantity
              RaffleRound
                { priceAssetName: C.AssetName $ stringToUtf8Hex priceTokenName
                , tokenBundle
                }
          onRoundsConfigChange $ Just rounds
        _ -> onRoundsConfigChange Nothing


    { formState, onSubmit: onSubmit', result: possibleResult } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit: \{ result } -> handlePossibleResult result
      , validationDebounce: Seconds 0.5
      }

    possibleResultRef <- useStateRef' possibleResult
    useEffect possibleResult do
      R.readRef possibleResultRef >>= handlePossibleResult
      pure $ pure unit

    let
      fieldsSets = StatelessFormSpec.renderFormSpec formSpec formState

    pure $ DOOM.fragment $ flip foldMapWithIndex fieldsSets \idx fieldsSet ->
      [ DOM.h4 { className: "mt-4 mb-3" } [ DOOM.text $ "Round " <> show (idx + 1) ] ] <> fieldsSet

configFromFormResult result = result >>= fst >>> un V >>> hush

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  let
    initialAutoRun = AutoRun true
    -- NO OTHER INITIAL WORKS!
    initialRoundsNumber = OneRound

  raffleRoundsComponent <- mkRaffleRoundsComponent

  liftEffect $ component "CreateContract" \{ now, onSuccess, onDismiss, walletContext, walletInfo } -> React.do
    currentRun /\ setCurrentRun <- React.useState' Nothing
    { state: submissionState, applyAction, reset: resetStateMachine } <- do
      let
        props = machineProps walletInfo cardanoMultiplatformLib runtime
      useMooreMachine props

    let
      mkRafflePlan raffleConfig rounds = RaffleDeploymentPlan
        { organizer: raffleConfig.organizer
        , contractsTimeout: raffleConfig.contractsTimeout
        , priceTokensCurrency: raffleConfig.possiblePriceTokensCurrency
        , rounds
        , participants: raffleConfig.participants
        }
      mkContracts config rounds = do
        let
          plan = mkRafflePlan config rounds
        hush $ mkRaffleContracts plan Nothing

    raffleConfigFormSpec <- React.useMemo unit \_ -> mkRaffleConfigFormSpec
      cardanoMultiplatformLib
      initialRoundsNumber
      (Now now)
      do
        RaffleOrganizer do
          let WalletContext { changeAddress } = walletContext
          changeAddress
      do
        let WalletContext { balance } = walletContext
        Array.nub $ Array.sort $ Array.catMaybes $ C.valueAssetIds balance <#> case _ of
          C.AdaAssetId -> Nothing
          C.AssetId policyId assetName -> Just $ policyId

    possibleRoundsConfig /\ possibleRoundsConfigRef /\ setPossibleRoundsConfig <- useStateWithRef' Nothing

    { formState: raffleConfigFormState, onSubmit: onSubmit', result: raffleConfigFormResult } <- useStatelessFormSpec
      { spec: raffleConfigFormSpec
      , onSubmit: \{ result } -> do
          possibleRounds <- R.readRef possibleRoundsConfigRef
          let
            contractInfo = do
              config <- configFromFormResult result
              rounds <- possibleRounds
              let
                plan = mkRafflePlan config rounds
                encodeTags = initialDepositTagsFromPlan plan
                rolesConfig = rolesConfigFromPlan plan
              RaffleContracts { rounds: contracts } <- hush $ mkRaffleContracts plan Nothing
              let
                PriceFundingContracts { initialContracts } = snd $ NonEmptyArray.head contracts
                initialDeposit = fst $ NonEmptyArray.head initialContracts

              pure
                { initialDeposit
                , rolesConfig
                , encodeTags
                , extraTags: config.extraTags
                }
          case contractInfo of
            Nothing -> do
              traceM "Failed to create contracts from:"
              traceM $ result
              traceM $ possibleRounds
            Just { extraTags, initialDeposit, rolesConfig, encodeTags } -> do
              -- FIXME: move this to the machine or to local useAff?
              launchAff_ do
                encodeTags >>= case _ of
                  Just raffleTags -> do
                    let
                      tags = AppTags.toRuntimeTags $ AppTags
                        { extraTags
                        , raffleTags
                        }
                    liftEffect $ applyAction $ Machine.TriggerSubmission (Just rolesConfig) initialDeposit tags
                  Nothing -> do
                    traceM "Failed to encode tags"
              -- onSubmit' contracts

      , validationDebounce: Seconds 0.5
      }
    let
      possibleRaffleConfig = configFromFormResult raffleConfigFormResult

      possibleRaffleContracts = join $ mkContracts <$> possibleRaffleConfig <*> possibleRoundsConfig

    possibleRaffleContractsRef <- useStateRef' possibleRaffleContracts

    -- We cache all the possible initials - if a user lower the number of rounds, we don't
    -- lose the data and we can restore it if the user increases the number of rounds again.
    roundsInitials /\ setRoundsInitials <- R.do
      useState' $ (bottom `enumFromTo` top :: NonEmptyArray RaffleRoundsNumber) <#> \roundNumber -> do
        let
          idx = fromEnum roundNumber + 1
          WalletContext { balance: C.Value balance } = walletContext
          -- priceBundle :: Array (C.PolicyId /\ C.AssetName /\ C.Quantity)
          -- balance :: C.Value ~ Value (Map (C.AssetId (Map C.AssetName C.Quantity)))
          priceBundle = Array.catMaybes $ Map.toUnfoldable balance <#> case _ of
            C.AdaAssetId /\ _ -> Nothing
            C.AssetId policyId assetName /\ quantity -> Just $ policyId /\ assetName /\ quantity
        RaffleRoundConfigBase
          { priceBundle
          , priceTokenName: "Price number " <> show idx
          }

    -- React.useEffect (_.changeAddress <<< un WalletContext <<< snd <$> possibleWalletInfo) $ do
    --   case possibleWalletInfo of
    --     Just (_ /\ (WalletContext { changeAddress: Just changeAddress })) -> do
    --       { multiChoiceTest: initialContract } <- liftEffect $ mkInitialContracts changeAddress
    --       case Map.lookup contractFieldId formState.fields of
    --         Just { touched, onChange } -> do
    --           when (not $ un Disj touched) do
    --             onChange [ stringifyWithIndent 2 $ encodeJson initialContract ]
    --         Nothing -> pure unit
    --     _ -> pure unit
    --   pure (pure unit)

    pure $ case submissionState of
      Machine.DefiningContract -> do
        let
          raffleConfigFields = StatelessFormSpec.renderFormSpec raffleConfigFormSpec raffleConfigFormState

          renderTab props children = tab props $ DOM.div { className: "pt-4 h-vh60 overflow-auto" } children
          formBody = DOM.div { className: "row" } $ DOM.div { className: "col-12" } $
            tabs { fill: true, justify: true, defaultActiveKey: "form", variant: Tabs.variant.pills } do
              [ renderTab
                  { eventKey: eventKey "form"
                  , disabled: false
                  , title: DOOM.span_
                      [ Icons.toJSX $ unsafeIcon "filetype-yml"
                      , DOOM.text " Raffle form"
                      ]
                  }
                  [ DOM.div { className: "form-group container" } $
                      raffleConfigFields
                        <> [ DOOM.hr {} ]
                        <> do
                          case possibleRaffleConfig of
                            Just { possiblePriceTokensCurrency, roundsNumber } -> do
                              let
                                roundsInitials' = fromMaybe roundsInitials $
                                  NonEmptyArray.fromArray (NonEmptyArray.take (fromEnum roundsNumber + 1) roundsInitials)
                                props =
                                  { walletInfo
                                  , walletContext
                                  -- , onDismiss: pure unit
                                  -- , onSuccess: const $ pure unit
                                  , onRoundsConfigChange: setPossibleRoundsConfig
                                  , mintTokens: isJust possiblePriceTokensCurrency
                                  , roundsInitials: roundsInitials'
                                  }
                              [ R.keyed (show (fromEnum roundsNumber)) $ raffleRoundsComponent props ]
                            Nothing -> [ DOOM.text "Please finish up the main configuration to set up the rounds" ]
                  ]

              , case possibleRaffleContracts of
                  Nothing -> renderTab
                    { eventKey: eventKey "graph"
                    , disabled: true
                    , title: DOOM.span_
                        [ Icons.toJSX $ unsafeIcon "diagram-2"
                        , DOOM.text " Contract graphs"
                        ]
                    }
                    mempty
                  Just (RaffleContracts { rounds }) -> renderTab
                    { eventKey: eventKey "graph"
                    , disabled: false
                    , title: DOOM.span_
                        [ Icons.toJSX $ unsafeIcon "diagram-2"
                        , DOOM.text " Source graphs"
                        ]
                    }
                    do
                      fold $ rounds `flip mapWithIndex` \roundNo (round /\ funding) -> do
                        let
                          PriceFundingContracts { initialContracts: fundingContracts } = funding
                          possibleDivider = if roundNo /= 0
                            then Array.cons (DOOM.hr { className: "dropdown-divider" })
                            else identity
                          title = Array.cons $ DOM.h5 {} $ "Round #" <> show (roundNo + 1)
                        possibleDivider $ title $ do
                          (contract /\ _) <- NonEmptyArray.toArray fundingContracts
                          pure $ marloweGraph { contract: contract }
              ]
          formActions = fragment
            [ DOM.div { className: "row" } $
                [ DOM.div { className: "col-6 text-start" } $
                    [ link
                        { label: DOOM.text "Cancel"
                        , onClick: onDismiss
                        , showBorders: true
                        , extraClassNames: "me-3"
                        }
                    ]
                , DOM.div { className: "col-6 text-end" } $
                    [ DOM.button
                        do
                          { className: "btn btn-primary"
                          , onClick: onSubmit'
                          , disabled: isNothing possibleRaffleContracts
                          }
                        [ R.text "Submit" ]
                    ]
                ]
            ]

        BodyLayout.component
          { title: stateToTitle submissionState
          , description: stateToDetailedDescription submissionState
          , content: wrappedContentWithFooter
              formBody
              formActions
          }
      Machine.ContractCreated { contract, createTxResponse } -> do
        let
          { links: { contract: contractEndpoint } } = createTxResponse
        BodyLayout.component
          { title: stateToTitle submissionState
          , description: stateToDetailedDescription submissionState
          , content: wrappedContentWithFooter
              do marloweYaml contract
              do
                DOOM.fragment
                  [ DOM.div { className: "row" } $
                      [ DOM.div { className: "col-12 text-end" } $
                          [ DOM.button
                              { className: "btn btn-primary"
                              , onClick: handler_ (onSuccess contractEndpoint)
                              }
                              [ R.text "Done" ]
                          ]
                      ]
                  ]
          }
      machineState -> do
        let
          machineEnv = { connectedWallet: walletInfo, cardanoMultiplatformLib, runtime }
          possibleRequest = currentRun >>= case _ of
            Manual _ -> do
              Machine.driver machineEnv machineState
            _ -> Nothing

          body = fragment
            [ do
                let
                  StepIndex index = (machineStateToStepIndex machineState)
                if index < machineStepsCardinality then do
                  let
                    stepPercent = Int.ceil $ (Int.toNumber (index - 1) / Int.toNumber (machineStepsCardinality - 1)) * 100.0
                    style = css { width: show stepPercent <> "%" }
                  DOM.div { className: "progress mb-3" } $ do
                    DOOM.div { className: "progress-bar", style, children: [] }
                else mempty
            , case currentRun of
                Just (Manual true) -> do
                  DOM.div { className: "d-flex justify-content-center" } $ spinner Nothing
                _ -> mempty
            ]

          formActions = case possibleRequest of
            Nothing -> mempty
            Just request -> DOOM.fragment
              [ DOM.div { className: "row" } $
                  [ DOM.div { className: "col-6 text-start" } $
                      [ link
                          { label: DOOM.text "Cancel"
                          , onClick: onDismiss
                          , showBorders: true
                          , extraClassNames: "me-3"
                          }
                      ]
                  , DOM.div { className: "col-6 text-end" } $
                      [ DOM.button
                          { className: "btn btn-primary"
                          , disabled: case currentRun of
                              Just (Manual b) -> b
                              _ -> false
                          , onClick: handler_ do
                              setCurrentRun (Just $ Manual true)
                              launchAff_ do
                                action <- request
                                liftEffect $ do
                                  applyAction action
                                  setCurrentRun (Just $ Manual false)
                          }
                          [ R.text "Run" ]
                      ]
                  ]
              ]
        BodyLayout.component
          { title: stateToTitle submissionState
          , description: stateToDetailedDescription submissionState
          , content: wrappedContentWithFooter body formActions
          }

stateToTitle :: Machine.State -> String
stateToTitle state = case state of
  Machine.DefiningContract -> "Defining contract"
  Machine.FetchingRequiredWalletContext {} -> "Fetching required wallet context"
  Machine.CreatingTx {} -> "Creating transaction"
  Machine.SigningTx {} -> "Signing transaction"
  Machine.SubmittigTx {} -> "Submitting transaction"
  Machine.ContractCreated {} -> "Contract created"

-- To display progress bar
newtype StepIndex = StepIndex Int

machineStepsCardinality :: Int
machineStepsCardinality = 7

machineStateToStepIndex :: Machine.State -> StepIndex
machineStateToStepIndex state = StepIndex $ case state of
  Machine.DefiningContract -> 1
  Machine.FetchingRequiredWalletContext {} -> 3
  Machine.CreatingTx {} -> 4
  Machine.SigningTx {} -> 5
  Machine.SubmittigTx {} -> 6
  Machine.ContractCreated {} -> 7

-- | We want to describe in details what kind of data we are gathering
-- | when we are performing a given transtition (state determines the next transition in our case)
-- | The output should be readable to the developer which should understand the whole flow.
-- | Let's use standard react-basic JSX functions like: DOM.div { className: "foo" } [ DOOM.text "bar" ]
stateToDetailedDescription :: Machine.State -> JSX
stateToDetailedDescription state = case state of
  Machine.DefiningContract -> DOOM.div_
    [ DOM.p {} $ DOOM.text "We are currently in the initial state, awaiting the user to initiate the contract creation process."
    , DOM.p {} $ DOOM.text "When we receive the correct contract value in JSON format, it will be utilized as part of the request to the Marlowe runtime"
    , DOM.p {} $ DOOM.text "As a user, you have two options for providing the contract:"
    , DOM.ul {}
        [ DOM.li {} $ DOOM.text "Enter a valid contract in JSON format in the input field to the right"
        , DOM.li {} $ DOOM.text "Upload a contract JSON file using the \"Upload\" button."
        ]
    , DOM.p { className: "h3 fw-bold py-3" } $ DOOM.text "How to Provide a Contract in JSON Format:"
    , DOM.p {}
        [ DOOM.text "Please provide a contract in a JSON format. To generate it, you can use a Marlowe library for your language of choice (for example, "
        , DOM.a { href: "https://github.com/input-output-hk/marlowe-ts-sdk", target: "_blank", className: "white-color" } [ DOOM.text " marlowe-ts-sdk" ]
        , DOOM.text "), or you can use the "
        , DOM.a { href: "https://play.marlowe.iohk.io/#/", target: "_blank", className: "white-color" } [ DOOM.text " Marlowe Playground" ]
        , DOOM.text " to generate it. After creating a contract in the simulator within the Marlowe Playground, you can use the \"Download JSON\" button to obtain the contract in JSON format. Once you have the JSON file, you can either enter it in the input field to the right or upload it using the \"Upload\" button."
        ]
    ]
  Machine.FetchingRequiredWalletContext { errors: Nothing } -> DOOM.div_
    [ DOM.p {}
        [ DOOM.text "We are currently fetching the required wallet context for creating the Marlowe Contract on chain." ]
    , DOM.p {}
        [ DOOM.text "The marlowe-runtime requires information about wallet addresses in order to select the appropriate UTxOs to pay for the initial transaction. To obtain the set of addresses from the wallet, we utilize the "
        , DOM.code {} [ DOOM.text "getUsedAddresses" ]
        , DOOM.text " method from CIP-30. The addresses are then re-encoded from the lower-level Cardano CBOR hex format into Bech32 format ("
        , DOM.code {} [ DOOM.text "addr_test..." ]
        , DOOM.text ")."
        ]
    , DOM.p {}
        [ DOOM.text "Please wait while we fetch the wallet context. This process may take a few moments." ]
    ]
  Machine.FetchingRequiredWalletContext { errors: Just error } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "It seems that the provided wallet is lacking addresses or failed to execute the method:"
    , DOM.p {} $ DOOM.text error
    ]
  Machine.CreatingTx { errors: Nothing } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "Utilizing the Marlowe-runtime, this interface enables you to generate an initial transaction. The generated transaction needs to be signed using the wallet you've connected. By doing so, you are authorizing and verifying the transaction's intent and ensuring its secure execution."
    , DOM.p {} $ DOOM.text "Please review all the details carefully before proceeding with the transaction confirmation."
    ]
  Machine.CreatingTx { reqWalletContext, errors: Just error } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "It seems that the marlowe-runtime failed to create the initial transaction:"
    , DOM.p {} $ DOOM.text error
    , DOM.p {} $ DOOM.text "The wallet context we used:"
    , DOM.p {} $ DOOM.text $ unsafeStringify reqWalletContext
    ]
  Machine.SigningTx { errors: Nothing } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "You are currently in the process of digitally signing your initial transaction. This step is critical in validating the transaction's authenticity, confirming that it has indeed originated from you. By signing, you are ensuring the transaction's integrity and non-repudiation."
    , DOM.p {} $ DOOM.text "Carefully review all details to confirm they are correct before finalizing your signature."
    ]
  Machine.SigningTx { errors: Just error } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "It seems that the wallet failed to sign the initial transaction:"
    , DOM.p {} $ DOOM.text error
    ]
  Machine.SubmittigTx { errors: Nothing } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "You have now reached the transaction submission phase. Having signed your initial transaction, it's time to submit it into the system for processing. This step essentially sends the transaction to the network where it's queued for inclusion in the blockchain. Please ensure all details are correct. Once submitted, the transaction is irreversible and will be permanently recorded."
    , DOM.p {} $ DOOM.text "Your transaction journey is almost complete. Press 'Submit' when you are ready."
    ]
  Machine.SubmittigTx { errors: Just error } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "It seems that the marlowe-runtime failed to submit the initial transaction:"
    , DOM.p {} $ DOOM.text error
    ]
  Machine.ContractCreated _ -> DOOM.div_
    [ DOM.p {} $ DOOM.text "Congratulations! Your contract has been successfully created and recorded on the blockchain. This marks the successful completion of your transaction, now encapsulated into a secure, immutable contract. From here, the contract's terms will govern the further actions and transactions. You may want to keep a record of the contract details for future reference. Remember, the blockchain's nature of immutability makes this contract permanent and transparent."
    , DOM.p {} $ DOOM.text "Thank you for using our platform, and feel free to create more contracts as needed."
    ]

-- | Let's use error information and other details of the state to describe the sitution.
-- | Let's use standard react-basic JSX functions like: DOM.div { className: "foo" } [ DOOM.text "bar" ]
stateToDescription :: Machine.State -> JSX
stateToDescription state = case state of
  Machine.DefiningContract -> DOOM.text "Please define your contract."
  Machine.FetchingRequiredWalletContext { errors } -> case errors of
    Nothing -> DOOM.text "Fetching required wallet context."
    Just err -> DOOM.text $ "Fetching required wallet context failed: " <> err
  Machine.CreatingTx { errors } -> case errors of
    Nothing -> DOOM.text "Creating transaction."
    Just err -> DOOM.text $ "Creating transaction failed: " <> err
  Machine.SigningTx { errors } -> case errors of
    Nothing -> DOOM.text "Signing transaction."
    Just err -> DOOM.text $ "Signing transaction failed: " <> err
  Machine.SubmittigTx { errors } -> case errors of
    Nothing -> DOOM.text "Submitting transaction."
    Just err -> DOOM.text $ "Submitting transaction failed: " <> err
  Machine.ContractCreated {} -> DOOM.text "Contract created."

three :: BigInt
three = BigInt.fromInt 3

four :: BigInt
four = BigInt.fromInt 4

-- 07b2fd78a2129da3b4a27f3c1f0877edef5d04f15f13cee5c216e1a1,Token01,1
-- 07b2fd78a2129da3b4a27f3c1f0877edef5d04f15f13cee5c216e1a1,Token02,1
-- 07b2fd78a2129da3b4a27f3c1f0877edef5d04f15f13cee5c216e1a1,Token03,1
-- 07b2fd78a2129da3b4a27f3c1f0877edef5d04f15f13cee5c216e1a1,Token31,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token01,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token02,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token03,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token04,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token05,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token06,1

-- 07b2fd78a2129da3b4a27f3c1f0877edef5d04f15f13cee5c216e1a1,Token01,1
-- 07b2fd78a2129da3b4a27f3c1f0877edef5d04f15f13cee5c216e1a1,Token02,1
-- 07b2fd78a2129da3b4a27f3c1f0877edef5d04f15f13cee5c216e1a1,Token03,1
-- 07b2fd78a2129da3b4a27f3c1f0877edef5d04f15f13cee5c216e1a1,Token31,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token01,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token02,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token03,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token04,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token05,1
-- 221a1498e87ef23cc4d86dc32687ad66a037dfccec756f9defb1a2f0,Token06,1
-- 257efb43dca8e9b859f7553d8a52bdffe7376f93383478cb13de90a4,Token01,1
-- 257efb43dca8e9b859f7553d8a52bdffe7376f93383478cb13de90a4,Token02,1
-- 257efb43dca8e9b859f7553d8a52bdffe7376f93383478cb13de90a4,Token03,1
-- 257efb43dca8e9b859f7553d8a52bdffe7376f93383478cb13de90a4,Token04,1
-- 301ac5a08868551ecca6358bf4cf91fb62ae43cb92537ae69643e9b6,,1
-- 5d4c7f632bb80c6f2f62b3141fd196c1158da484167b71d7439a15bf,Price number 1,1
-- 6a5b9fdd1b5cf3d85b0d5d6265e8438de0571c8efbe397be9e1de6b1,Price number 1,1
-- 89cd4d46ee5982a760cd12886224fbb285aa98211f19d354ccbe452b,1stPrize,1
-- 8a939e4308689f9a242a3e4b0353f48ea0ad8430a3d6cc0c08d671e2,Token01,1
-- 8a939e4308689f9a242a3e4b0353f48ea0ad8430a3d6cc0c08d671e2,Token02,1
-- 8a939e4308689f9a242a3e4b0353f48ea0ad8430a3d6cc0c08d671e2,Token03,1
-- 8a939e4308689f9a242a3e4b0353f48ea0ad8430a3d6cc0c08d671e2,Token31,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,
-- �@OTKPirate2485,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,CNCAla,100000
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,ChainsOfWar08271,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Empowa,500000000
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,FirstCouncil0747,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Gaya0869,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,HOSKY,200000000
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,HardForkEaselle0024,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,JARODIRT485,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,MIRA,200000
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,MechaStag01431,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,MechaStag02331,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,MeteraBlue26,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,MeteraBlue27,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,MeteraBlue29,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone1CNCAla0064,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone1CNCAla0629,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone1CNCAla1587,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone2CNCAla1329,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone2CNCAla1538,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone2CNCAla1628,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone3CNCAla0723,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone3CNCAla0789,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone3CNCAla1263,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone4CNCAla0044,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone4CNCAla0355,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone4CNCAla0900,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone5CNCAla1370,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone5CNCAla1485,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone5CNCAla1957,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone6CNCAla0365,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone6CNCAla1272,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone6CNCAla1537,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone7CNCAla0703,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone7CNCAla1003,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Milestone7CNCAla1524,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,NEWM,100000000000
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,OTK Rum,500000
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,OTKFamilyCrest320,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,PINKPANTHER,100000000
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,PROXIE,1000000000
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,ProxieWomb,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Sabrecat124,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Wisdom0107,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,WisdomOrdinals0302,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,cnc.ala-raffle1,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,cnc.ala-raffle2,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,cnc.ala-raffle3,1
-- 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,wheresCardanoProxiesWaldo679,1


-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token01,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token03,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token04,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token05,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token06,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token07,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token08,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token09,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token10,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token11,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token12,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token13,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token14,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token15,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token16,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token17,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token18,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token19,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token20,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token21,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token22,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token23,1
-- 9ea08c58d46e6fed49ad4970c7a2a8e49f86cf72fd2a60e46fc17147,Token24,1
-- b38671ea1ac7ad539e8d855e21fcd2ca750db80b4863643bca963ef9,Price number 1,1
-- c5bdde7f65972a855873e6fd3c5649e1845b36c7b127a37c76c7c167,Price number 1,1
-- d09845ed38832e8137b7274c00b10e7b2608c4d808f27dbeab99bd3f,,1
