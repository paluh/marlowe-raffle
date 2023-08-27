module Component.App where

import Prelude

import App.Pages.SplittedDeposit (mkSplittedDeposit)
import CardanoMultiplatformLib (bech32ToString)
import Component.Assets.Svgs (marloweLogoUrl)
import Component.BodyLayout as BodyLayout
import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (mkContractList)
import Component.InputHelper (addressesInContract, rolesInContract)
import Component.LandingPage (mkLandingPage)
import Component.MessageHub (mkMessageBox, mkMessagePreview)
import Component.Types (ContractInfo(..), MessageContent(Success, Info), MessageHub(MessageHub), MkComponentMBase, WalletInfo(..))
import Component.Types.AppTags as AppTags
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.UseWithdrawal (HookStatus(..), useWithdrawal)
import Component.UseWithdrawal.Blockfrost as B
import Component.Widgets (link, linkWithIcon)
import Contrib.Cardano as C
import Contrib.Data.Map (New(..), Old(..), additions, deletions) as Map
import Contrib.Halogen.Subscription (MinInterval(..))
import Contrib.Halogen.Subscription (bindEffect, foldMapThrottle) as Subscription
import Contrib.React.Svg (svgImg)
import Contrib.ReactSyntaxHighlighter (yamlSyntaxHighlighter)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map (catMaybes, empty, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Data.Newtype (un)
import Data.Newtype as Newtype
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Now (now)
import Effect.Uncurried (mkEffectFn1)
import Halogen.Subscription (Emitter) as Subscription
import Language.Marlowe.Core.V1.Semantics (emptyState) as V1
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsEvent, ContractWithTransactionsMap, ContractWithTransactionsStream(..))
import Marlowe.Runtime.Web.Types (PolicyId(..), TxOutRef(..))
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX)
import React.Basic as ReactContext
import React.Basic.DOM (img, span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, provider, readRef, useEffect, useEffectOnce, useState')
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Offcanvas (offcanvas)
import ReactBootstrap.Offcanvas as Offcanvas
import Record as Record
import Type.Prelude (Proxy(..))
import Utils.React.Basic.Hooks (useEmitter', useLoopAff, useStateRef, useStateRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))
import WalletContext as WalletContext
import Web.HTML (window)

-- | Debugging helpers which allow us to automatically connect wallet
data WalletBrand
  = Lace
  | Yoroi
  | Nami
  | Eternl

instance Show WalletBrand where
  show Yoroi = "Yoroi"
  show Nami = "Nami"
  show Lace = "Lace"
  show Eternl = "Eternl"

autoConnectWallet :: WalletBrand -> (WalletInfo Wallet.Api -> Effect Unit) -> Aff Unit
autoConnectWallet walletBrand onSuccess = liftEffect (window >>= Wallet.cardano) >>= case _ of
  Nothing -> do
    -- We use this function in development mode, so we can just throw an error
    liftEffect $ throw $ "Missing \"cardano\" window attr"
  Just cardano -> do
    let
      extractWallet = case walletBrand of
        Lace -> Wallet.lace
        Nami -> Wallet.nami
        Yoroi -> Wallet.yoroi
        Eternl -> Wallet.eternl
    liftEffect (extractWallet cardano) >>= traverse walletInfo >>= case _ of
      Nothing -> do
        liftEffect $ throw $ "Unable to extract wallet " <> show walletBrand
      Just walletInfo@(WalletInfo { wallet }) -> do
        Wallet.enable wallet >>= case _ of
          Right walletApi -> do
            let
              walletInfo' = Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) walletInfo
            liftEffect $ onSuccess walletInfo'
          -- FIXME: paluh - handle error
          Left _ -> pure unit

-- | Use this switch to autoconnect the wallet for testing.
debugWallet :: Maybe WalletBrand
debugWallet = Just Yoroi -- Just Lace -- Nami -- Eternl -- Nami -- Nothing

type ContractInfoMap = Map Runtime.ContractId ContractInfo

-- On the app level we keep the previous wallet context
-- so we can reuse pieces of contract info from the previous
-- state in a safe manner.
newtype AppContractInfoMap = AppContractInfoMap
  { walletContext :: Maybe WalletContext
  , map :: ContractInfoMap
  }

policyIdsStr :: C.Value -> Array String
policyIdsStr = map C.assetIdToString <<< C.valueAssetIds

mkWithdrawalWidget :: MkComponentMBase () (WalletInfo Wallet.Api -> JSX)
mkWithdrawalWidget = do
  liftEffect $ component "WithdrawalWidget" \(WalletInfo walletInfo) -> React.do
    let
      withdrawalProps =
        { wallet: walletInfo.wallet
        , network: B.preprod
        , txOutRef: Runtime.TxOutRef
          -- { txId: Runtime.TxId "c93175feff92ddfb571f4d12b9d34ab910594dce54ad4017a5670a0b43a930f5"
          -- , txIx: 0
          -- }
          { txId: Runtime.TxId "3605db0c5ae9be7623cd4ecb04f8e99d784da35258f2952896b25f7613968b54"
          , txIx: 2
          }
        , blockfrostProjectId: B.ProjectId "preprodD9cONxVqzHYtFEL4RObOZ46y4begqNHc"
        }
    withdrawalStatus <- useWithdrawal withdrawalProps

    pure $ case withdrawalStatus of
      AwaitingWithdrawal { withdraw, lastAttemptResult } -> do
        -- | Let's dispaly a button which triggers withdrawal
        -- | and a status message.
        let
          statusMessage = case lastAttemptResult of
            Nothing -> DOOM.text "Awaiting withdrawal"
            Just (Left err) -> yamlSyntaxHighlighter (encodeJson err) {}
            Just (Right txId) -> yamlSyntaxHighlighter (encodeJson txId) {}
        DOM.div {}
          [ DOM.button
              { className: "btn btn-primary"
              , onClick: handler_ withdraw
              }
              [ DOOM.text "Withdraw" ]
          , statusMessage
          ]
      ProcessingWithdrawal msg -> DOOM.text msg

mkApp :: MkComponentMBase () (Unit -> JSX)
mkApp = do
  landingPage <- mkLandingPage
  messageBox <- liftEffect $ mkMessageBox
  messagePreview <- liftEffect $ mkMessagePreview
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  subcomponents <- do
    contractListComponent <- mkContractList
    -- eventListComponent <- mkEventList
    connectWallet <- mkConnectWallet
    splittedDeposit <- mkSplittedDeposit
    withdrawalWidget <- mkWithdrawalWidget
    pure { contractListComponent, connectWallet, messageBox, splittedDeposit, withdrawalWidget }

  (ContractWithTransactionsStream contractStream) <- asks _.contractStream

  throttledEmitter :: Subscription.Emitter (List ContractWithTransactionsEvent) <- liftEffect $
    Subscription.foldMapThrottle (List.singleton) (MinInterval $ Milliseconds 1_000.0) contractStream.emitter

  initialVersion <- liftEffect now

  walletInfoCtx <- asks _.walletInfoCtx
  msgHub@(MessageHub msgHubProps) <- asks _.msgHub

  liftEffect $ component "App" \_ -> React.do
    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    let
      walletInfoName = _.name <<< un WalletInfo <$> possibleWalletInfo

    possibleWalletInfoRef <- useStateRef walletInfoName possibleWalletInfo
    possibleWalletContext /\ setWalletContext <- useState' Nothing
    possibleWalletContextRef <- useStateRef' possibleWalletContext

    useEffectOnce do
      -- FIXME: We should restart fetchers on exception
      fiber <- launchAff $ contractStream.start
      pure $ launchAff_ $ killFiber (error "Unmounting component") fiber

    useLoopAff walletInfoName (Milliseconds 20_000.0) do
      pwi <- liftEffect $ readRef possibleWalletInfoRef
      pwc <- liftEffect $ readRef possibleWalletContextRef
      case pwi, pwc of
        Nothing, Nothing -> pure unit
        Nothing, Just _ -> do
          liftEffect $ setWalletContext Nothing
        Just (WalletInfo walletInfo), _ -> do
          let
            action = do
              walletContext <- WalletContext.walletContext cardanoMultiplatformLib walletInfo.wallet
              liftEffect $ setWalletContext walletContext
          action `catchError` \_ -> do
            -- FIXME: Report back (to the reporting backend) a wallet problem?
            traceM "ERROR during wallet context construction"
            pure unit

    configuringWallet /\ setConfiguringWallet <- useState' false
    checkingNotifications /\ setCheckingNotifications <- useState' false

    -- We are ignoring contract events for now and we update the whole contractInfo set.
    upstreamVersion <- useEmitter' initialVersion (Subscription.bindEffect (const now) throttledEmitter)
    upstreamVersionRef <- useStateRef' upstreamVersion

    -- Let's use versioning so we avoid large comparison.
    (version /\ contractMap) /\ setContractMap <- useState' (upstreamVersion /\ AppContractInfoMap { walletContext: possibleWalletContext, map: Map.empty })
    idRef <- useStateRef version contractMap

    useEffect (upstreamVersion /\ possibleWalletContext) do
      updates <- contractStream.getLiveState
      old <- readRef idRef
      newVersion <- readRef upstreamVersionRef
      let
        new = updateAppContractInfoMap old possibleWalletContext updates
        _map (AppContractInfoMap { map }) = map

        old' = _map old
        new' = _map new

        (additionsNumber :: Int) = length $ Map.additions (Map.Old old') (Map.New new')
        (deletionsNumber :: Int) = length $ Map.deletions (Map.Old old') (Map.New new')

      when (deletionsNumber > 0 || additionsNumber > 0) do
        msgHubProps.add $ Info $ DOOM.text $
          "Update: "
            <> (if deletionsNumber == 0 then "" else show deletionsNumber <> " contracts removed")
            <> (if deletionsNumber > 0 && additionsNumber > 0 then ", " else "")
            <> (if additionsNumber == 0 then "" else show additionsNumber <> " contracts discovered")
            <> "."

      setContractMap (newVersion /\ new)
      pure $ pure unit

    -- -- This causes a lot of re renders - we avoid it for now by
    -- -- enforcing manual offcanvas toggling.
    -- -- FIXME: expose the msgs emitter and use it to detect
    -- -- when message box is empty.
    -- msgs <- useContext msgHubProps.ctx
    -- useEffect (List.null msgs) do
    --   when (List.null msgs) do
    --     setCheckingNotifications false
    --   pure $ pure unit

    useAff unit $ for debugWallet \walletBrand ->
      autoConnectWallet walletBrand \walletInfo -> do
        liftEffect $ setWalletInfo $ Just walletInfo
    let
      AppContractInfoMap { map: contracts } = contractMap

    pure $ case possibleWalletInfo of
      Nothing -> landingPage { setWalletInfo: setWalletInfo <<< Just }
      _ -> provider walletInfoCtx ((/\) <$> possibleWalletInfo <*> possibleWalletContext) $
        [ DOM.nav { className: "navbar navbar-expand-sm navbar-light bg-light shadow-bottom fixed-top" } $
            DOM.div { className: "container-fluid" }
              [ DOM.a { href: "#", className: "navbar-brand p-0" }
                  [ svgImg { src: marloweLogoUrl } ]
              , DOM.div { className: "navbar-collapse justify-content-end text-end" } $
                  [ DOM.ul { className: "navbar-nav gap-2" }
                      [ DOM.li { className: "nav-item" } $
                          case possibleWalletInfo of
                            Just (WalletInfo wallet) -> link
                              { label: DOM.span { className: "h5" }
                                  [ DOOM.img { src: wallet.icon, alt: wallet.name, className: "w-1_2rem me-1" }
                                  , DOOM.span_ [ DOOM.text $ wallet.name <> " wallet" ]
                                  ]
                              , extraClassNames: "nav-link"
                              , onClick: setConfiguringWallet true
                              }
                            Nothing -> linkWithIcon
                              { icon: Icons.wallet2
                              , label: DOOM.text "Connect Wallet"
                              , extraClassNames: "nav-link"
                              , onClick: setConfiguringWallet true
                              }
                      ]
                  ]
              ]
        , DOM.div { className: "position-fixed mt-2 position-left-50 transform-translate-x--50 z-index-popover" }
            $ DOM.div { className: "container-xl" }
            $ DOM.div { className: "row" }
            $ messagePreview msgHub
        , ReactContext.consumer msgHubProps.ctx \_ ->
            pure $ offcanvas
              { onHide: setCheckingNotifications false
              , placement: Offcanvas.placement.end
              , show: checkingNotifications -- && (not $ List.null msgs)
              , scroll: false
              }
              [ DOM.div { className: "p-3 overflow-auto" } $ messageBox msgHub
              ]
        , Monoid.guard configuringWallet do
            let
              jsx = subcomponents.connectWallet
                { currentlyConnected: possibleWalletInfo
                , onWalletConnect: \result -> do
                    case result of
                      ConnectWallet.Connected walletInfo -> do
                        let
                          WalletInfo { name } = walletInfo
                        msgHubProps.add $ Success $ DOOM.text $ "Connected to " <> name
                        setWalletInfo (Just walletInfo)
                      ConnectWallet.ConnectionError _ -> pure unit
                      ConnectWallet.NoWallets -> pure unit
                    setConfiguringWallet false
                , onDismiss: setConfiguringWallet false
                , inModal: true
                }
            jsx
        , BodyLayout.component
            { title: "Withdrawal widget"
            , description: DOOM.text "Withdrawal widget"
            , content: case possibleWalletInfo of
                Just walletInfo -> subcomponents.withdrawalWidget walletInfo
                Nothing -> DOOM.text "Please connect your wallet to use this widget."
            }
        ]

updateAppContractInfoMap :: AppContractInfoMap -> Maybe WalletContext -> ContractWithTransactionsMap -> AppContractInfoMap
updateAppContractInfoMap (AppContractInfoMap { map: prev }) walletContext updates = do
  let
    walletCtx = un WalletContext <$> walletContext
    (usedAddresses :: Array String) = map bech32ToString $ fromMaybe [] $ _.usedAddresses <$> walletCtx
    (tokens :: Array String) = fromMaybe [] $ policyIdsStr <<< _.balance <$> walletCtx

    map = Map.catMaybes $ updates <#> \{ contract: { resource: contractHeader@(Runtime.ContractHeader { contractId, roleTokenMintingPolicyId, tags }), links: endpoints }, contractState, transactions } -> do
      let
        marloweInfo = do
          Runtime.ContractState contractState' <- contractState
          pure $ MarloweInfo
            { initialContract: contractState'.initialContract
            , currencySymbol: case roleTokenMintingPolicyId of
                PolicyId "" -> Nothing
                PolicyId policyId -> Just $ policyId
            , state: contractState'.state
            , currentContract: contractState'.currentContract
            , initialState: V1.emptyState -- FIXME: No initial state on the API LEVEL?
            , unclaimedPayouts: contractState'.unclaimedPayouts
            }

      let
        keepContract =
          case marloweInfo of
            Just (MarloweInfo { initialContract })
              | (not $ Array.null $ Array.intersect usedAddresses (addressesInContract initialContract))
                  || (not $ Array.null $ Array.intersect tokens (rolesInContract initialContract)) -> Just true
            Just _ -> Just false
            _ -> Nothing
        possibleAppTags = AppTags.fromRuntimeTags tags

      case contractId `Map.lookup` prev, keepContract, possibleAppTags of
        Just (ContractInfo contractInfo), Just true, Just appTags -> do
          pure $ ContractInfo $ contractInfo
            { marloweInfo = marloweInfo
            , tags = appTags
            , _runtime
                { contractHeader = contractHeader
                , transactions = transactions
                }
            }
        Nothing, Just true, Just appTags -> do
          let Runtime.ContractHeader { contractId } = contractHeader
          pure $ ContractInfo $
            { contractId
            , endpoints
            , marloweInfo
            , tags: appTags
            , _runtime: { contractHeader, transactions }
            }
        _, _, _ -> Nothing
  AppContractInfoMap { walletContext, map }
