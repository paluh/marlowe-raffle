module Component.SelectWallet where

import Prelude

import Component.Assets.Svgs as Svgs
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link, spinner)
import Component.Widgets.Form (mkSingleChoiceField)
import Component.Widgets.Form as Form
import Contrib.ChakraUI as Chakra
import Contrib.React.Svg (loadingSpinnerLogo, svgProgress)
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap)
import Data.Newtype as Newtype
import Data.String.Extra (upperCaseFirst)
import Data.Traversable (traverse)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (button, h2_, img, span_, text) as DOOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import ReactBootstrap.Modal (modal, modalBody, modalFooter, modalHeader)
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet (Wallet)
import Wallet as Wallet
import Web.HTML (window)

type Wallets = Map String Wallet

walletInfo :: Wallet -> Aff (WalletInfo Wallet)
walletInfo wallet = WalletInfo <$> ado
  name <- liftEffect (Wallet.name wallet)
  icon <- liftEffect (Wallet.icon wallet)
  isEnabled <- Wallet.isEnabled_ wallet
  apiVersion <- liftEffect (Wallet.apiVersion wallet)
  in
    { name, icon, isEnabled, apiVersion, wallet }

data Response
  = NoWallets
  | ConnectionError Error
  | Connected (WalletInfo Wallet.Api)

type Props =
  { currentlyConnected :: Maybe (WalletInfo Wallet.Api)
  , onDismiss :: Effect Unit
  , onWalletConnect :: Response -> Effect Unit
  }

mkSelectWallet :: MkComponentM (Props -> JSX)
mkSelectWallet = do
  singleChoiceField <- liftEffect mkSingleChoiceField
  -- modal <- liftEffect mkModal

  liftEffect $ component "Wallet" \{ currentlyConnected, onWalletConnect, onDismiss } -> React.do
    -- pure \{ currentlyConnected, onWalletConnect, onDismiss } -> coerceHook React.do
    possibleWallets /\ setWallets <- useState' (Nothing :: Maybe (ArrayAL 1 (WalletInfo Wallet)))
    selectedWallet /\ setSelectedWallet <- useState' $ Nothing

    useEffectOnce do
      liftEffect (Wallet.cardano =<< window) >>= case _ of
        Nothing -> pure unit
        Just cardano -> launchAff_ do
          eternl <- liftEffect (Wallet.eternl cardano) >>= traverse walletInfo
          gerowallet <- liftEffect (Wallet.gerowallet cardano) >>= traverse walletInfo
          lace <- liftEffect (Wallet.lace cardano) >>= traverse walletInfo
          nami <- liftEffect (Wallet.nami cardano) >>= traverse walletInfo
          typhon <- liftEffect (Wallet.typhon cardano) >>= traverse walletInfo
          yoroi <- liftEffect (Wallet.yoroi cardano) >>= traverse walletInfo
          case ArrayAL.fromArray (Proxy :: Proxy 1) (Array.catMaybes [ eternl, gerowallet, lace, nami, typhon, yoroi ]) of
            Nothing -> liftEffect $ onWalletConnect NoWallets
            Just wallets -> liftEffect $ do
              setWallets (Just wallets)
              setSelectedWallet $ do
                { name } <- un WalletInfo <$> currentlyConnected
                Array.find (\(WalletInfo wallet) -> wallet.name == name) (ArrayAL.toArray wallets)
      pure (pure unit)

    let
      onSelect possibleValue = do
        let
          possibleWallet = do
            value <- possibleValue
            wallets <- possibleWallets <#> ArrayAL.toArray
            Array.find (\(WalletInfo { name }) -> name == value) wallets
        case possibleWallet of
          Just selected@(WalletInfo s) ->
            if Just s.name == (_.name <<< unwrap <$> currentlyConnected) then onDismiss
            else launchAff_ do
              possibleApi <- (Just <$> Wallet.enable_ s.wallet) `catchError` \error -> do
                liftEffect $ onWalletConnect (ConnectionError error)
                pure Nothing
              case possibleApi of
                Just (walletApi :: Wallet.Api) -> do
                  let
                    selected' = Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) selected
                  liftEffect $ onWalletConnect (Connected selected')
                Nothing -> do
                  traceM $ "Error connecting wallet - no api returned"
                  -- FIXME: Error handling
                  liftEffect $ onDismiss
          Nothing -> onDismiss

    pure $ do
      let
        options = case possibleWallets of
          Nothing -> mempty
          Just wallets -> ArrayAL.toArray $ wallets <#> \(WalletInfo { icon, name }) -> do
            DOM.option { value: name }
              [ DOOM.img { src: icon, alt: name, className: "w-2rem me-2" }
              , DOOM.span_ [ DOOM.text $ upperCaseFirst name ]
              ]
      Chakra.select
        { placeholder: "Connect wallet"
        , onChange: handler targetValue onSelect
        , size: "lg"
        }
        options
      -- let
      --   { formBody, formActions } = case possibleWallets of
      --     Nothing ->
      --       { formBody: DOM.div { className: "d-flex justify-content-center" } $ loadingSpinnerLogo {}
      --       , formActions: mempty
      --       }
      --     Just wallets -> do
      --       let
      --         choices = wallets <#> \(WalletInfo { icon, name }) -> do
      --           let
      --             label = DOM.span { className: "h5" }
      --               [ DOOM.img { src: icon, alt: name, className: "w-2rem me-2" }
      --               , DOOM.span_ [ DOOM.text name ]
      --               ]
      --           -- We know that only Nami is working - should we disable all the other wallets?
      --           name /\ label /\ false

      --       { formBody: singleChoiceField
      --           { initialValue: fromMaybe "" (_.name <<< unwrap <$> selectedWallet)
      --           , onValueChange: \walletName -> do
      --               setSelectedWallet $ Array.find (\(WalletInfo wallet) -> wallet.name == walletName) (ArrayAL.toArray wallets)
      --           , type: Form.RadioButtonField choices
      --           }
      --       , formActions: DOOM.fragment
      --           if inModal then
      --             [ link { label: DOOM.text "Cancel", onClick: onDismiss, showBorders: true }
      --             , DOOM.button do
      --                 let
      --                   _name :: forall wallet. Maybe (WalletInfo wallet) -> Maybe String
      --                   _name = map $ (_.name <<< unwrap)
      --                   selectedIsConnected = _name selectedWallet == _name currentlyConnected

      --                 { type: "button"
      --                 , className: "btn btn-primary"
      --                 , onClick: handler_ onSubmit
      --                 , disabled: selectedIsConnected
      --                 , children: [ DOOM.text "Connect wallet" ]
      --                 }
      --             ]
      --           else
      --             [ DOOM.button do
      --                 let
      --                   _name :: forall wallet. Maybe (WalletInfo wallet) -> Maybe String
      --                   _name = map $ (_.name <<< unwrap)
      --                   selectedIsConnected = _name selectedWallet == _name currentlyConnected

      --                 { type: "button"
      --                 , className: "btn btn-primary mt-3"
      --                 , onClick: handler_ onSubmit
      --                 , disabled: selectedIsConnected
      --                 , children: [ DOM.p { className: "h4 font-weight-bold" } [ DOOM.text "Connect wallet" ] ]
      --                 }
      --             ]
      --       }
      -- if inModal then modal
      --   { onHide: onDismiss -- : setConfiguringWallet false
      --   -- , footer: formActions
      --   -- , body: formBody
      --   -- , title: DOOM.text "Connect wallet"
      --   , show: true
      --   }
      --   [ modalHeader {} $ DOOM.text "Choose a wallet"
      --   , modalBody {} formBody
      --   , modalFooter {} formActions
      --   ]

      -- else
      --   DOM.div { className: "card p-5 m-5" }
      --     [ DOM.p { className: "h3 font-weight-bold" } [ DOOM.text "Choose a wallet" ]
      --     , DOM.span { className: "h5 text-muted" } [ DOOM.text "Please select a wallet to deploy a contract" ]
      --     , formBody
      --     , formActions
      --     ]
