module Component.LandingPage where

import Prelude

import Component.ConnectWallet (mkConnectWallet)
import Component.ConnectWallet as ConnectWallet
import Component.Types (ContractInfo, MkComponentMBase, WalletInfo)
import Contrib.React.Svg (SvgUrl(..), svgImg)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import JS.Unsafe.Stringify (unsafeStringify)
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, useState')
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (WalletContext)

type ContractInfoMap = Map Runtime.ContractId ContractInfo

-- On the app level we keep the previous wallet context
-- so we can reuse pieces of contract info from the previous
-- state in a safe manner.
newtype AppContractInfoMap = AppContractInfoMap
  { walletContext :: Maybe WalletContext
  , map :: ContractInfoMap
  }

type Props =
  { setWalletInfo :: WalletInfo Wallet.Api -> Effect Unit
  }

data ConnectionErrors
  = NoWallets
  | ConnectionError Error

mkLandingPage :: MkComponentMBase () (Props -> JSX)
mkLandingPage = do
  connectWallet <- mkConnectWallet
  liftEffect $ component "LandingPage" \{ setWalletInfo } -> React.do
    possibleErrors /\ setErrors <- useState' Nothing
    pure $ DOM.div {} $
      [ DOM.nav { className: "navbar navbar-expand-sm navbar-light bg-light shadow-bottom fix-top" } $
          DOM.div { className: "container-fluid" }
            [ DOM.a { href: "#", className: "navbar-brand" }
                [ svgImg { src: marloweLogoUrl } ]
            ]
      , DOM.div { className: "container-fluid" }
          $ DOM.div { className: "row justify-content-center" }
          $ DOM.div { className: "col-xl-5 col-lg-8 col-12" }
              [ case possibleErrors of
                  -- FIXME: Should we present errors on the connectWallet level?
                  Just NoWallets -> DOOM.text "NO WALLETS?"
                  Just (ConnectionError err) -> DOOM.text $ unsafeStringify err
                  Nothing -> connectWallet
                    { currentlyConnected: Nothing
                    , onWalletConnect: case _ of
                        ConnectWallet.Connected walletInfo -> setWalletInfo walletInfo
                        ConnectWallet.NoWallets -> setErrors $ Just NoWallets
                        ConnectWallet.ConnectionError err -> setErrors $ Just $ ConnectionError err
                    , onDismiss: pure unit
                    , inModal: false
                    }
              ]
      ]

marloweLogoUrl :: SvgUrl
marloweLogoUrl = SvgUrl "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMTYwIiBoZWlnaHQ9IjQyIiB2aWV3Qm94PSIwIDAgMTYwIDQyIiBmaWxsPSJub25lIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGRhdGEtbG9nbz0idHJ1ZSI+PGcgY2xpcC1wYXRoPSJ1cmwoI2xvZ29fX2EpIj48cGF0aCBkPSJNNjkuNzEgMjIuMzUydjkuMTJoLTIuOXYtOC43MzRjMC0yLjgzNi0xLjg2OS0zLjc3Mi0zLjQ0OS0zLjc3Mi0xLjU4IDAtMy40MTcuOTM2LTMuNDE3IDMuNzcydjguNzM1aC0yLjkwMXYtOC43MzVjMC0yLjgzNi0xLjg2OS0zLjc3Mi0zLjQ4LTMuNzcyLTEuNjEgMC0zLjQxNy45MzYtMy40MTcgMy43NzJ2OC43MzVoLTIuOTAxVjE2LjY0NmgyLjkwMXYxLjc3MmMxLjA2My0xLjM4NyAyLjY3NC0yIDQuMjU0LTIgMS44NjggMCAzLjgwMy43MSA0LjggMi42MTMgMS4xMy0xLjkzNSAzLjE2LTIuNjEzIDQuOTk4LTIuNjEzIDIuNzA4IDAgNS41MSAxLjQ4NCA1LjUxIDUuOTNsLjAwMy4wMDNaTTg3LjQ2OSAxNi42NXYxNC44MjZoLTIuOTAxdi0yLjI4OWMtMS4wOTUgMS41OC0yLjgwNSAyLjU0Ny01LjA5NCAyLjU0Ny00LjE4OCAwLTcuMTg1LTMuMjU2LTcuMTg1LTcuNjcxIDAtNC40MTUgMi45OTctNy42NCA3LjE4NS03LjY0IDIuMjg5IDAgMy45OTYuOTY3IDUuMDk0IDIuNTEydi0yLjI4OGgyLjkwMXYuMDAzWm0tMi45MDEgNy40MTNjMC0yLjg2Ny0xLjg2OS00Ljg5Ny00LjcwNS00Ljg5Ny0yLjgzNSAwLTQuNzM5IDIuMDMtNC43MzkgNC44OTcgMCAyLjg2NyAxLjg3IDQuOTMyIDQuNzQgNC45MzJzNC43MDQtMi4wOTYgNC43MDQtNC45MzJaTTk4LjY1IDE2LjQyM3YyLjc0Yy0yIC4wMy01LjIyLjktNS4yMiA0LjcwNHY3LjYwNWgtMi45MDJWMTYuNjQ2aDIuOTAydjIuNzRjMS4xNi0yLjAzIDMuMTU5LTIuOTMyIDUuMjItMi45Njd2LjAwNFpNMTAwLjc0NiAzMS40NzZWMTAuNTI0aDIuOTMybC0uMDMxIDIwLjk0OWgtMi45MDF2LjAwM1pNMTA1Ljk1MyAyNC4wNjNjMC00LjQ4IDMuMTI1LTcuNzAyIDcuNjA2LTcuNzAyczcuNTQgMy4yMjQgNy41NCA3LjcwMmMwIDQuNDc3LTMuMDk0IDcuNzAyLTcuNTQgNy43MDJzLTcuNjA2LTMuMjg3LTcuNjA2LTcuNzAyWm0xMi4yOCAwYzAtMi44MzYtMS44NjktNC44OTctNC42NzQtNC44OTctMi44MDUgMC00LjcwNSAyLjA2MS00LjcwNSA0Ljg5NyAwIDIuODM2IDEuODY5IDQuOTMyIDQuNzA1IDQuOTMyIDIuODM2IDAgNC42NzQtMi4xMjcgNC42NzQtNC45MzJaTTE0NC4yODEgMTYuNjVsLTQuOTMxIDE0LjgyNmgtMi44NjdsLTMuNDgtMTAuMjQ5LTMuNTEzIDEwLjI0OWgtMi44NjdsLTQuODY2LTE0LjgyNmgzLjAyOGwzLjQ0OCAxMC43MzQgMy43MzgtMTAuNzM0aDIuMTI3bDMuNjc1IDEwLjczNCAzLjQ4LTEwLjczNGgzLjAyOFpNMTU5LjI0NSAyNC4wNjN2LjgzNmgtMTEuNjY3Yy4yODkgMi42NDMgMi4xNTggNC4zNSA0LjczOSA0LjM1IDIuMTI3IDAgMy40NzktLjk2NyA0LjE4OC0ybDIuMDMxIDEuNDVjLTEuMjkxIDEuODY4LTMuNTE0IDMuMDYzLTYuMjE5IDMuMDYzLTQuNTEyIDAtNy41NC0zLjI4Ny03LjU0LTcuNzAzIDAtNC40MTUgMi45MDEtNy42NyA3LjM0Ny03LjY3IDQuNDQ3IDAgNy4xMjQgMy4yNTUgNy4xMjQgNy42N2wtLjAwMy4wMDRaTTE0Ny41NzggMjNoOC45NThjLS4zNTQtMi40ODItMS45MzQtNC4yODktNC40MTUtNC4yODlzLTQuMTg4IDEuNzA3LTQuNTQzIDQuMjg5WiIgZmlsbD0idmFyKC0tdGV4dC1jb2xvciwjMUMxQzFDKSI+PC9wYXRoPjxwYXRoIGQ9Ik0yMS4yMTcgMS43NDggMTguMTg1IDAgMCAxMC41djIxbC42OTkuNDAzIDIuMzMzIDEuMzQ1TDE4LjE4NSA0Mmw2LjA2NC0zLjV2LTIxbC0xMi4xMjUtNy0zLjAzMiAxLjc0OCAxMi4xMjUgN3YxNy41TDE4LjE4NSAzOC41IDMuMDMyIDI5Ljc0OHYtMTcuNUwxOC4xODUgMy41bDE1LjE1NiA4Ljc0OHYyMWwzLjAzMi0xLjc0OHYtMjFMMjEuMjE3IDEuNzQ4WiIgZmlsbD0iIzUxMUNGNyI+PC9wYXRoPjxwYXRoIGQ9Im0xNS4xNTYgOC43NDggMTIuMTIxIDd2MjFMMzAuMzEgMzVWMTRMMTguMTg1IDdsLTMuMDI5IDEuNzQ4WiIgZmlsbD0iIzUxMUNGNyI+PC9wYXRoPjwvZz48ZGVmcz48Y2xpcFBhdGggaWQ9ImxvZ29fX2EiPjxwYXRoIGZpbGw9IiNmZmYiIGQ9Ik0wIDBoMTU5LjI0NXY0MkgweiI+PC9wYXRoPjwvY2xpcFBhdGg+PC9kZWZzPjwvc3ZnPg=="
