module Component.App where

import Prelude

import App.Pages.SplittedDeposit (mkSplittedDeposit)
import CardanoMultiplatformLib (bech32ToString)
import Component.CNCAla.CNCSpinner (cncSpinner)
import Component.CNCAla.DesktopLogosRaffle (desktopLogosRaffle)
import Component.CNCAla.Logo as CNCAla.Logo
import Component.CNCAla.MobileLogosRaffle (mobileLogosRaffle)
import Component.CNCAla.SocialMedia (socialMedia)
import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ContractList (mkContractList)
import Component.InputHelper (addressesInContract, rolesInContract)
import Component.MessageHub (mkMessageBox)
import Component.SelectWallet (Response(..), mkSelectWallet)
import Component.Types (ContractInfo(..), MkComponentMBase, WalletInfo(..))
import Component.Types.AppTags as AppTags
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.UseWithdrawal (HookStatus(..), HookError, useWithdrawal)
import Component.UseWithdrawal.Blockfrost as B
import Contrib.CNCAla.Animation.EaseInWithSlidingAnimation (easeInWithSlidingAnimation)
import Contrib.Cardano as C
import Contrib.ChakraUI (chakraProvider, useColorModeValue)
import Contrib.ChakraUI as Chakra
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (catMaybes, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Newtype as Newtype
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign.Internal.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics (emptyState) as V1
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsMap, ContractWithTransactionsStream(..))
import Marlowe.Runtime.Web.Types (PolicyId(..), TxId(..))
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX)
import React.Basic.DOM (img, text) as DOOM
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (component, readRef, useContext, useState')
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Record as Record
import Type.Prelude (Proxy(..))
import Utils.React.Basic.Hooks (useLoopAff, useStateRef, useStateRef')
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
debugWallet = Nothing -- Just Nami -- Just Lace -- Nami -- Eternl -- Nami -- Nothing

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

mkApp :: MkComponentMBase () (Unit -> JSX)
mkApp = do
  selectWallet <- mkSelectWallet
  messageBox <- liftEffect $ mkMessageBox
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  subcomponents <- do
    contractListComponent <- mkContractList
    -- eventListComponent <- mkEventList
    connectWallet <- mkConnectWallet
    splittedDeposit <- mkSplittedDeposit
    -- withdrawalWidget <- mkWithdrawalWidget
    pure { contractListComponent, connectWallet, messageBox, splittedDeposit } --, withdrawalWidget }

  (ContractWithTransactionsStream contractStream) <- asks _.contractStream

  appCtxCtx <- asks _.appCtx
  -- walletInfoCtx <- asks _.walletInfoCtx

  raffleModal <- mkRaffleModal

  liftEffect $ component "App" \_ -> React.do
    appCtx <- useContext appCtxCtx

    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    let
      walletInfoName = _.name <<< un WalletInfo <$> possibleWalletInfo

    possibleWalletInfoRef <- useStateRef walletInfoName possibleWalletInfo
    possibleWalletContext /\ setWalletContext <- useState' Nothing
    possibleWalletContextRef <- useStateRef' possibleWalletContext

    -- useEffectOnce do
    --   -- FIXME: We should restart fetchers on exception
    --   fiber <- launchAff $ contractStream.start
    --   pure $ launchAff_ $ killFiber (error "Unmounting component") fiber

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

    -- configuringWallet /\ setConfiguringWallet <- useState' false
    -- checkingNotifications /\ setCheckingNotifications <- useState' false

    -- We are ignoring contract events for now and we update the whole contractInfo set.
    -- upstreamVersion <- useEmitter' initialVersion (Subscription.bindEffect (const now) throttledEmitter)
    -- upstreamVersionRef <- useStateRef' upstreamVersion

    -- -- Let's use versioning so we avoid large comparison.
    -- (version /\ contractMap) /\ setContractMap <- useState' (upstreamVersion /\ AppContractInfoMap { walletContext: possibleWalletContext, map: Map.empty })
    -- idRef <- useStateRef version contractMap

    -- useEffect (upstreamVersion /\ possibleWalletContext) do
    --   updates <- contractStream.getLiveState
    --   old <- readRef idRef
    --   newVersion <- readRef upstreamVersionRef
    --   let
    --     new = updateAppContractInfoMap old possibleWalletContext updates
    --     _map (AppContractInfoMap { map }) = map

    --     old' = _map old
    --     new' = _map new

    --     (additionsNumber :: Int) = length $ Map.additions (Map.Old old') (Map.New new')
    --     (deletionsNumber :: Int) = length $ Map.deletions (Map.Old old') (Map.New new')

    --   when (deletionsNumber > 0 || additionsNumber > 0) do
    --     msgHubProps.add $ Info $ DOOM.text $
    --       "Update: "
    --         <> (if deletionsNumber == 0 then "" else show deletionsNumber <> " contracts removed")
    --         <> (if deletionsNumber > 0 && additionsNumber > 0 then ", " else "")
    --         <> (if additionsNumber == 0 then "" else show additionsNumber <> " contracts discovered")
    --         <> "."

    --   setContractMap (newVersion /\ new)
    --   pure $ pure unit

    -- -- This causes a lot of re renders - we avoid it for now by
    -- -- enforcing manual offcanvas toggling.
    -- -- FIXME: expose the msgs emitter and use it to detect
    -- -- when message box is empty.
    -- msgs <- useContext msgHubProps.ctx
    -- useEffect (List.null msgs) do
    --   when (List.null msgs) do
    --     setCheckingNotifications false
    --   pure $ pure unit
    -- let
    --   AppContractInfoMap { map: contracts } = contractMap
    useAff unit $ for debugWallet \walletBrand ->
      autoConnectWallet walletBrand \walletInfo -> do
        liftEffect $ setWalletInfo $ Just walletInfo

    pure $ chakraProvider { theme: Chakra.theme, cssVarsRoot: "body" } $ Array.singleton
      $ Chakra.container { alignSelf: "center", w: "100%" }
      $ Array.singleton
      $
        Chakra.stack { spacing: "0", pt: "16", align: "center" }
          [ easeInWithSlidingAnimation {} $
              [ Chakra.heading { size: if appCtx.isDesktop then "lg" else "md" }
                  [ DOOM.text "CNC Ala Raffle's Claiming Portal" ]
              , Chakra.stack { py: "10" }
                  [ Chakra.text
                      { overflowWrap: "break-word"
                      , w: if appCtx.isDesktop then "2xl" else "100%"
                      , fontSize: "xl"
                      , align: "center"
                      }
                      [ DOOM.text "If you received the winning NFT from our Marlowe smart contract," ]
                  , Chakra.text
                      { overflowWrap: "break-word"
                      , w: if appCtx.isDesktop then "2xl" else "100%"
                      , fontSize: "xl"
                      , align: "center"
                      }
                      [ DOOM.text "please connect your wallet to claim your prize! 🌳" ]
                  ]
              ]
          , easeInWithSlidingAnimation {}
              [ Chakra.center {} $ Array.singleton
                $ selectWallet
                    { currentlyConnected: Nothing
                    , onWalletConnect: case _ of
                        Connected walletInfo -> setWalletInfo $ Just walletInfo
                        _ -> pure unit
                    , onDismiss: pure unit
                    }
              ]
          -- <EaseInWithSlidingAnimation initY={20} delay={0.5}>
          --   <Image
          --     src="/assets/images/winner_nfts.png"
          --     alt={"Winner NFTs"}
          --     width={800}
          --     height={600}
          --   />
          -- </EaseInWithSlidingAnimation>
          , easeInWithSlidingAnimation {} $
              [ DOOM.img
                  { src: "./winner_nfts.png"
                  , alt: "Winner NFTs"
                  , width: "800"
                  , height: "600"
                  }
              ]

          -- <EaseInWithSlidingAnimation>
          --   <Stack
          --     spacing="4"
          --     direction={isDesktop ? "row" : "column"}
          --     align={"center"}
          --     pb={16}
          --   >
          --     {isDesktop ? (
          --       <DesktopLogosRaffle />
          --     ) : (
          --       <MobileLogosRaffle />
          --     )}
          --   </Stack>
          -- </EaseInWithSlidingAnimation>
          , easeInWithSlidingAnimation {} $
              [ Chakra.stack
                  { spacing: "4"
                  , direction: if appCtx.isDesktop then "row" else "column"
                  , align: "center"
                  , pb: "16"
                  }
                  [ if appCtx.isDesktop then desktopLogosRaffle {}
                    else mobileLogosRaffle {}
                  ]
              ]
          , case possibleWalletInfo of
              Just walletInfo -> raffleModal
                { isOpen: true
                , onClose: handler_ $ setWalletInfo Nothing
                , wallet: walletInfo
                }
              _ -> mempty
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

data Prize
  = FirstPrize
  | SecondPrize
  | ThirdPrize

type ModalContentProps =
  { firstPrizeStatus :: HookStatus
  , secondPrizeStatus :: HookStatus
  , thirdPrizeStatus :: HookStatus
  }

data StatusSummary
  = Checking
  | TotalLoser
  | Winner
    { prize :: Prize
    , trigger :: Effect Unit
    , status :: HookStatus
    }
  | Withdrawing
    { prize :: Prize
    , status :: HookStatus
    }
  | FinalWithdrawalFailed
    { prize :: Prize
    , error :: HookError
    }
  | FinalWithdrawalSucceeded
    { prize :: Prize
    , txId :: TxId
    }
  | Failure
    { failure :: HookStatus
    }

summarize :: ModalContentProps -> StatusSummary
summarize props = case props.firstPrizeStatus, props.secondPrizeStatus, props.thirdPrizeStatus of
  AwaitingWithdrawalTrigger trigger, _, _ -> Winner { prize: FirstPrize, trigger, status: props.firstPrizeStatus }
  _, AwaitingWithdrawalTrigger trigger, _ -> Winner { prize: SecondPrize, trigger, status: props.secondPrizeStatus }
  _, _, AwaitingWithdrawalTrigger trigger -> Winner { prize: ThirdPrize, trigger, status: props.thirdPrizeStatus }
  ProcessingWithdrawal _, _, _ -> Withdrawing { prize: FirstPrize, status: props.firstPrizeStatus }
  _, ProcessingWithdrawal _, _ -> Withdrawing { prize: SecondPrize, status: props.secondPrizeStatus }
  _, _, ProcessingWithdrawal _ -> Withdrawing { prize: ThirdPrize, status: props.thirdPrizeStatus }
  WithdrawalFailed failure, _, _ -> FinalWithdrawalFailed { prize: FirstPrize, error: failure.error }
  _, WithdrawalFailed failure, _ -> FinalWithdrawalFailed { prize: SecondPrize, error: failure.error }
  _, _, WithdrawalFailed failure -> FinalWithdrawalFailed { prize: ThirdPrize, error: failure.error }
  WithdrawalSucceeded txId, _, _ -> FinalWithdrawalSucceeded { prize: FirstPrize, txId }
  _, WithdrawalSucceeded txId, _ -> FinalWithdrawalSucceeded { prize: SecondPrize, txId }
  _, _, WithdrawalSucceeded txId -> FinalWithdrawalSucceeded { prize: ThirdPrize, txId }
  InitializationFailed _, InitializationFailed _, InitializationFailed _ -> TotalLoser
  FatalError _, _, _ -> Failure { failure: props.firstPrizeStatus }
  _, FatalError _, _ -> Failure { failure: props.secondPrizeStatus }
  _, _, FatalError _ -> Failure { failure: props.thirdPrizeStatus }
  _, _, _ -> Checking

getModalContent :: ModalContentProps -> JSX
getModalContent props = do
  let
    animationKeyframes = "keyframes{ 0% { opacity: 0; } 50% { opacity: 1; } 100% { opacity: 0; } }"
    animation = animationKeyframes <> " 2.5s ease-in-out infinite"
    summary = summarize props

  case summary of
    Checking -> do
      Chakra.stack { py: "14" }
        [ cncSpinner {}
        , Chakra.text { textAlign: "center", animation }
            [ DOOM.text "Looking for a winner NFT in your wallet." ]
        ]
    TotalLoser -> do
      Chakra.stack {}
        [ Chakra.heading { as: "h3", fontSize: "3xl", textAlign: "center", size: "sm" }
            [ DOOM.text "Better luck next time!" ]
        , Chakra.text { textAlign: "justify" }
            [ DOOM.text "Unfortunately, we couldn't find any winner NFT in your wallet. Thank you so much for participating and for helping us boost our impact in Madagascar!" ]
        , Chakra.link { alignSelf: "center", justifyContent: "center", href: "https://climateneutralcardano.org/", _hover: { bgColor: "green" } }
            [ Chakra.button
                { w: "2xs"
                , alignSelf: "center"
                , bgColor: "olivedrab"
                , borderRadius: "20px"
                }
                [ DOOM.text "Discover the CNC Ala" ]
            ]
        ]
    Winner { prize, trigger } -> do
      let
        congratulations = case prize of
          FirstPrize -> "Congratulations, lucky winner!"
          SecondPrize -> "Congratulations, you won the second prize!"
          ThirdPrize -> "Congratulations, you won the third prize!"
      Chakra.stack {}
        [ Chakra.heading { as: "h3", fontSize: "3xl", textAlign: "center", size: "sm" }
            [ DOOM.text congratulations ]
        , Chakra.text { textAlign: "justify" }
            [ DOOM.text "Your prize is a few clicks away. Thank you so much for participating and for helping us boost our impact in Madagascar!" ]
        , Chakra.button
            { w: "2xs"
            , alignSelf: "center"
            , bgColor: "olivedrab"
            , borderRadius: "20px"
            , justifySelf: "center"
            , onClick: handler_ trigger
            , _hover: { bgColor: "green" }
            }
            [ DOOM.text "Claim Prize" ]
        ]
    Withdrawing { prize } -> do
      Chakra.stack {}
        [ cncSpinner {}
        , Chakra.text { textAlign: "center", animation }
            [ DOOM.text $ "Processing " <> case prize of
                FirstPrize -> "first"
                SecondPrize -> "second"
                ThirdPrize -> "third"
                <> " prize withdrawal."
            ]
        ]
    FinalWithdrawalFailed { error } -> do
      Chakra.stack {}
        [ Chakra.heading { as: "h3", fontSize: "3xl", textAlign: "center", size: "sm" }
            [ DOOM.text "Something went wrong 💥"
            , DOOM.text $ unsafeStringify error
            ]
        , Chakra.text { textAlign: "justify" }
            [ DOOM.text "Please try again or get in touch with the members of CNC on Twitter or Discord." ]
        ]
    FinalWithdrawalSucceeded { txId } -> do
      Chakra.stack { align: "center", spacing: "6" }
        [ Chakra.heading { as: "h3", fontSize: "3xl", textAlign: "center", size: "sm" }
            [ DOOM.text "Success! 🌳" ]
        , Chakra.text { textAlign: "justify" }
            [ DOOM.text "You should see your prize on your wallet soon. You can check the transaction here:" ]
        , do
            let
              TxId txIdStr = txId
              href = if useMainnet == UseMainnet true
                then "https://cardanoscan.io/transaction/" <> txIdStr
                else "https://preprod.cardanoscan.io/transaction/" <> txIdStr

            Chakra.link { href, target: "_blank" }
              [ Chakra.text { textAlign: "justify" } [ DOOM.text txIdStr ] ]
        ]
    Failure { failure } -> do
      Chakra.stack {}
        [ Chakra.heading { as: "h3", fontSize: "3xl", textAlign: "center", size: "sm" }
            [ DOOM.text "Something went wrong 💥 and we were not able to even check if you won a prize."
            , DOOM.text $ unsafeStringify failure
            ]
        , Chakra.text { textAlign: "justify" }
            [ DOOM.text "Please try again or get in touch with the members of CNC on Twitter or Discord." ]
        ]

newtype UseMainnet = UseMainnet Boolean
derive newtype instance Eq UseMainnet
derive newtype instance Show UseMainnet

useMainnet :: UseMainnet
useMainnet = UseMainnet true


mkRaffleModal :: MkComponentMBase () ({ wallet :: WalletInfo Wallet.Api, isOpen :: Boolean, onClose :: EventHandler } -> JSX)
mkRaffleModal = do
  appCtxCtx <- asks _.appCtx
  traceM $ "useMainnet:" <> show useMainnet
  liftEffect $ component "App" \{ isOpen, onClose, wallet: WalletInfo walletInfo } -> React.do
    let
      withdrawalProps txOutRef = if useMainnet == UseMainnet true
        then
          { wallet: walletInfo.wallet
          , network: B.mainnet
          , txOutRef
          , blockfrostProjectId: B.ProjectId "mainnetw05PaBKTzXiuXujWPlKJHo9Jvvk5SmyL"
          }
        else
          { wallet: walletInfo.wallet
          , network: B.preprod
          , txOutRef
          , blockfrostProjectId: B.ProjectId "preprodD9cONxVqzHYtFEL4RObOZ46y4begqNHc"
          }

      { firstPrizeWithdrawalProps, secondPrizeWithdrawalProps, thirdPrizeWithdrawalProps } = if useMainnet == UseMainnet true
        then
          { firstPrizeWithdrawalProps: withdrawalProps $ Runtime.TxOutRef
              { txId: Runtime.TxId "8d239e97fa5b7e8cee00d7f160aa4f51adaef17386f883de4d3d9e391bbf7bfc"
              , txIx: 1
              }
          , secondPrizeWithdrawalProps: withdrawalProps $ Runtime.TxOutRef
              { txId: Runtime.TxId "6c01c234aa14bf292c41875c0437cc8657b13570b1b349b64578fd0a212710ab"
              , txIx: 1
              }
          , thirdPrizeWithdrawalProps: withdrawalProps $ Runtime.TxOutRef
              { txId: Runtime.TxId "143fe58ddd63197f7a7b1f48d6c2a616ce8955873c9c633ea0d39b8a81fef97f"
              , txIx: 1
              }
          }
        else
          { firstPrizeWithdrawalProps: withdrawalProps $ Runtime.TxOutRef
              { txId: Runtime.TxId "11120a47cfccb289df49e0c4f357257139fb90b93a7bd4b66fac8994df95c920"
              , txIx: 0
              }
          , secondPrizeWithdrawalProps: withdrawalProps $ Runtime.TxOutRef
              { txId: Runtime.TxId "ce4e76d193271ef43e54353331d1cfd26d54594385a6abff3290a63bae4cef5f"
              , txIx: 0
              }
          , thirdPrizeWithdrawalProps: withdrawalProps $ Runtime.TxOutRef
              { txId: Runtime.TxId "a9352a7a6d0b7ce6b1c1655f8fd22c70c9404414f0b0c682f8457a21efc3596d"
              , txIx: 0
              }
          }

    { status: firstPrizeStatus } <- useWithdrawal firstPrizeWithdrawalProps
    { status: secondPrizeStatus } <- useWithdrawal secondPrizeWithdrawalProps
    { status: thirdPrizeStatus } <- useWithdrawal thirdPrizeWithdrawalProps

    let
      prizeModalProps = { firstPrizeStatus, secondPrizeStatus, thirdPrizeStatus }

    appCtx <- useContext appCtxCtx
    backgroundColor <- useColorModeValue "gray.50" "gray.800"

    pure $
      Chakra.modal
        { isOpen: isOpen
        , onClose: onClose
        , size: if appCtx.isDesktop then "2xl" else "xs"
        , motionPreset: "slideInBottom"
        , isCentered: true
        }
        [ Chakra.modalOverlay { bgColor: "blackAlpha.800" }
        , Chakra.modalContent
            { bgColor: backgroundColor
            , boxShadow: "0 0 7px 1px #48BB78"
            , rounded: "xl"
            , px: "4"
            }
            [ Chakra.modalHeader {}
                [ Chakra.hStack {}
                    [ Chakra.link
                        { href: "https://climateneutralcardano.org"
                        , boxShadow: "none"
                        , border: "none"
                        , _focus:
                            { boxShadow: "none"
                            , border: "none"
                            }
                        , ml: "-4"
                        }
                        [ CNCAla.Logo.logo {} ]
                    ]
                ]
            , Chakra.modalCloseButton { _focus: { boxShadow: "none" } }
            , Chakra.modalBody { minH: "2xs" }
                [ Chakra.stack
                    { align: "center", spacing: "6", py: "8" }
                    [ getModalContent prizeModalProps ]
                ]
            , Chakra.modalFooter { textAlign: "center", alignSelf: "center" }
                [ Chakra.stack {}
                    [ Chakra.text { fontWeight: "light", fontSize: "sm" }
                        [ DOOM.text "Join our community" ]
                    , socialMedia {}
                    ]
                ]
            ]
        ]

--    <Modal
--      isOpen={isOpen}
--      onClose={onClose}
--      size={isDesktop ? "2xl" : "xs"}
--      motionPreset="slideInBottom"
--      isCentered
--    >
--      <ModalOverlay bg={"blackAlpha.800"} />
--      <ModalContent
--        bgColor={backgroundColor}
--        boxShadow={`0 0 7px 1px #48BB78`}
--        rounded="xl"
--        px={4}
--      >
--        <ModalHeader>
--          <HStack>
--            <Link
--              href="https://climateneutralcardano.org"
--              boxShadow="none"
--              border="none"
--              _focus={{ boxShadow: "none", border: "none" }}
--              ml={-4}
--            >
--              <Logo size={32} ml={-4} />
--            </Link>
--          </HStack>
--        </ModalHeader>
--        <ModalCloseButton _focus={{ boxShadow: "none" }} />
--        <ModalBody minH="2xs">
--          <Stack align="center" spacing={6} py="8">
--            {getModalContent()}
--          </Stack>
--        </ModalBody>
--        <ModalFooter textAlign={"center"} alignSelf="center">
--          <Stack>
--            <Text fontWeight={"light"} fontSize={"sm"}>
--              Join our community
--            </Text>
--            <SocialMedia />
--          </Stack>
--        </ModalFooter>
--      </ModalContent>
--    </Modal>
--  );
-- }

-- pure $ chakraProvider $ Array.singleton $ case possibleWalletInfo of
--   Nothing -> landingPage { setWalletInfo: setWalletInfo <<< Just }
--   _ -> provider walletInfoCtx ((/\) <$> possibleWalletInfo <*> possibleWalletContext) $
--     [ DOM.nav { className: "navbar navbar-expand-sm navbar-light bg-light shadow-bottom fixed-top" } $
--         DOM.div { className: "container-fluid" }
--           [ DOM.a { href: "#", className: "navbar-brand p-0" }
--               [ svgImg { src: marloweLogoUrl } ]
--           , DOM.div { className: "navbar-collapse justify-content-end text-end" } $
--               [ DOM.ul { className: "navbar-nav gap-2" }
--                   [ DOM.li { className: "nav-item" } $
--                       case possibleWalletInfo of
--                         Just (WalletInfo wallet) -> link
--                           { label: DOM.span { className: "h5" }
--                               [ DOOM.img { src: wallet.icon, alt: wallet.name, className: "w-1_2rem me-1" }
--                               , DOOM.span_ [ DOOM.text $ wallet.name <> " wallet" ]
--                               ]
--                           , extraClassNames: "nav-link"
--                           , onClick: setConfiguringWallet true
--                           }
--                         Nothing -> linkWithIcon
--                           { icon: Icons.wallet2
--                           , label: DOOM.text "Connect Wallet"
--                           , extraClassNames: "nav-link"
--                           , onClick: setConfiguringWallet true
--                           }
--                   ]
--               ]
--           ]
--     , DOM.div { className: "position-fixed mt-2 position-left-50 transform-translate-x--50 z-index-popover" }
--         $ DOM.div { className: "container-xl" }
--         $ DOM.div { className: "row" }
--         $ messagePreview msgHub
--     , ReactContext.consumer msgHubProps.ctx \_ ->
--         pure $ offcanvas
--           { onHide: setCheckingNotifications false
--           , placement: Offcanvas.placement.end
--           , show: checkingNotifications -- && (not $ List.null msgs)
--           , scroll: false
--           }
--           [ DOM.div { className: "p-3 overflow-auto" } $ messageBox msgHub
--           ]
--     , Monoid.guard configuringWallet do
--         let
--           jsx = subcomponents.connectWallet
--             { currentlyConnected: possibleWalletInfo
--             , onWalletConnect: \result -> do
--                 case result of
--                   ConnectWallet.Connected walletInfo -> do
--                     let
--                       WalletInfo { name } = walletInfo
--                     msgHubProps.add $ Success $ DOOM.text $ "Connected to " <> name
--                     setWalletInfo (Just walletInfo)
--                   ConnectWallet.ConnectionError _ -> pure unit
--                   ConnectWallet.NoWallets -> pure unit
--                 setConfiguringWallet false
--             , onDismiss: setConfiguringWallet false
--             , inModal: true
--             }
--         jsx
--     , BodyLayout.component
--         { title: "Withdrawal widget"
--         , description: DOOM.text "Withdrawal widget"
--         , content: case possibleWalletInfo of
--             Just walletInfo -> subcomponents.withdrawalWidget walletInfo
--             Nothing -> DOOM.text "Please connect your wallet to use this widget."
--         }
--     ]
