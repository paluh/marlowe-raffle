module Backend.Api where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Isomers (Api)
import Isomers as Isomers
import Isomers.Node.Server (serve) as Isomers.Node.Server
import Isomers.Request.Accum as Request.Accum
import Isomers.Response.Duplex as Response.Raw.Duplexes
import Isomers.Server as Isomers.Server
import Isomers.Spec (decompact) as Isomers.Spec
import Isomers.Spec as Spec
import Node.Process (exit, onSignal)
import React.Basic.Hooks ((/\))

type HomeApi = Isomers.HTTP''' {} String

api :: Api
  ( "" :: HomeApi
  , "test." :: HomeApi
  , "test.test" :: HomeApi
  )
api = Spec.compact $ Spec.flatten $ Spec.foldSpec
  { "": Request.Accum.pass /\ Response.Raw.Duplexes.string
  , "test":
    { "" : Request.Accum.pass /\ Response.Raw.Duplexes.string
    , "test": Request.Accum.pass /\ Response.Raw.Duplexes.string
    }
  }

-- Can we derive the type?
type BackendHandlersRow =
  ( "" :: {} -> Aff String
  , "test." :: {} -> Aff String
  , "test.test" :: {} -> Aff String
  )

handlers :: { | BackendHandlersRow }
handlers =
  { "": \{} -> pure "Hello, world!"
  , "test.": \{} -> pure "Test path"
  , "test.test": \{} -> pure "Test/test path"
  }

server :: Aff (Effect Unit)
server = do
  -- config@(Config cfg) ← Backend.Web.Config.config
  -- handlers <- mkHandlers
  let
    cfg =
      { http:
        { serve:
          { ip: "127.0.0.1"
          , port: 8080
          }
        , public:
          { scheme: "http"
          }
        }
      }
    hostInfo = { hostName: cfg.http.serve.ip, port: cfg.http.serve.port, scheme: cfg.http.public.scheme }
    -- client = Api.client hostInfo
    router = Isomers.Server.router (Isomers.Spec.decompact api) handlers

  onClose ← liftEffect $ Isomers.Node.Server.serve
    router
    identity
    { hostname: hostInfo.hostName, port: hostInfo.port, backlog: Nothing }
    (log $ "TEST") -- show hostInfo)
  pure $ onClose (log "Closed")

main :: Effect Unit
main = launchAff_ do
  closeServer <- server
  liftEffect $ onSignal SIGINT do
    log "Closing servers..."
    closeServer
    exit 0


-- type ClientRow r = Admin.ClientRow + Printer.ClientRow + r
-- 
-- type Client = { | ClientRow () }
-- 
-- client
--   :: Isomers.Client.Fetch.HostInfo
--   -> Client
-- client hostInfo = do
--   let
--     printerClient = Printer.client hostInfo
--     adminClient = Admin.client hostInfo
-- 
--   printerClient
--     `Record.merge`
--     adminClient
-- 
-- 
-- client :: HostInfo -> { | ClientRow () }
-- client hostInfo = do
--   let
--     fetch = Fetch.fetch hostInfo
--     Spec.Spec { request: reqDpl, response: resDpls } = Spec.decompact api
--   Isomers.Client.client fetch reqDpl resDpls


