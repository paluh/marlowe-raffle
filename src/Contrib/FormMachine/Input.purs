module Contrib.FormMachine.Input where

import Prelude

import Data.FormURLEncoded.Query as FormURLEncoded
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Web.DOM (Element)

-- | TODO: handle custom actions
data Input
  = Validate
  | UpdateFieldsValue (FormURLEncoded.Query -> FormURLEncoded.Query)
  -- | Action action

