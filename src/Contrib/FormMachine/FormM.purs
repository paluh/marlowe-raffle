module Contrib.FormMachine.FormM where

import Prelude

import Data.Tuple (Tuple)
import Control.Applicative.Free (FreeAp, liftFreeAp, hoistFreeAp)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Parallel.Class (class Parallel)
import Data.Bifunctor (lmap)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over)
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Prim.Row as Row
import Type.Proxy (Proxy)

newtype ForkId = ForkId Int

derive newtype instance Eq ForkId
derive newtype instance Ord ForkId

data FormF state m a
  = State (state -> Tuple a state)
  | Lift (m a)
  | Fork (FormM state m Unit) (ForkId -> a)
  -- | Par (HalogenAp state action slots output m a)
  -- | Join ForkId a
  -- | Kill ForkId a

newtype FormM state m a = FormM (Free (FormF state m) a)

instance Functor m => Functor (FormF state m) where
  map f = case _ of
    State k -> State (lmap f <<< k)
    Lift q -> Lift (map f q)
    Fork hmu k -> Fork hmu (f <<< k)
    -- Par pa -> Par (map f pa)
    -- Join fid a -> Join fid (f a)
    -- Kill fid a -> Kill fid (f a)

derive newtype instance Functor (FormM state m)
derive newtype instance Apply (FormM state m)
derive newtype instance Applicative (FormM state m)
derive newtype instance Bind (FormM state m)
derive newtype instance Monad (FormM state m)
derive newtype instance Semigroup a => Semigroup (FormM state m a)
derive newtype instance Monoid a => Monoid (FormM state m a)

instance MonadEffect m => MonadEffect (FormM state m) where
  liftEffect = FormM <<< liftF <<< Lift <<< liftEffect

instance MonadAff m => MonadAff (FormM state m) where
  liftAff = FormM <<< liftF <<< Lift <<< liftAff

instance MonadTrans (FormM state) where
  lift = FormM <<< liftF <<< Lift

instance MonadRec (FormM state m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance MonadState state (FormM state m) where
  state = FormM <<< liftF <<< State

instance MonadAsk r m => MonadAsk r (FormM state m) where
  ask = FormM $ liftF $ Lift ask

instance MonadTell w m => MonadTell w (FormM state m) where
  tell = FormM <<< liftF <<< Lift <<< tell

instance MonadThrow e m => MonadThrow e (FormM state m) where
  throwError = FormM <<< liftF <<< Lift <<< throwError

-- instance Parallel (HalogenAp state action slots output m) (FormM state m) where
--   parallel = HalogenAp <<< liftFreeAp
--   sequential = FormM <<< liftF <<< Par

fork :: forall state m. FormM state m Unit -> FormM state m ForkId
fork hmu = FormM $ liftF $ Fork hmu identity

-- join :: forall state action slots output m. ForkId -> FormM state m Unit
-- join fid = FormM $ liftF $ Join fid unit
-- 
-- -- | Kills a forked process if it is still running. Attempting to kill a forked
-- -- | process that has already ended will have no effect.
-- kill :: forall state action slots output m. ForkId -> FormM state m Unit
-- kill fid = FormM $ liftF $ Kill fid unit
