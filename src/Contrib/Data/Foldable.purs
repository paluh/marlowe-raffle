module Contrib.Data.Foldable where

import Prelude

import Data.Foldable (class Foldable, foldM, foldMap)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex)

foldMapFlipped :: forall a b t. Foldable t => Monoid b => t a -> (a -> b) -> b
foldMapFlipped = flip foldMap

foldMapWithIndexFlipped :: forall a b f i. FoldableWithIndex i f => Monoid b => f a -> (i -> a -> b) -> b
foldMapWithIndexFlipped = flip foldMapWithIndex

foldMFlipped :: forall @t @m a b. Foldable t => Monad m => b -> t a -> (b -> a -> m b) -> m b
foldMFlipped z t f = foldM f z t
