module Contrib.CNCAla.Animation.EaseInWithSlidingAnimation where

import Contrib.JsonBigInt as JsonBigInt
import Data.Argonaut (class EncodeJson, encodeJson, stringifyWithIndent)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Foreign.Object (Object)
import JsYaml as JsYaml
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic (JSX, ReactComponent, element)
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

-- TS props
-- interface EaseInWithSlidingAnimationProps {
-- 	initX?: number;
-- 	finalX?: number;
-- 	initY?: number;
-- 	finalY?: number;
-- 	delay?: number;
-- 	duration?: number;
-- 	children: JSX.Element[] | JSX.Element;
-- }

type EaseInWithSlidingAnimationPropsBaseRow r =
  ( initX :: Opt Int
  , finalX :: Opt Int
  , initY :: Opt Int
  , finalY :: Opt Int
  , delay :: Opt Int
  , duration :: Opt Int
  | r
  )
type EaseInWithSlidingAnimationPropsRow = EaseInWithSlidingAnimationPropsBaseRow (children :: Array JSX)

foreign import _EaseInWithSlidingAnimation :: ReactComponent { | EaseInWithSlidingAnimationPropsRow }

easeInWithSlidingAnimation
  :: forall props
   . NoProblem.Coerce { | props } { | EaseInWithSlidingAnimationPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (EaseInWithSlidingAnimationPropsBaseRow + ()) EaseInWithSlidingAnimationPropsRow
  => Row.Lacks "children" (EaseInWithSlidingAnimationPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
easeInWithSlidingAnimation props children = do
  let
    props' :: { | EaseInWithSlidingAnimationPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _EaseInWithSlidingAnimation props''
