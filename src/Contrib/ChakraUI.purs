module Contrib.ChakraUI where

import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Hook, unsafeHook)
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

foreign import data Theme :: Type

foreign import theme :: Theme

type ChakraProviderBasePropos r = (theme :: Opt Theme, cssVarsRoot :: Opt String | r)
type ChakraProviderProps = ChakraProviderBasePropos (children :: Array JSX)

foreign import _ChakraProvider :: ReactComponent { | ChakraProviderProps }

chakraProvider
  :: forall props
   . NoProblem.Coerce { | props } { | ChakraProviderBasePropos + () }
  => Row.Cons "children" (Array JSX) (ChakraProviderBasePropos + ()) ChakraProviderProps
  => Row.Lacks "children" (ChakraProviderBasePropos + ())
  => { | props }
  -> Array JSX
  -> JSX
chakraProvider props children = do
  let
    props' :: { | ChakraProviderBasePropos + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _ChakraProvider props''

type ContainerPropsBaseRow r = (alignSelf :: String, w :: String | r)
type ContainerPropsRow = ContainerPropsBaseRow (children :: Array JSX)

foreign import _Container :: ReactComponent { | ContainerPropsRow }

container
  :: forall props
   . NoProblem.Coerce { | props } { | ContainerPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (ContainerPropsBaseRow + ()) ContainerPropsRow
  => Row.Lacks "children" (ContainerPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
container props children = do
  let
    props' :: { | ContainerPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Container props''

type StackPropsBaseRow r =
  ( align :: Opt String
  , direction :: Opt String -- "row"
  , spacing :: Opt String -- { base :: String }
  , pb :: Opt String
  , pt :: Opt String
  , py :: Opt String
  , textAlign :: Opt String
  | r
  )

type StackPropsRow = StackPropsBaseRow (children :: Array JSX)

foreign import _Stack :: ReactComponent { | StackPropsRow }

stack
  :: forall props
   . NoProblem.Coerce { | props } { | StackPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (StackPropsBaseRow + ()) StackPropsRow
  => Row.Lacks "children" (StackPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
stack props children = do
  let
    props' :: { | StackPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Stack props''

type HeadingPropsBaseRow r =
  ( "as" :: Opt String
  , alignSelf :: Opt String
  , fontSize :: Opt String
  , size :: Opt String
  , textAlign :: Opt String
  | r
  )

type HeadingPropsRow = HeadingPropsBaseRow (children :: Array JSX)

foreign import _Heading :: ReactComponent { | HeadingPropsRow }

heading
  :: forall props
   . NoProblem.Coerce { | props } { | HeadingPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (HeadingPropsBaseRow + ()) HeadingPropsRow
  => Row.Lacks "children" (HeadingPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
heading props children = do
  let
    props' :: { | HeadingPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Heading props''

type TextPropsBaseRow r =
  ( overflowWrap :: Opt String
  , w :: Opt String
  , fontSize :: Opt String
  , fontWeight :: Opt String
  , align :: Opt String
  , textAlign :: Opt String
  , animation :: Opt String
  | r
  )

type TextPropsRow = TextPropsBaseRow (children :: Array JSX)

foreign import _Text :: ReactComponent { | TextPropsRow }

text
  :: forall props
   . NoProblem.Coerce { | props } { | TextPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (TextPropsBaseRow + ()) TextPropsRow
  => Row.Lacks "children" (TextPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
text props children = do
  let
    props' :: { | TextPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Text props''

type CenterPropsBaseRow r = (color :: Opt String | r)
type CenterPropsRow = CenterPropsBaseRow (children :: Array JSX)

foreign import _Center :: ReactComponent { | CenterPropsRow }

center
  :: forall props
   . NoProblem.Coerce { | props } { | CenterPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (CenterPropsBaseRow + ()) CenterPropsRow
  => Row.Lacks "children" (CenterPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
center props children = do
  let
    props' :: { | CenterPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Center props''

-- const isDesktop = useBreakpointValue({ base: false, lg: true });
-- 
-- const isTablet = useBreakpointValue({ base: false, md: true, lg: false });
type UseBreakpointValuePropsRow = (base :: Opt Boolean, lg :: Opt Boolean, md :: Opt Boolean)

foreign import useBreakpointValue_ :: EffectFn1 { | UseBreakpointValuePropsRow } Boolean

foreign import data UseBreakpointValue :: Type -> Type

useBreakpointValue
  :: forall props
   . NoProblem.Coerce { | props } { | UseBreakpointValuePropsRow }
  => { | props }
  -> Hook UseBreakpointValue Boolean
useBreakpointValue props = unsafeHook do
  let
    props' :: { | UseBreakpointValuePropsRow }
    props' = NoProblem.coerce props
  runEffectFn1 useBreakpointValue_ props'

foreign import data UseColorModeValue :: Type -> Type

foreign import useColorModeValue_ :: EffectFn2 String String String

useColorModeValue
  :: String
  -> String
  -> Hook UseColorModeValue String
useColorModeValue color1 color2 = unsafeHook do
  runEffectFn2 useColorModeValue_ color1 color2

type ModalPropsBaseRow r =
  ( isOpen :: Boolean
  , onClose :: Opt EventHandler
  , size :: Opt String
  , motionPreset :: Opt String
  , isCentered :: Opt Boolean
  | r
  )

type ModalPropsRow = ModalPropsBaseRow (children :: Array JSX)

foreign import _Modal :: ReactComponent { | ModalPropsRow }

modal
  :: forall props
   . NoProblem.Coerce { | props } { | ModalPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (ModalPropsBaseRow + ()) ModalPropsRow
  => Row.Lacks "children" (ModalPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
modal props children = do
  let
    props' :: { | ModalPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Modal props''

type ModalOverlayPropsBaseRow r = (bgColor :: String | r)
type ModalOverlayPropsRow = ModalOverlayPropsBaseRow ()

foreign import _ModalOverlay :: ReactComponent { | ModalOverlayPropsRow }

modalOverlay
  :: forall props
   . NoProblem.Coerce { | props } { | ModalOverlayPropsBaseRow + () }
  => { | props }
  -> JSX
modalOverlay props = do
  let
    props' :: { | ModalOverlayPropsBaseRow + () }
    props' = NoProblem.coerce props
  element _ModalOverlay props'

type ModalContentPropsBaseRow r =
  ( bgColor :: String
  , boxShadow :: String
  , rounded :: String
  , px :: String
  | r
  )

type ModalContentPropsRow = ModalContentPropsBaseRow (children :: Array JSX)

foreign import _ModalContent :: ReactComponent { | ModalContentPropsRow }

modalContent
  :: forall props
   . NoProblem.Coerce { | props } { | ModalContentPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (ModalContentPropsBaseRow + ()) ModalContentPropsRow
  => Row.Lacks "children" (ModalContentPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
modalContent props children = do
  let
    props' :: { | ModalContentPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _ModalContent props''

type ModalHeaderPropsBaseRow r = (| r)
type ModalHeaderPropsRow = ModalHeaderPropsBaseRow (children :: Array JSX)

foreign import _ModalHeader :: ReactComponent { | ModalHeaderPropsRow }

modalHeader
  :: forall props
   . NoProblem.Coerce { | props } { | ModalHeaderPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (ModalHeaderPropsBaseRow + ()) ModalHeaderPropsRow
  => Row.Lacks "children" (ModalHeaderPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
modalHeader props children = do
  let
    props' :: { | ModalHeaderPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _ModalHeader props''

type ModalCloseButtonPropsBaseRow r = (_focus :: { boxShadow :: String } | r)
type ModalCloseButtonPropsRow = ModalCloseButtonPropsBaseRow ()

foreign import _ModalCloseButton :: ReactComponent { | ModalCloseButtonPropsRow }

modalCloseButton
  :: forall props
   . NoProblem.Coerce { | props } { | ModalCloseButtonPropsBaseRow + () }
  => { | props }
  -> JSX
modalCloseButton props = do
  let
    props' :: { | ModalCloseButtonPropsBaseRow + () }
    props' = NoProblem.coerce props
  element _ModalCloseButton props'

type ModalBodyPropsBaseRow r = (minH :: String | r)
type ModalBodyPropsRow = ModalBodyPropsBaseRow (children :: Array JSX)

foreign import _ModalBody :: ReactComponent { | ModalBodyPropsRow }

modalBody
  :: forall props
   . NoProblem.Coerce { | props } { | ModalBodyPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (ModalBodyPropsBaseRow + ()) ModalBodyPropsRow
  => Row.Lacks "children" (ModalBodyPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
modalBody props children = do
  let
    props' :: { | ModalBodyPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _ModalBody props''

type ModalFooterPropsBaseRow r = (textAlign :: String, alignSelf :: String | r)
type ModalFooterPropsRow = ModalFooterPropsBaseRow (children :: Array JSX)

foreign import _ModalFooter :: ReactComponent { | ModalFooterPropsRow }

modalFooter
  :: forall props
   . NoProblem.Coerce { | props } { | ModalFooterPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (ModalFooterPropsBaseRow + ()) ModalFooterPropsRow
  => Row.Lacks "children" (ModalFooterPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
modalFooter props children = do
  let
    props' :: { | ModalFooterPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _ModalFooter props''

type HStackPropsBaseRow r = (| r)
type HStackPropsRow = HStackPropsBaseRow (children :: Array JSX)

foreign import _HStack :: ReactComponent { | HStackPropsRow }

hStack
  :: forall props
   . NoProblem.Coerce { | props } { | HStackPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (HStackPropsBaseRow + ()) HStackPropsRow
  => Row.Lacks "children" (HStackPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
hStack props children = do
  let
    props' :: { | HStackPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _HStack props''

type LinkPropsBaseRow r =
  ( href :: String
  , alignSelf :: Opt String
  , boxShadow :: Opt String
  , border :: Opt String
  , _focus :: Opt { boxShadow :: Opt String, border :: Opt String }
  , _hover :: Opt { bgColor :: Opt String }
  , justifyContent :: Opt String
  , ml :: Opt String
  , target :: Opt String
  | r
  )

type LinkPropsRow = LinkPropsBaseRow (children :: Array JSX)

foreign import _Link :: ReactComponent { | LinkPropsRow }

link
  :: forall props
   . NoProblem.Coerce { | props } { | LinkPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (LinkPropsBaseRow + ()) LinkPropsRow
  => Row.Lacks "children" (LinkPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
link props children = do
  let
    props' :: { | LinkPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Link props''

-- <Stack spacing={3}>
--   <Select variant='outline' placeholder='Outline' />
--   <Select variant='filled' placeholder='Filled' />
--   <Select variant='flushed' placeholder='Flushed' />
--   <Select variant='unstyled' placeholder='Unstyled' />
-- </Stack>
--
-- <Stack spacing={3}>
-- <Select placeholder='extra small size' size='xs' />
-- <Select placeholder='small size' size='sm' />
-- <Select placeholder='medium size' size='md' />
-- <Select placeholder='large size' size='lg' />
-- <Select icon={<MdArrowDropDown />} placeholder='Woohoo! A new icon' />
-- <Select
--   bg='tomato'
--   borderColor='tomato'
--   color='white'
--   placeholder='Woohoo! A new background color!'
-- />
-- </Stack>

-- Given the examples above let's try to bind to Select

type SelectPropsBaseRow r =
  ( placeholder :: Opt String
  , variant :: Opt String
  , size :: Opt String
  , icon :: Opt JSX
  , bg :: Opt String
  , borderColor :: Opt String
  , color :: Opt String
  , onChange :: Opt EventHandler
  | r
  )

type SelectPropsRow = SelectPropsBaseRow (children :: Array JSX)

foreign import _Select :: ReactComponent { | SelectPropsRow }

select
  :: forall props
   . NoProblem.Coerce { | props } { | SelectPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (SelectPropsBaseRow + ()) SelectPropsRow
  => Row.Lacks "children" (SelectPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
select props children = do
  let
    props' :: { | SelectPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Select props''

type ButtonPropsBaseRow r =
  ( w :: Opt String
  , alignSelf :: Opt String
  , bgColor :: Opt String
  , borderRadius :: Opt String
  , justifySelf :: Opt String
  , onClick :: Opt EventHandler
  , _hover :: Opt { bgColor :: Opt String }
  | r
  )

type ButtonPropsRow = ButtonPropsBaseRow (children :: Array JSX)

foreign import _Button :: ReactComponent { | ButtonPropsRow }

button
  :: forall props
   . NoProblem.Coerce { | props } { | ButtonPropsBaseRow + () }
  => Row.Cons "children" (Array JSX) (ButtonPropsBaseRow + ()) ButtonPropsRow
  => Row.Lacks "children" (ButtonPropsBaseRow + ())
  => { | props }
  -> Array JSX
  -> JSX
button props children = do
  let
    props' :: { | ButtonPropsBaseRow + () }
    props' = NoProblem.coerce props

    props'' = Record.insert (Proxy :: Proxy "children") children props'
  element _Button props''
