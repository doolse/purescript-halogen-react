module Halogen.React where

import Prelude
import Data.Exists (Exists)
import Data.Function.Uncurried (mkFn3, mkFn2)
import Data.Maybe (Maybe)
import Halogen.HTML.Core (PropName)
import Halogen.HTML.Events.Handler (EventHandler)
import React (ReactElement, ReactClass)
import React.DOM.Props (Props)
import Unsafe.Coerce (unsafeCoerce)

data React i = Text String
  | Element (Exists ReactClass) (Array (Prop i)) (Array (React i))

data Prop i
  = Prop (Exists PropF)
  | ParentRef String
  | Props (Exists PropsF)
  | Handler (Exists (HandlerF i))
  | Renderable String (RenderableF i)

foreign import data RenderableF :: * -> *

mkRenderableF :: forall i. React i -> RenderableF i
mkRenderableF = unsafeCoerce

mkRenderableF1 :: forall a i. (a -> React i) -> RenderableF i
mkRenderableF1 = unsafeCoerce

mkRenderableF2 :: forall a b i. (a -> b -> React i) -> RenderableF i
mkRenderableF2 f = unsafeCoerce $ mkFn2 f

mkRenderableF3 :: forall a b c i. (a -> b -> c -> React i) -> RenderableF i
mkRenderableF3 f = unsafeCoerce $ mkFn3 f

newtype PropsF value = PropsF value
data PropF value = PropF (PropName value) value

foreign import data HandlerFn2 :: * -> *
foreign import runRenderable :: forall i. (React i -> ReactElement) -> RenderableF i -> Props

mkHandler2 :: forall a b e. (a -> b -> e) -> UncurriedEvent e
mkHandler2 f = unsafeCoerce $ mkFn2 f

foreign import data UncurriedEvent :: * -> *
foreign import runUncurriedEvent :: forall event r. (event -> r) -> UncurriedEvent event -> Props

data HandlerF i event = HandleUnit String (event -> EventHandler (Maybe i))
  | HandleUncurried String (UncurriedEvent event) (event -> EventHandler (Maybe i))
  | HandleAff String (event -> EventHandler (Maybe i))
