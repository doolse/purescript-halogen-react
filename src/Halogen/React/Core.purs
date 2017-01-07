module Halogen.React where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Exists (Exists)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Function.Uncurried (mkFn3, mkFn2)
import Data.Maybe (Maybe)
import Halogen.HTML.Core (PropName)
import Halogen.HTML.Events.Handler (EventHandler)
import React (ReactClass, ReactElement)
import React.DOM.Props (Props)
import Unsafe.Coerce (unsafeCoerce)

data React i = Text String
  | RenderedElement forall eff. (i -> Aff eff Unit) -> ReactElement
  | Element (Exists ReactClass) (Array (Prop i)) (Array (React i))
  | NamedElement String (Array (Prop i)) (Array (React i))

data Prop i
  = Prop (Exists PropF)
  | NoOp
  | ParentRef String
  | Props (Exists PropsF)
  | Handler (Exists (HandlerF i))
  | Renderable ((React i -> ReactElement) -> Prop i)
  | UnsafeWithProp (Prop i) (Props -> Props)

newtype PropsF value = PropsF value
data PropF value = PropF (PropName value) value

foreign import data UncurriedEvent :: * -> *
foreign import runUncurriedEvent :: forall event r. (event -> r) -> UncurriedEvent event -> Props

mkHandler2 :: forall a b e. (a -> b -> e) -> UncurriedEvent e
mkHandler2 f = unsafeCoerce $ mkFn2 f

mkHandler3 :: forall a b c e. (a -> b -> c -> e) -> UncurriedEvent e
mkHandler3 f = unsafeCoerce $ mkFn3 f

data HandlerF i event = HandleUnit String (event -> EventHandler (Maybe i))
  | HandleUncurried String (UncurriedEvent event) (event -> EventHandler (Maybe i))
  | HandleAff String (event -> EventHandler (Maybe i))

renderedElement :: forall i. ReactElement -> React i
renderedElement = RenderedElement <<< const

type AffDispatcher f = forall eff. (Unit -> f Unit) -> Aff eff Unit
type Dispatcher f = forall a eff. (a -> Unit -> f Unit) -> EffFn1 eff a Unit

renderedElement' :: forall f. ({dispatch::Dispatcher f, dispatchAff::AffDispatcher f} -> ReactElement) -> React (f Unit)
renderedElement' f = RenderedElement \d -> f {
      dispatchAff: \a -> unsafeCoerceAff $ d (a unit)
    , dispatch: \f2 -> mkEffFn1 (\a -> void $ unsafeCoerceEff $ launchAff $ d (f2 a unit))
  }
