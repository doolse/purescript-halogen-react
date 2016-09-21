module Halogen.React.Properties where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Exists (mkExists)
import Data.Maybe (maybe, Maybe)
import Data.Nullable (toMaybe, Nullable)
import Halogen.HTML.Core (propName)
import Halogen.HTML.Events.Handler (EventHandler)
import Halogen.React (PropsF(PropsF), RenderableF, mkHandler2, PropF(PropF), HandlerF(..), Prop(ParentRef, Props, Renderable, Handler, Prop))
import Halogen.React.Driver (ReactEffects)
import React (ReactThis)

type EventProp e i = (e -> EventHandler (Maybe i)) -> Prop i
type AffHandler eff = Aff (ReactEffects eff) Unit

handler1 :: forall e i. String -> (e -> EventHandler (Maybe i)) -> Prop i
handler1 name handler = Handler (mkExists (HandleUnit name handler))

handler2 :: forall a b e i. String -> (a -> b -> e) -> (e -> EventHandler (Maybe i)) -> Prop i
handler2 name toEv handler = Handler (mkExists (HandleUncurried name (mkHandler2 toEv) handler))

handlerAff :: forall e i. String -> (e -> EventHandler (Maybe i)) -> Prop i
handlerAff name handler = Handler (mkExists (HandleAff name handler))

parentRef :: forall i. String -> Prop i
parentRef = ParentRef

style :: forall i s. s -> Prop i
style s = prop "style" s

props :: forall v i. v -> Prop i
props v = Props $ mkExists $ PropsF v

prop :: forall value i. String -> value -> Prop i
prop n v = Prop $ mkExists $ PropF (propName n) v

renderProp :: forall i. String -> RenderableF i -> Prop i
renderProp = Renderable

key :: forall i a. a -> Prop i
key a = prop "key" a

ref :: forall i. String  -> Prop i
ref = prop "ref"

onRef :: forall p s i. EventProp (ReactThis p s) i
onRef = handler1 "ref"

onRefEff :: forall p s i eff. (ReactThis p s -> Eff eff Unit) -> Prop i
onRefEff f = prop "ref" unsafeRef
  where
    unsafeRef :: Nullable (ReactThis p s) -> Unit
    unsafeRef this = maybe unit (unsafePerformEff <<< f) $ toMaybe this
