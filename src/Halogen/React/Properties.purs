module Halogen.React.Properties where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Exists (mkExists)
import Data.Maybe (Maybe, maybe)
import Data.Nullable (toMaybe, Nullable)
import Halogen.HTML.Core (propName)
import Halogen.HTML.Events (Event)
import Halogen.HTML.Events.Handler (EventHandler)
import Halogen.React (mkHandler3, React, PropsF(PropsF), mkHandler2, PropF(PropF), HandlerF(..), Prop(Renderable, NoOp, ParentRef, Props, Handler, Prop))
import Halogen.React.Driver (ReactEffects)
import React (ReactElement, ReactThis)

type HandlerProp e i = (e -> EventHandler (Maybe i)) -> Prop i
type EventProp e i = (Event e -> EventHandler (Maybe i)) -> Prop i
type AffHandler eff = Aff (ReactEffects eff) Unit

handler1 :: forall e i. String -> (e -> EventHandler (Maybe i)) -> Prop i
handler1 name handler = Handler (mkExists (HandleUnit name handler))

handler2 :: forall a b e i. String -> (a -> b -> e) -> (e -> EventHandler (Maybe i)) -> Prop i
handler2 name toEv handler = Handler (mkExists (HandleUncurried name (mkHandler2 toEv) handler))

handler3 :: forall a b c e i. String -> (a -> b -> c -> e) -> (e -> EventHandler (Maybe i)) -> Prop i
handler3 name toEv handler = Handler (mkExists (HandleUncurried name (mkHandler3 toEv) handler))

handlerAff :: forall e i. String -> (e -> EventHandler (Maybe i)) -> Prop i
handlerAff name handler = Handler (mkExists (HandleAff name handler))

renderableProp :: forall i. ((React i -> ReactElement) -> Prop i) -> Prop i
renderableProp = Renderable

renderable1 :: forall a i. String -> (a -> React i) -> Prop i
renderable1 n f = Renderable (\r -> prop n $ (f >>> r))

renderedProp :: forall i. String -> React i -> Prop i
renderedProp n r = Renderable (\render -> prop n $ render r)

parentRef :: forall i. String -> Prop i
parentRef = ParentRef

style :: forall i s. s -> Prop i
style s = prop "style" s

props :: forall v i. v -> Prop i
props v = Props $ mkExists $ PropsF v

prop :: forall value i. String -> value -> Prop i
prop n v = Prop $ mkExists $ PropF (propName n) v

propMaybe :: forall value i. String -> Maybe value -> Prop i
propMaybe n v = maybe NoOp (prop n) v

key :: forall i a. a -> Prop i
key a = prop "key" a

ref :: forall i. String  -> Prop i
ref = prop "ref"

onRef :: forall p s i. HandlerProp (ReactThis p s) i
onRef = handler1 "ref"

onRefEff :: forall p s i eff. (ReactThis p s -> Eff eff Unit) -> Prop i
onRefEff f = prop "ref" unsafeRef
  where
    unsafeRef :: Nullable (ReactThis p s) -> Unit
    unsafeRef this = maybe unit (unsafePerformEff <<< f) $ toMaybe this
