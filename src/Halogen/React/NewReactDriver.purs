module Halogen.React.Dryver (
  ReactDriver,
  ReactEffects,
  ReactComponent,
  ReactClassDriver,
  interpretEval,
  interpretComponent,
  reactComponent,
  reactLifecycleComponent,
  createReactPropsClass,
  createReactPropsClassDriver,
  createReactPropsSpecDriver
) where

import Prelude
import Control.Coroutine.Stalling as SCR
import React as React
import Control.Coroutine (await)
import Control.Monad.Aff (Aff, forkAff, runAff)
import Control.Monad.Cont (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (hoistFree, runFreeM)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..), maybe)
import Halogen (ComponentDSL, HalogenF, HalogenFP(HaltHF, QueryHF, RenderPendingHF, RenderHF, SubscribeHF, StateHF), StateF(Modify, Get))
import Halogen.Query.EventSource (runEventSource)
import Halogen.Query.HalogenF (hoistHalogenF)
import React (createClass, getProps, readState, spec, transformState)

type ReactEffects eff = (state::React.ReactState React.ReadWrite,props::React.ReactProps, refs::React.ReactRefs React.ReadOnly, err::EXCEPTION |eff)
type ReactDriver f eff = f ~> Aff (ReactEffects eff)

type ReactClassDriver p s f eff = {clazz::React.ReactClass p, driver:: React.ReactThis p s -> ReactDriver f eff}

type ReactSpecDriver p s f eff = {spec::React.ReactSpec p s (err::EXCEPTION |eff), driver:: React.ReactThis p s -> ReactDriver f eff}

type EventHandler =
  forall eff refs.
    Eff ( props :: React.ReactProps
        , state :: React.ReactState React.ReadWrite
        , refs :: React.ReactRefs refs
        | eff
        ) Unit

type ReactComponentSpec p s f g  = {
    render :: ((f Unit) -> EventHandler) -> p -> s -> React.ReactElement
  , eval :: p -> f ~> (ComponentDSL s f g)
}

type LifecycleReactComponentSpec p s f g  = {
    render :: ((f Unit) -> EventHandler) -> p -> s -> React.ReactElement
  , eval :: p -> f ~> (ComponentDSL s f g)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
}

newtype ReactComponent p s f g = ReactComponent (LifecycleReactComponentSpec p s f g)

interpretEval :: forall s f g g'. Functor g' => f ~> ComponentDSL s f g -> g ~> g' -> f ~> ComponentDSL s f g'
interpretEval eval nat = hoistFree (hoistHalogenF nat) <<< eval

interpretComponent :: forall p s f g g'. Functor g' => g ~> g' -> ReactComponent p s f g -> ReactComponent p s f g'
interpretComponent nat (ReactComponent spec) = ReactComponent $ spec {eval=(\p -> interpretEval (spec.eval p) nat)}

reactComponent :: forall p s f g. ReactComponentSpec p s f g -> ReactComponent p s f g
reactComponent {render,eval} =
  reactLifecycleComponent
    { render, eval
    , initializer: Nothing
    , finalizer: Nothing
    }

reactLifecycleComponent :: forall p s f g. LifecycleReactComponentSpec p s f g -> ReactComponent p s f g
reactLifecycleComponent s = ReactComponent s

createReactPropsClass :: forall eff p s f. ReactComponent p s f (Aff (ReactEffects eff)) -> s -> React.ReactClass p
createReactPropsClass c = (createReactPropsClassDriver c) >>> _.clazz

createReactPropsClassDriver :: forall eff p s f. ReactComponent p s f (Aff (ReactEffects eff)) -> s -> ReactClassDriver p s f eff
createReactPropsClassDriver rc s = case createReactPropsSpecDriver rc s of
  {spec,driver} -> {clazz:createClass spec, driver}

createReactPropsSpecDriver :: forall eff p s f. ReactComponent p s f (Aff (ReactEffects eff)) -> s -> ReactSpecDriver p s f eff
createReactPropsSpecDriver (ReactComponent rc) s = {spec:(spec s render) {componentDidMount=onMount}, driver:compDriver}
  where
    compDriver = rnDrivers rc.eval
    onMount this = do
      handleAff $ maybe (pure unit) (compDriver this) rc.initializer
      pure unit
    render this = do
      props <- getProps this
      state <- readState this
      pure $ rc.render dispatch props state
      where
        dispatch :: f Unit -> EventHandler
        dispatch f = unsafeCoerceEff $ handleAff $ compDriver this f

handleAff :: forall eff a. Aff (err::EXCEPTION|eff) a -> Eff (err::EXCEPTION|eff) Unit
handleAff = void <<< runAff throwException (const (pure unit)) -- "void <<<" was a quick solution, don't know if it's correct

rnDrivers :: forall p s f eff. (p -> f ~> ComponentDSL s f (Aff (ReactEffects eff))) -> React.ReactThis p s -> ReactDriver f eff
rnDrivers eval this = queryDriver
  where
  queryDriver :: ReactDriver f eff
  queryDriver q = do
    p <- liftEff $ getProps this
    runFreeM freeDriver (eval p q)
  freeDriver:: HalogenF s f (Aff (ReactEffects eff)) ~> Aff (ReactEffects eff)
  freeDriver h =
    case h of
      StateHF i -> do
        case i of
          Get k -> liftEff $ k <$> readState this
          Modify f next -> do
            liftEff $ do
              transformState this f
              pure next
      SubscribeHF es next -> do
        let producer = runEventSource es
            consumer = forever (lift <<< queryDriver =<< await)
        forkAff $ SCR.runStallingProcess (producer SCR.$$? consumer)
        pure next
      RenderHF p next -> pure next
      RenderPendingHF k -> pure $ k Nothing
      QueryHF q -> q
      HaltHF msg -> throwError $ error msg
