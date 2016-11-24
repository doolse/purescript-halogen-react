
module Halogen.React.Driver (
  ReactDriver,
  ReactEffects,
  ReactComponent,
  ReactComponentSpec,
  ReactClassDriver,
  PropsState,
  modifyState,
  interpretEval,
  interpretComponent,
  reactComponent,
  reactLifecycleComponent,
  createReactClass,
  createReactClassDriver,
  createReactPropsClass,
  createReactPropsClassDriver,
  createReactSpecDriver,
  createReactPropsSpecDriver,
  getDOMRef,
  withDOMRef,
  withRef
)
where

import Prelude
import Control.Coroutine.Stalling as SCR
import Control.Coroutine (await)
import Control.Monad.Aff (Aff, forkAff, runAff)
import Control.Monad.Cont (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (throwError)
import Control.Monad.Free (hoistFree, runFreeM)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Writer (runWriter)
import DOM.HTML.Types (HTMLElement)
import Data.Exists (runExists)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Nullable (toMaybe, Nullable)
import Data.Tuple (Tuple(Tuple))
import Halogen (fromEff, fromAff, ComponentDSL)
import Halogen.HTML.Core (runPropName)
import Halogen.HTML.Events.Handler (unEventHandler)
import Halogen.Query (get, modify)
import Halogen.Query.EventSource (runEventSource)
import Halogen.Query.HalogenF (hoistHalogenF, HalogenF, HalogenFP(..))
import Halogen.Query.StateF (StateF(Modify, Get))
import Halogen.React (PropsF(PropsF), runUncurriedEvent, HandlerF(..), PropF(PropF), Prop(..), React(..))
import React (ReactClass, ReactElement, ReactProps, ReactRefs, ReactSpec, ReactState, ReactThis, ReadOnly, ReadWrite, Refs, createClass, createElement, createElementTagName, getProps, getRefs, readState, spec, transformState)
import React.DOM.Props (Props, unsafeMkProps, unsafeFromPropsArray)
import Unsafe.Coerce (unsafeCoerce)
import React as React

type ReactEffects eff = (state::ReactState ReadWrite,props::ReactProps, refs::ReactRefs ReadOnly, err::EXCEPTION |eff)
type ReactDriver f eff = f ~> Aff (ReactEffects eff)

type ReactClassDriver p s f eff = {clazz::ReactClass p, driver:: ReactThis p s -> ReactDriver f eff}

type ReactSpecDriver p s f eff = {spec::ReactSpec p s (err::EXCEPTION |eff), driver:: ReactThis p s -> ReactDriver f eff}

foreign import createElementOneChild :: forall p. ReactClass p -> p -> ReactElement -> ReactElement

type EventHandler =
  forall eff refs.
    Eff ( props :: React.ReactProps
        , state :: React.ReactState React.ReadWrite
        , refs :: React.ReactRefs refs
        | eff
        ) Unit

type LifecycleReactComponentSpec2 p s f g  = {
    render :: ((f Unit) -> EventHandler) -> p -> s -> ReactElement
  , eval :: f ~> (ComponentDSL s f g)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
}

type LifecycleReactComponentSpec s f g  = {
    render :: s -> React (f Unit)
  , eval :: f ~> (ComponentDSL s f g)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
}

newtype ReactComponent s f g = ReactComponent (LifecycleReactComponentSpec s f g)

type ReactComponentSpec s f g =
  { render :: s -> React (f Unit)
  , eval :: f ~> ComponentDSL s f g
  }

-- | Builds a self-contained component with no possible children.
reactComponent :: forall s f g. ReactComponentSpec s f g -> ReactComponent s f g
reactComponent spec =
  reactLifecycleComponent
    { render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    }

interpretEval :: forall s f g g'. Functor g' => f ~> ComponentDSL s f g -> g ~> g' -> f ~> ComponentDSL s f g'
interpretEval eval nat = hoistFree (hoistHalogenF nat) <<< eval

interpretComponent :: forall s f g g'. Functor g' => g ~> g' -> ReactComponent s f g -> ReactComponent s f g'
interpretComponent nat (ReactComponent spec) = ReactComponent $ spec {eval=interpretEval spec.eval nat}

reactLifecycleComponent :: forall s f g. LifecycleReactComponentSpec s f g -> ReactComponent s f g
reactLifecycleComponent s = ReactComponent s

createReactClass :: forall eff p s f. ReactComponent s f (Aff (ReactEffects eff)) -> s -> ReactClass p
createReactClass c = (createReactClassDriver c) >>> _.clazz

createReactSpecDriver :: forall eff p s f. ReactComponent s f (Aff (ReactEffects eff)) -> s -> ReactSpecDriver p s f eff
createReactSpecDriver (ReactComponent rc) is = {spec:(spec is render) {componentDidMount=onMount}, driver:compDriver}
  where
    compDriver = rnDrivers (\s p r -> s) id rc.eval
    onMount this = do
      handleAff $ maybe (pure unit) (\i -> compDriver this i) rc.initializer
      pure unit
    render this = do
      s <- readState this
      pure $ renderReact (compDriver this) (rc.render s)

createReactClassDriver :: forall eff p s f. ReactComponent s f (Aff (ReactEffects eff)) -> s -> ReactClassDriver p s f eff
createReactClassDriver rc s = case createReactSpecDriver rc s of
  {spec,driver} -> {clazz:createClass spec, driver}


type PropsState props state = {props::props, state::state, refs::Refs}

modifyState :: forall p s f g. (s -> s) -> ComponentDSL (PropsState p s) f g Unit
modifyState f = modify \s -> s {state=f s.state}

createReactPropsClass :: forall eff p s f. ReactComponent (PropsState p s) f (Aff (ReactEffects eff)) -> s -> ReactClass p
createReactPropsClass c = (createReactPropsClassDriver c) >>> _.clazz

createReactPropsClassDriver :: forall eff p s f. ReactComponent (PropsState p s) f (Aff (ReactEffects eff)) -> s -> ReactClassDriver p s f eff
createReactPropsClassDriver rc s = case createReactPropsSpecDriver rc s of
  {spec,driver} -> {clazz:createClass spec, driver}

createReactPropsSpecDriver :: forall eff p s f. ReactComponent (PropsState p s) f (Aff (ReactEffects eff)) -> s -> ReactSpecDriver p s f eff
createReactPropsSpecDriver (ReactComponent rc) s = {spec:(spec s render) {componentDidMount=onMount}, driver:compDriver}
  where
    compDriver this = rnDrivers (\state props refs -> {props,state,refs}) _.state rc.eval this
    onMount this = do
      handleAff $ maybe (pure unit) (\i -> compDriver this i) rc.initializer
      pure unit
    render this = do
      props <- getProps this
      state <- readState this
      pure $ renderReact (compDriver this) (rc.render {props,state,refs:unsafeCoerce unit})

foreign import getRefImpl :: forall p s. Fn2 Refs String (Nullable (ReactThis p s))

getRef :: forall p s. Refs -> String -> Maybe (ReactThis p s)
getRef r n = toMaybe $ runFn2 getRefImpl r n

getDOMRef :: Refs -> String -> Maybe HTMLElement
getDOMRef = unsafeCoerce <<< getRef

withDOMRef ::forall eff p s f. String -> (HTMLElement -> Eff eff Unit) -> ComponentDSL (PropsState p s) f (Aff eff) Unit
withDOMRef = withRef' getDOMRef

withRef :: forall eff p s f. String -> (ReactThis p s -> Eff eff Unit) -> ComponentDSL (PropsState p s) f (Aff eff) Unit
withRef = withRef' (unsafeCoerce <<< getRef)

withRef' :: forall a p s f eff. (Refs -> String -> Maybe a) -> String -> (a -> Eff eff Unit) -> ComponentDSL (PropsState p s) f (Aff eff) Unit
withRef' g n f = do
  {refs} <- get
  fromEff $ maybe (pure unit) f $ g refs n

queryRef :: forall p s eff f a s' f'. Refs -> String -> (ReactThis p s -> ReactDriver f eff) -> f a -> ComponentDSL s' f' (Aff (ReactEffects eff)) (Maybe a)
queryRef refs n f action = fromAff $ maybe (pure Nothing) sendAction (getRef refs n)
  where sendAction this = Just <$> (f this action)

renderReact :: forall f eff. ReactDriver f eff -> React (f Unit) -> ReactElement
renderReact dr html = case html of
    (Text s) -> unsafeCoerce s
    (RenderedElement r) -> r
    (NamedElement n props els) -> createElementTagName n  (runProps props) $ map go els
    (Element clazzE props els) -> runExists (\clazz -> createElement clazz (runProps props)) clazzE $ map go els
    where
      go = renderReact dr
      runProps :: forall props. (Array (Prop (f Unit))) -> props
      runProps p = unsafeFromPropsArray (map renderProp p)

      renderProp :: Prop (f Unit) -> Props
      renderProp NoOp = unsafeFromPropsArray []
      renderProp (Prop e) = runExists (\(PropF key value) -> unsafeMkProps (runPropName key) value) e
      renderProp (Props e) = runExists (\(PropsF v) -> unsafeCoerce v) e
      renderProp (Handler e) = runExists renderHandler e
      renderProp (Renderable r) = renderProp (r go)
      renderProp (ParentRef n) = unsafeMkProps n dr
      renderProp (UnsafeWithProp p f) = f (renderProp p)

      renderHandler :: forall event. HandlerF (f Unit) event -> Props
      renderHandler h = case h of
        (HandleUnit name handler) -> unsafeMkProps name (execHandler handler)
        (HandleUncurried name conv handler) -> unsafeMkProps name $ runUncurriedEvent (execHandler handler) conv
        (HandleAff name handler) -> unsafeMkProps name (execAffHandler handler)
        where
          execAffHandler handler ev = case runWriter (unEventHandler (handler ev)) of
              (Tuple mf _) -> maybe (pure unit) dr mf
          execHandler handler e = unsafePerformEff $ handleAff $ execAffHandler handler e


handleAff :: forall eff a. Aff (err::EXCEPTION|eff) a -> Eff (err::EXCEPTION|eff) Unit
handleAff = void <<< runAff throwException (const (pure unit)) -- "void <<<" was a quick solution, don't know if it's correct

rnDrivers :: forall p s s' f eff. (s -> p -> Refs -> s') -> (s' -> s) -> (f ~> ComponentDSL s' f (Aff (ReactEffects eff))) -> ReactThis p s -> ReactDriver f eff
rnDrivers combine getState eval this = queryDriver
  where
  queryDriver :: ReactDriver f eff
  queryDriver q = runFreeM freeDriver (eval q)
  freeDriver:: HalogenF s' f (Aff (ReactEffects eff)) ~> Aff (ReactEffects eff)
  freeDriver h =
    case h of
      StateHF i -> do
        case i of
          Get k -> do
            s' <- liftEff $ do
                    p <- getProps this
                    s <- readState this
                    r <- getRefs this
                    pure $ combine s p r
            pure (k s')
          Modify f next -> do
            liftEff $ do
              p <- getProps this
              r <- getRefs this
              transformState this (\s -> getState $ f (combine s p r))
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
