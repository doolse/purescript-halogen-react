
module Halogen.React.Driver (
  ReactDriver,
  ReactEffects,
  ReactComponent,
  reactComponent,
  createReactClass
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
import Control.Monad.Free (runFreeM)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Writer (runWriter)
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
import Halogen.Query.HalogenF (HalogenF, HalogenFP(..))
import Halogen.Query.StateF (StateF(Modify, Get))
import Halogen.React (PropsF(PropsF), runUncurriedEvent, runRenderable, HandlerF(..), PropF(PropF), Prop(..), React(..))
import React (transformState, ReactThis, Refs, ReactElement, ReactClass, ReadWrite, ReactState, ReadOnly, ReactRefs, ReactProps, getRefs, getProps, readState, createElement, createElementTagName, spec, createClass)
import React.DOM.Props (Props, unsafeMkProps, unsafeFromPropsArray)
import Unsafe.Coerce (unsafeCoerce)

type ReactEffects eff = (state::ReactState ReadWrite,props::ReactProps, refs::ReactRefs ReadOnly, err::EXCEPTION |eff)
type ReactDriver f eff = f ~> Aff (ReactEffects eff)

type ReactClassDriver p s s' f eff = {clazz::ReactClass p, driver:: ReactThis p s -> ReactDriver f eff}

foreign import createElementOneChild :: forall p. ReactClass p -> p -> ReactElement -> ReactElement

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

reactLifecycleComponent :: forall s f g. LifecycleReactComponentSpec s f g -> ReactComponent s f g
reactLifecycleComponent s = ReactComponent s

createReactClass :: forall eff p s f. ReactComponent s f (Aff (ReactEffects eff)) -> s -> ReactClass p
createReactClass c = (createReactClassDriver c) >>> _.clazz

createReactClassDriver :: forall eff p s f. ReactComponent s f (Aff (ReactEffects eff)) -> s -> ReactClassDriver p s s f eff
createReactClassDriver (ReactComponent rc) s = {clazz:createClass $ (spec s render) {componentDidMount=onMount}, driver:compDriver}
  where
    compDriver = rnDrivers (\s p r -> s) id rc.eval
    onMount this = do
      handleAff $ maybe (pure unit) (\i -> compDriver this i) rc.initializer
      pure unit
    render this = do
      s <- readState this
      pure $ renderReact (compDriver this) (rc.render s)


type PropsState props state = {props::props, state::state, refs::Refs}

modifyState :: forall p s f g. (s -> s) -> ComponentDSL (PropsState p s) f g Unit
modifyState f = modify \s -> s {state=f s.state}

createPropsClass :: forall eff p s f. ReactComponent (PropsState p s) f (Aff (ReactEffects eff)) -> s -> ReactClass p
createPropsClass c = (createPropsClassDriver c) >>> _.clazz

createPropsClassDriver :: forall eff p s f. ReactComponent (PropsState p s) f (Aff (ReactEffects eff)) -> s -> ReactClassDriver p s (PropsState p s) f eff
createPropsClassDriver (ReactComponent rc) s = {clazz:createClass $ (spec s render) {componentDidMount=onMount}, driver:compDriver}
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

withRef :: forall p s p' s' f eff. String -> (ReactThis p s -> Eff eff Unit) -> ComponentDSL (PropsState p' s') f (Aff eff) Unit
withRef n f = do
  {refs} <- get
  let r = getRef refs n
  fromEff $ maybe (pure unit) f r

queryRef :: forall p s eff f a s' f'. Refs -> String -> (ReactThis p s -> ReactDriver f eff) -> f a -> ComponentDSL s' f' (Aff (ReactEffects eff)) (Maybe a)
queryRef refs n f action = fromAff $ maybe (pure Nothing) sendAction (getRef refs n)
  where sendAction this = Just <$> (f this action)

renderReact :: forall f eff. ReactDriver f eff -> React (f Unit) -> ReactElement
renderReact dr html = let go = renderReact dr in case html of
    (Text s) -> unsafeCoerce s
    (NamedElement n props els) -> createElementTagName n  (runProps props) $ map go els
    (Element clazzE props els) -> runExists (\clazz -> createElement clazz (runProps props)) clazzE $ map go els
    where
      runProps :: forall props. (Array (Prop (f Unit))) -> props
      runProps p = unsafeFromPropsArray (map renderProp p)

      renderProp :: Prop (f Unit) -> Props
      renderProp (Prop e) = runExists (\(PropF key value) -> unsafeMkProps (runPropName key) value) e
      renderProp (Props e) = runExists (\(PropsF v) -> unsafeCoerce v) e
      renderProp (Handler e) = runExists renderHandler e
      renderProp (Renderable n r) = unsafeMkProps n $ runRenderable (renderReact dr) r
      renderProp (ParentRef n) = unsafeMkProps n dr

      renderHandler :: forall event. HandlerF (f Unit) event -> Props
      renderHandler h = case h of
        (HandleUnit name handler) -> unsafeMkProps name (execHandler handler)
        (HandleUncurried name conv handler) -> unsafeMkProps name $ runUncurriedEvent (execHandler handler) conv
        (HandleAff name handler) -> unsafeMkProps name (execAffHandler handler)
        where
          execAffHandler handler ev = case runWriter (unEventHandler (handler ev)) of
              (Tuple mf _) -> maybe (pure unit) dr mf
          execHandler h e = unsafePerformEff $ handleAff $ execAffHandler h e


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
