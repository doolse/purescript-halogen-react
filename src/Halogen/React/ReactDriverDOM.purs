module Halogen.React.Driver.DOM (renderToDOMThis,renderToDOM) where

import Prelude
import Control.Monad.Aff (makeAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Halogen.React.Driver (ReactEffects, ReactClassDriver, ReactComponent, createReactClassDriver)
import React (ReactThis, createElement, ReactElement)
import Unsafe.Coerce (unsafeCoerce)

foreign import renderElementDOM :: forall e. ReactElement -> HTMLElement -> Eff (dom::DOM|e) Unit

renderToDOM :: forall s f e. ReactComponent s f (Aff (ReactEffects (dom::DOM|e))) -> s -> HTMLElement -> Aff (ReactEffects (dom::DOM|e)) Unit
renderToDOM c s e = do
  let cd = createReactClassDriver c s
  void $ renderToDOMThis cd e

renderToDOMThis :: forall p s f e. ReactClassDriver p s f (dom::DOM|e) -> HTMLElement -> Aff (ReactEffects (dom::DOM|e)) (ReactThis p s)
renderToDOMThis cd e = makeAff setup
  where
    setup err suc = renderElementDOM (createElement cd.clazz (unsafeCoerce {ref: refFunc }) []) e
      where
        refFunc :: ReactThis p s -> Unit
        refFunc t = unsafePerformEff $ suc t
