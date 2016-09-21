module Halogen.React.Driver.DOM (renderToDOM) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Halogen.React.Driver (createReactClass, ReactEffects, ReactComponent)
import React (createElement, ReactElement)

foreign import renderElementDOM :: forall e. ReactElement -> HTMLElement -> Eff (dom::DOM|e) Unit

renderToDOM :: forall s f e. ReactComponent s f (Aff (ReactEffects e)) -> s -> HTMLElement -> Eff (dom::DOM|(ReactEffects e)) Unit
renderToDOM c s = renderElementDOM $ createElement (createReactClass c s) unit []
