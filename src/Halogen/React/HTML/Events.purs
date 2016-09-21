module Halogen.React.HTML.Events where

import Halogen.HTML.Events (Event)
import Halogen.HTML.Events.Types (MouseEvent)
import Halogen.React.Properties (EventProp, handler1)

onClick :: forall i. EventProp (Event MouseEvent) i
onClick = handler1 "onClick"
