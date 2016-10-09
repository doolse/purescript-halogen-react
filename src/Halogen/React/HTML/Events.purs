module Halogen.React.HTML.Events where

import Halogen.HTML.Events.Types (KeyboardEvent, MouseEvent)
import Halogen.React.Properties (EventProp, handler1)

onClick :: forall i. EventProp MouseEvent i
onClick = handler1 "onClick"

onChange :: forall i. EventProp KeyboardEvent i
onChange = handler1 "onChange"
