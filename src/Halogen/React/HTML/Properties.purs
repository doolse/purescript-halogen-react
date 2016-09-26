module Halogen.React.HTML.Properties where

import Prelude
import Data.String (joinWith)
import Halogen.HTML (runClassName)
import Halogen.HTML.Core (ClassName)
import Halogen.React (Prop)
import Halogen.React.Properties (prop)

classes :: forall i. Array ClassName -> Prop i
classes c = prop "className" $ joinWith " " $ runClassName <$> c
