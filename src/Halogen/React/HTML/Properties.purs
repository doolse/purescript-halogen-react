module Halogen.React.HTML.Properties where

import Prelude
import Data.String (joinWith)
import Halogen.HTML (runClassName)
import Halogen.HTML.Core (ClassName)
import Halogen.React (Prop)
import Halogen.React.Properties (prop)

classes :: forall i. Array ClassName -> Prop i
classes c = prop "className" $ joinWith " " $ runClassName <$> c

title :: forall i. String -> Prop i
title = prop "title"

href :: forall i. String -> Prop i
href = prop "href"

id_ :: forall i. String -> Prop i
id_ = prop "id"

value :: forall i a. a -> Prop i
value = prop "value"

label :: forall i. String -> Prop i
label = prop "label"
