module Halogen.React.HTML where

import Halogen.React (Prop, React(Text, NamedElement))

div :: forall i. Array (Prop i) -> Array (React i) -> React i
div = NamedElement "div"

div_ :: forall i. Array (React i) -> React i
div_ = div []

h1 :: forall i. Array (Prop i) -> Array (React i) -> React i
h1 = NamedElement "h1"

h1_ :: forall i. Array (React i) -> React i
h1_ = h1 []

text :: forall i. String -> React i
text = Text

p :: forall i. Array (Prop i) -> Array (React i) -> React i
p = NamedElement "p"

p_ :: forall i. Array (React i) -> React i
p_ = p []

button :: forall i. Array (Prop i) -> Array (React i) -> React i
button = NamedElement "button"

button_ :: forall i. Array (React i) -> React i
button_ = button []
