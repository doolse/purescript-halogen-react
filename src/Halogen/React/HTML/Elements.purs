module Halogen.React.HTML where

import Halogen.React (Prop, React(NamedElement, Text))

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

table :: forall i. Array (Prop i) -> Array (React i) -> React i
table = NamedElement "table"

thead :: forall i. Array (Prop i) -> Array (React i) -> React i
thead = NamedElement "thead"

tbody :: forall i. Array (Prop i) -> Array (React i) -> React i
tbody = NamedElement "tbody"

th :: forall i. Array (Prop i) -> Array (React i) -> React i
th = NamedElement "th"

tr :: forall i. Array (Prop i) -> Array (React i) -> React i
tr = NamedElement "tr"

td :: forall i. Array (Prop i) -> Array (React i) -> React i
td = NamedElement "td"
