module Text.Hquery.Internal.Dsl where
{- | DSL for selectors.

## "theId" >. "theClass" #$ "onclick" @= "some javascript"
#. "aClass" >. "anotherClass" #$ "class" @+ "hidden floatLeft"
#* #% "all content replaced with this"
## "theId" #! "entire id is replaced with this"
## "theId" #% "theId stays, content is inserted as child"
#<> "div" #% "all divs have this inserted"

-}
import Text.Hquery.Internal.Selector

class Selectable
