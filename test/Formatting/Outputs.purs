module Test.Formatting.Outputs where

import Prelude

import Type.Proxy (Proxy)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Text.Output as O
import Data.Text.Format as F
import Data.Text.Doc as D

import Data.Text.Output.Blessed as O
import Data.Text.Output.Html as O
import Data.Text.Output.Markdown as O
import Data.Text.Output.Org as O

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Test.Utils (helper)


options =
    { break : D.All, indent : D.Spaces 2 }


spec :: Spec Unit
spec = do

  describe "Formatting works properly for different outputs" $ do

    helper
        { title : \idx _ -> "Example " <> show idx
        , spec : \(example /\ output)-> do
            O.perform O.blessed options example `shouldEqual` output.blessed
            O.perform O.html options example `shouldEqual` output.html
            O.perform O.markdown options example `shouldEqual` output.markdown
            O.perform O.org options example `shouldEqual` output.org
        }
        outputsExamples


outputsExamples :: Array (F.Tag /\ { html :: String, markdown :: String, blessed :: String, org :: String })
outputsExamples =
    [ F.s "foo" /\
        { html : "foo", markdown : "foo", blessed : "foo", org : "foo" }
    , F.bolds "foo" /\
        { html : "<b>foo</b>", markdown : "**foo**", blessed : "{bold}foo{/bold}", org : "*foo*" }
    , F.code "javascript" "function() { return 42; }" /\
        { html :
            """<code lang="javascript">function() { return 42; }</code>"""
        , markdown :
"""```javascript
function() { return 42; }
```"""
        , blessed :
            "function() { return 42; }"
        , org :
"""#+BEGIN_SRC javascript
function() { return 42; }
#+END_SRC"""
        }
    ]