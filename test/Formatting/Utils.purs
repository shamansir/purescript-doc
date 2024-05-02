module Test.Formatting.Utils where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.FoldableWithIndex (foldlWithIndex)

import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)



helper :: forall a. { title :: Int -> a /\ String -> String, render :: a -> String } -> Array (a /\ String) -> Spec Unit
helper { title, render } =
    foldlWithIndex
        (\idx prev (tag /\ expected) -> do
            prev
            *>
            (it ("works for sample " <> title idx (tag /\ expected)) $
                render tag `shouldEqual` expected
            )
        )
        (pure unit)