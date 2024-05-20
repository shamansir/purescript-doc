module Org.Test.Test04h where


import Prelude (($), (#))

import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Types (ListType(..))
import Data.Text.Format.Org.Construct as Org


test :: OrgFile
test =
    Org.f
        $ Org.ds
            [ Org.sec1 1 (Org.text "Node properties")
                    (Org.db
                        [ Org.blank -- FIXME:
                        , Org.para1 $ Org.text "The blank lines above are considered a part of a drawer"
                        , Org.list Hyphened
                            [

                            ]
                        ]
                    )
                # Org.wprop "NAME" "VALUE"
                # Org.wprop_ "NAME"
                # Org.wprop "NAME+" "VALUE"
                # Org.wprop_ "NAME+"
                # Org.drawer "drawer" [ Org.text "Text." ]
            ]