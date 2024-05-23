module Org.Test.Test04i where


import Prelude (($), (#))

import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Types (ListType(..))
import Data.Text.Format.Org.Construct as Org


test :: OrgFile
test =
    Org.f
        $ Org.ds
            [ Org.sece1 1 (Org.text "")
                # Org.drawer1 "drawer" (Org.text "Text.")
            , Org.sec 1
                    [ Org.text "Heading title is a part of the headline element itself <BEGIN>"
                    , Org.br, Org.br
                    , Org.text "Text inside heading is considered a part of its CONTENTS and can"
                    , Org.text "contain other elements recursively.  This paragraph only has CONTENTS,"
                    , Org.text "no BEGIN, no END, and a BLANK line."
                    ]
                (Org.db1 $ Org.para1 $ Org.text "This is the end of the heading, no END exists for headings."
                )
                # Org.drawer "drawer"
                    [ Org.text "The same works at the deeper levels, with this drawer having", Org.br
                    , Org.text "=:drawer:= line as BEGIN, this paragraph belonging to drawer CONTENTS,"
                    , Org.text "=:end:= representing END, and no BLANK after."
                    ]
            ]