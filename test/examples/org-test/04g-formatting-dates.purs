module Org.Test.Test04g where


import Prelude (($))

import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Construct as Org

import Data.Time (Time)
import Data.Date (Date, canonicalDate)


test :: OrgFile
test =
    Org.f
        $ Org.db
            [ Org.para1 $ Org.at $ Org.adatetime (Org.d 1997 11 3) (Org.t 19 15)
            ]