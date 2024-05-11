module Org.Test.Test03c where


import Data.Text.Format.Org.Construct as Org
import Data.Text.Format.Org.Types (OrgFile, RepeaterMode(..), Interval(..))
import Prelude (($), (#))


test :: OrgFile
test =
    Org.f
        $ Org.ds1
            $ Org.schedule (Org.adatetime (Org.d 2024 5 10) (Org.t 22 13))
            $ Org.sec1 1 (Org.text "We have a schedule here") $
                Org.ds1
                    $ Org.deadline (Org.adatetime (Org.d 2024 5 11) (Org.t 18 12))
                    $ Org.sec1 2 (Org.text "And some deadline, oh no") $
                        Org.ds1
                            $ Org.schedule (Org.adatetime (Org.d 2024 5 10) (Org.t 22 45))
                            $ Org.deadline (Org.repeat Single 1 Week $ Org.adate $ Org.d 2024 5 11)
                            $ Org.sece1 3 ( Org.text "And here we have both schedule and deadline, why not?" )