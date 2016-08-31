module Test exposing (..)

import List exposing (reverse, length)
import Check exposing (Claim, Evidence, suite, claim, that, is, for, quickCheck)
import Check.Producer exposing (list, int)
import Check.Test
import ElmTest


myClaims : Claim
myClaims =
    suite "fromString"
        [-- claim "Reversing a list twice yields the original list"
         --     `that` (\list -> reverse (reverse list))
         --     `is` identity
         --     `for` list int
         -- , claim "Reversing a list does not modify its length"
         --     `that` (\list -> length (reverse list))
         --     `is` (\list -> length list)
         --     `for` list int
        ]



-- 10/3 + 10/3 + 10/3 = 10
-- i.e. no loss of information when dividing
-- (number divided by other_number) added to itself (other_number - 1) times equals number


evidence : Evidence
evidence =
    quickCheck myClaims


main =
    ElmTest.runSuite (Check.Test.evidenceToTest evidence)
