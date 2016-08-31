-- TODO consider using divmod, rather than unsafeDivmod


module BigRatio
    exposing
        ( BigRational
        , gcd
        , add
        , subtract
        , multiply
        , divide
        , negate
        , over
        , denominator
        , numerator
        , split
        , toFloat
        , toDecimal
        , isNegative
        , fromInt
        , fromString
        , zero
        )

{-| A module providing a ratio type for big rational numbers. Forked from Izzy Meckler's [Ratio](https://github.com/imeckler/ratio) and dependent on Javier Casas' [elm-integer](https://github.com/javcasas/elm-integer).

will@willwhite.website

# Types
@docs BigRational

# Introduction
@docs over

# From/To
@docs fromInt, fromString, toFloat, toDecimal

# Operations
@docs add, subtract, multiply, divide, negate

# Elimination
@docs numerator, denominator, split

# Util
@docs gcd

# Value
@docs isNegative

# Common numbers
@docs zero

-}

import Basics exposing (..)
import Array exposing (..)
import Data.Integer as I exposing (..)


-- String is part of a HACK:

import String


{-| Arbitrary (infinite digits) precision fractional numbers. Think of
    it as the length of a rigid bar that you've constructed from a bunch of
    initial bars of the same fixed length
    by the operations of gluing bars together and shrinking a
    given bar so that an integer number of copies of it glues together to
    make another given bar.
-}
type BigRational
    = BigRatio Integer Integer


{-| The biggest number that divides both arguments (the greatest common divisor).
-}
gcd : Integer -> Integer -> Integer
gcd a b =
    if b `eq` I.zero || b `eq` minusZeroFromUnsafeDivmod then
        a
    else
        gcd b (snd (a `unsafeDivmod` b))



-- b will never `eq` zero or minusZero in the else branch, so unsafeDivmod will always return (Integer, Integer)


minusZeroFromUnsafeDivmod : Integer
minusZeroFromUnsafeDivmod =
    snd (I.fromInt -3 `I.unsafeDivmod` I.fromInt 1)



-- NOTE minusZeroFromUnsafeDivmod returns a minusZero with no magnitude, whereas I.fromString "-0" returns a minusZero with a magnitude of 0.
-- > snd (I.fromInt -3 `I.unsafeDivmod` I.fromInt 1)
-- Integer (Negative,Magnitude []) : Data.Integer.Integer
-- > I.fromString "-0"
-- Just (Integer (Negative,Magnitude [0])) : Maybe.Maybe Data.Integer.Integer
-- `eq` says they are the same:
-- > (Maybe.withDefault I.one (I.fromString "-0")) `I.eq` (snd (I.fromInt -3 `I.unsafeDivmod` I.one))
-- True : Bool
-- I don't know if there's an important difference between using either (other than the (TEST: difference) below), but I am using minusZeroFromUnsafeDivmod, rather than I.fromString "-0", as `unsafeDivmod` (Integer (Negative,Magnitude [])) was breaking gcd as it was. (numbers used below as shorthand for Integers):
-- > minusthree = Data.Integer.fromInt -3
-- Integer (Negative,Magnitude [3]) : Data.Integer.Integer
-- > ten = Data.Integer.fromInt 10
-- Integer (Positive,Magnitude [10]) : Data.Integer.Integer
-- > minusTenOverThree = ten `BigRatio.over` minusthree
-- ^C
-- willwhite (master *) share $  Computation interrupted, any definitions were not completed.
-- > t
-- elm-repl: <stdin>: hGetChar: hardware fault (Input/output error)
-- n^C
-- willwhite (master *) share $
{- gcd -3 1 =
     if 1 `eq` I.zero then
         -3
     else
         gcd 1 (snd (-3 `unsafeDivmod` 1)) -> gcd 1 -0

   gcd 1 -0 =
       if -0 `eq` I.zero then -- (it doesn't)
           1
       else
           gcd -0 (snd (1 `unsafeDivmod` -0)) -- breaks

-}
-- TEST (difference):
--> (I.fromInt -3) `I.unsafeDivmod` (Maybe.withDefault I.one (I.fromString "-0"))
-- (Integer (Positive,Magnitude [97151,97153,2]),Integer (Negative,Magnitude [3])) : ( Data.Integer.Integer, Data.Integer.Integer )
-- > (I.fromInt -3) `I.unsafeDivmod` (snd((I.fromInt -3) `I.unsafeDivmod` (I.one)))
-- (Integer (Positive,Magnitude [97151,97153,97153,2]),Integer (Negative,Magnitude [3])) : ( Data.Integer.Integer, Data.Integer.Integer )
-- TEST (no difference):
-- > minusthree = I.fromInt -3
-- Integer (Negative,Magnitude [3]) : Data.Integer.Integer
-- > ten = I.fromInt 10
-- Integer (Positive,Magnitude [10]) : Data.Integer.Integer
-- > minusTenOverThree = ten `B.over` minusthree
-- BigRatio (Integer (Negative,Magnitude [10])) (Integer (Positive,Magnitude [3]))
--     : BigRatio.BigRational
-- > minusTenOverThree = ten `B.over` minusthree (with (Maybe.withDefault I.one (I.fromString "-0")))
-- BigRatio (Integer (Negative,Magnitude [10])) (Integer (Positive,Magnitude [3]))
--     : BigRatio.BigRational
-- TEST (no difference):
-- > minusten = I.fromInt -10
-- Integer (Negative,Magnitude [10]) : Data.Integer.Integer
-- > minusten `B.over` minusthree
-- BigRatio (Integer (Negative,Magnitude [10])) (Integer (Negative,Magnitude [3]))
--     : BigRatio.BigRational
-- > minusten `B.over` minusthree (with (Maybe.withDefault I.one (I.fromString "-0")))
-- BigRatio (Integer (Negative,Magnitude [10])) (Integer (Negative,Magnitude [3]))
--     : BigRatio.BigRational


{-| E.g. 4/8 -> 1/2
-}
normalize : BigRational -> BigRational
normalize (BigRatio p q) =
    if (p `eq` I.zero || p `eq` minusZeroFromUnsafeDivmod) && (q `eq` I.zero || q `eq` minusZeroFromUnsafeDivmod) then
        BigRatio p q
    else
        let
            k =
                (gcd p q)
                    `mul` (if q `lt` I.zero then
                            minusOne
                           else
                            one
                          )
        in
            BigRatio (fst (p `unsafeDivmod` k)) (fst (q `unsafeDivmod` k))



-- k can't be 0 as gcd cannot return 0 unless gcd 0 0 and p and q can't be zero or minusZero in the else branch, and the conditional expression in the let expression only returns one or minusOne.


{-| Addition. It's like gluing together two bars of the given lengths.
-}
add : BigRational -> BigRational -> BigRational
add (BigRatio a b) (BigRatio c d) =
    normalize (BigRatio ((a `mul` d) `I.add` (b `mul` c)) (b `mul` d))


{-| Subtract
-}
subtract : BigRational -> BigRational -> BigRational
subtract a b =
    add a (negate b)


{-| Multiplication. `multiply x (c / d)` is the length of the bar that you'd get
   if you glued `c` copies of a bar of length `x` end-to-end and then shrunk it
   down enough so that `d` copies of the shrunken bar would fit in the big
   glued bar.
-}
multiply : BigRational -> BigRational -> BigRational
multiply (BigRatio a b) (BigRatio c d) =
    normalize (BigRatio (a `mul` c) (b `mul` d))


{-| Division. It's sort of like multiplication!
-}
divide : BigRational -> BigRational -> BigRational
divide r (BigRatio c d) =
    multiply r (BigRatio d c)


{-| This doesn't really fit with the bar metaphor but this is multiplication by `-1`.
-}
negate : BigRational -> BigRational
negate (BigRatio a b) =
    BigRatio (a `mul` I.minusOne) b


{-| `over x y` is like `x / y`.
-}
over : Integer -> Integer -> BigRational
over x y =
    normalize (BigRatio x y)


{-| `fromInt x = over x 1`
-}
fromInt : Int -> BigRational
fromInt x =
    I.fromInt x `over` one


{-|
-}
fromString : String -> BigRational
fromString x =
    -- TODO upgrade to return Maybe BigRational
    let
        -- 1) convert String to two Strings (numerator and denominator) representing Ints:
        -- "0.35" -> "35", "100"
        -- "1.35" -> "135", "100"
        -- "11.35" -> "1135", "100"
        -- "0.05" -> "5", "100"
        -- "12" -> "12", "1" (or don't do non-decimals?)
        numerator =
            -- numerator: from first non-decimal point character, to last, excluding decimal point: "0.35" -> "035", "1.35" -> "135", "11.35" -> "1135", "0.05" -> "005", "12" -> "12"
            -- HACK numerator relies on Data.Integer's parsing:
            -- > Data.Integer.fromString "005"
            -- Just (Integer (Positive,Magnitude [5])) : Maybe.Maybe Data.Integer.Integer
            String.filter (\character -> character /= '.') x

        denominator =
            -- denominator: from first character after decimal point, to last character: "0.35" -> "100", "1.35" -> "100", "11.35" -> "100", "0.05" -> "100", "12" -> "1"
            let
                wholeAndDecimalParts =
                    String.split "." x

                decimalPart =
                    Maybe.withDefault "" (Array.get 1 (Array.fromList wholeAndDecimalParts))
            in
                String.cons '1' (String.map (\_ -> '0') decimalPart)
    in
        -- 2) convert the two Strings to two Data.Integers
        -- 3) convert the two Data.Integers to a BigRational
        (Maybe.withDefault I.zero (I.fromString numerator)) `over` (Maybe.withDefault I.one (I.fromString denominator))


{-| -}
numerator : BigRational -> Integer
numerator (BigRatio a _) =
    a


{-| -}
denominator : BigRational -> Integer
denominator (BigRatio _ b) =
    b


{-| `split x = (numerator x, denominator x)`
-}
split : BigRational -> ( Integer, Integer )
split (BigRatio a b) =
    ( a, b )


{-| -}



-- TODO upgrade toFloat to return Maybe Float, in case conversion breaks


toFloat : BigRational -> Float
toFloat (BigRatio a b) =
    -- Basics.toFloat a / Basics.toFloat b
    -- original above, below is a HACK?: instead, make "toInt : Integer -> Int" for elm-integer, then use Basics.toFloat : Int -> Float?
    Result.withDefault 0 (String.toFloat (I.toString a)) / Result.withDefault 1 (String.toFloat (I.toString b))


{-|
    1 -> (BigRatio 1 4) -> "0.2"
    100 -> (BigRatio 1 4) -> "0.25"
-}
toDecimal : Int -> BigRational -> String



-- TODO integrate this line from Fraction.hs: decimal _ (u:-:0) = putStr (show u++"//0")


toDecimal digits x =
    -- as per Fraction.hs decimal http://code.haskell.org/~thielema/numeric-quest/Fraction.hs
    let
        num =
            numerator x

        den =
            denominator x

        ( u, v ) =
            Maybe.withDefault ( I.zero, I.zero {- TODO sensible default -} ) <| num `divmod` {- NOTE Data.Integer.divmod v2.0.2 does quotRem (//, rem), not divMod (//, %) https://github.com/javcasas/elm-integer/issues/4 -} den

        g y z m str =
            case m of
                0 ->
                    str

                _ ->
                    let
                        ( p, q ) =
                            -- ( (//) y z, rem y z )
                            Maybe.withDefault ( I.zero, I.zero {- TODO sensible default -} ) <| y `divmod` {- NOTE Data.Integer.divmod v2.0.2 does quotRem (//, rem), not divMod (//, %) https://github.com/javcasas/elm-integer/issues/4 -} z
                    in
                        if q `eq` I.zero {- remainder 0 -} then
                            str ++ I.toString p
                        else
                            g (q `mul` (I.fromInt 10)) z (m - 1) (str ++ I.toString p)
    in
        if digits <= 0 then
            toDecimal 1 x
        else if isNegative x {- HACK original: x < 0 -} then
            g ((I.negate v) `mul` I.fromInt 10) den digits ("-" ++ I.toString (I.negate u) ++ ".")
        else
            g (v `mul` (I.fromInt 10)) den digits (I.toString u ++ ".")


{-| -}
isNegative : BigRational -> Bool
isNegative (BigRatio a b) =
    xor (I.sign a == Negative) (I.sign b == Negative)


{-| -}
zero : BigRational
zero =
    fromInt 0
