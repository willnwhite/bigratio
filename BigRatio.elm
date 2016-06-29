module BigRatio
    exposing
        ( gcd
        , add
        , multiply
        , divide
          -- , negate
        , BigRational
        , over
          -- , denominator
          -- , numerator
          -- , split
          -- , toFloat
          -- , fromInt
        )

{-| A simple module providing a ratio type for rational numbers

# Types
@docs Rational

# Introduction
@docs over, fromInt

# Operations
@docs add, multiply, divide, negate

# Elimination
@docs numerator, denominator, split, toFloat

# Util
@docs gcd

-}

import Basics exposing (..)
import Data.Integer exposing (..)


{-| "Arbitrary" (up to `max_int` size) precision fractional numbers. Think of
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
    if b == zero then
        -- TODO ask Data.Integer's author: use Data.Integer.eq instead of ==? If so, replace == wherever it's not appropriate.
        a
    else
        gcd b (snd (a `unsafeDivmod` b))



-- b can't be 0 in the else branch (see the if branch), so Data.Integer.unsafeDivmod will always return (Integer, Integer)


{-| E.g. 4/8 -> 1/2
-}
normalize : BigRational -> BigRational
normalize (BigRatio p q) =
    if p == zero && q == zero then
        BigRatio p q
    else
        let
            k =
                (gcd p q)
                    `mul` (if q `lt` (fromInt 0) then
                            fromInt -1
                           else
                            fromInt 1
                          )
        in
            BigRatio (fst (p `unsafeDivmod` k)) (fst (q `unsafeDivmod` k))



-- k can't be 0 as gcd cannot return 0 unless gcd 0 0 and p and q can't be 0 in the else branch, and the conditional expression does not return 0


{-| Addition. It's like gluing together two bars of the given lengths.
-}
add : BigRational -> BigRational -> BigRational
add (BigRatio a b) (BigRatio c d) =
    normalize (BigRatio ((a `mul` d) `Data.Integer.add` (b `mul` c)) (b `mul` d))


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



{-
   {-| This doesn't really fit with the bar metaphor but this is multiplication by `-1`.
   -}
   negate : Rational -> Rational
   negate (Ratio a b) =
       Ratio (-a) b


   {-| `over x y` is like `x / y`.
   -}
-}


over : Integer -> Integer -> BigRational
over x y =
    normalize (BigRatio x y)



{-


   {-| `fromInt x = over x 1`
   -}
   fromInt : Int -> Rational
   fromInt x =
       x `over` 1


   {-| -}
   numerator : Rational -> Int
   numerator (Ratio a _) =
       a


   {-| -}
   denominator : Rational -> Int
   denominator (Ratio _ b) =
       b


   {-| `split x = (numerator x, denominator x)`
   -}
   split : Rational -> ( Int, Int )
   split (Ratio a b) =
       ( a, b )


   {-| -}
   toFloat : Rational -> Float
   toFloat (Ratio a b) =
       Basics.toFloat a / Basics.toFloat b
-}
