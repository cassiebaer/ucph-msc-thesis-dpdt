module Rational
import Data.So
%default total

infixr 7 :%
data Rational : Type where
    (:%) : Integer -> Integer -> Rational

gcd : Integer -> Integer -> Integer
gcd 0 0 = 0
gcd x y = gcd' (abs x) (abs y)
  where gcd' a 0 = a
        gcd' a b = assert_total (gcd' b (a `mod` b))

reduce : Integer -> Integer -> Rational
reduce x 0 = x :% 0
reduce x y = case choose (d == 0) of
                  (Left _)  => assert_total ((x `div` d) :% (y `div` d))
                  (Right _) => x :% y
  where d : Integer
        d = gcd x y

signum : Integer -> Integer
signum x = case compare x 0 of
                LT => (-1)
                EQ => 0
                GT => 1

infixr 7 //
(//) : Integer -> Integer -> Rational
(//) x y = reduce (x * signum y) (abs y)

toFloat : Rational -> Float
toFloat (x :% y) = fromInteger x / fromInteger y

instance Num Rational where
    (+) (n :% d) (n' :% d') = reduce (n*d' + n'*d) (d*d')
    (-) (n :% d) (n' :% d') = reduce (n*d' - n'*d) (d*d')
    (*) (n :% d) (n' :% d') = reduce (n*n') (d*d')
    abs (n :% d) = abs n // d
    fromInteger n = n // 1

instance Eq Rational where
    (==) (n :% d) (n' :% d') = n * d' == n' * d

instance Ord Rational where
    compare (n :% d) (n' :% d') = compare (n * d') (n' * d)

