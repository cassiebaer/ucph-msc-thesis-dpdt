module Rational
%default total

infixr 7 :%
data Rational : Type where
  (:%) : Integer -> Integer -> Rational

%assert_total
gcd : Integer -> Integer -> Integer
gcd x y = gcd' (abs x) (abs y)
  where gcd' a 0 = a
        gcd' a b = gcd' b (a `mod` b)

%assert_total
reduce : Integer -> Integer -> Rational
reduce x 0 = x :% 0
reduce x y = (x `div` d) :% (y `div` d)
  where d = gcd x y

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

