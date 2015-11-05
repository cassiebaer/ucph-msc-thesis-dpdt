module Data.Rational

import Data.So
%default total

infixr 7 :%
data Rational : Type where
    (:%) : Nat -> Nat -> Rational

%assert_total
reduce : Nat -> Nat -> Rational
reduce x Z = x :% Z
reduce x y = case choose (d == 0) of
                  (Left _)  => x :% y
                  (Right _) => assert_total ((x `div` d) :% (y `div` d))
  where d : Nat
        d = gcd x y

infixr 7 //
(//) : Nat -> Nat -> Rational
(//) = reduce

toFloat : Rational -> Float
toFloat (x :% y) = cast x / cast y
  where cast = fromInteger . fromNat

fromNat : Nat -> Rational
fromNat n = fromNat n // 1

instance Show Rational where
    show (x :% y) = parens (show x ++ " / " ++ show y)
      where parens x = "(" ++ x ++ ")"

instance Num Rational where
    (+) (n :% d) (n' :% d') = reduce (n*d' + n'*d) (d*d')
    (-) (n :% d) (n' :% d') = reduce (n*d' - n'*d) (d*d')
    (*) (n :% d) (n' :% d') = reduce (n*n') (d*d')
    abs (n :% d) = abs n // d
    fromInteger n = fromInteger n // 1

instance Eq Rational where
    (==) (n :% d) (n' :% d') = n * d' == n' * d

instance Ord Rational where
    compare (n :% d) (n' :% d') = compare (n * d') (n' * d)

