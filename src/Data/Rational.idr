module Data.Rational

%default total

fromDec : Lazy a -> Lazy a -> Dec b -> a
fromDec y n (Yes _) = Force y
fromDec y n (No  _) = Force n

infixr 7 :%
data Rational : Type where
    (:%) : Nat -> Nat -> Rational

%assert_total
reduce : Nat -> Nat -> Rational
reduce x Z = x :% Z
reduce x y = fromDec yes no (decEq d Z)
  where
    d : Nat
    d = gcd x y
    yes = x :% y
    no  = (x `div` d) :% (y `div` d)

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

instance DecEq Rational where
    decEq x y = if x == y then Yes primEq else No primNotEq
      where
        primEq : x = y
        primEq = believe_me (Refl {x})
        postulate primNotEq : x = y -> Void

