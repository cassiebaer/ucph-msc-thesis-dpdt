module Data.Rational

%default total

infix 7 :%
infix 7 //

-- Eliminates a Dec b
fromDec : Lazy a -> Lazy a -> Dec b -> a
fromDec y n (Yes _) = y
fromDec y n (No  _) = n

data Rational : Type where
  (:%) : Nat -> (y:Nat) -> Rational

reduce : Nat -> (y:Nat) -> Rational
reduce x Z = x :% Z
reduce x y = let d = gcd x y
              in fromDec (x :% y)
                         ((divNatNZ x d gcdIsNotZ) :% (divNatNZ y d gcdIsNotZ))
                         (decEq d Z)
  where
    gcdIsNotZ : {x,y : Nat} -> Not (gcd x y = Z)
    gcdIsNotZ Refl impossible

(//) : Nat -> (y:Nat) -> Rational
(//) = reduce

toFloat : (x:Rational) -> Float
toFloat (x :% y) = cast x / cast y
  where cast = fromInteger . fromNat

implicit
fromNat : Nat -> Rational
fromNat n = (//) (fromNat n) (S Z)

ratFromInteger : Integer -> Rational
ratFromInteger n = fromInteger n // 1

instance Show Rational where
    show (x :% y) = parens (show x ++ "//" ++ show y)
      where parens x = "(" ++ x ++ ")"

ratPlus : Rational -> Rational -> Rational
ratPlus (n :% d) (n' :% d') = reduce (n*d' + n'*d) (d*d')

ratMinus : Rational -> Rational -> Rational
ratMinus (n :% d) (n' :% d') = reduce (n*d' - n'*d) (d*d')

ratMult : Rational -> Rational -> Rational
ratMult (Z :% _) _          = ratFromInteger 0
ratMult (n :% d) (n' :% d') = reduce (n*n') (d*d')

ratEq : Rational -> Rational -> Bool
ratEq (n :% d) (n' :% d') = n * d' == n' * d

ratDecEq : (x:Rational) -> (y:Rational) -> Dec (x = y)
ratDecEq x y = if ratEq x y then Yes primEq else No primNotEq
  where
    primEq : x = y
    primEq = believe_me (Refl {x})
    postulate primNotEq : x = y -> Void

ratComp : Rational -> Rational -> Ordering
ratComp (n :% d) (n' :% d') = compare (n * d') (n' * d)

instance Num Rational where
    (+) = ratPlus
    (-) = ratMinus
    (*) = ratMult
    abs = id -- Our rationals can only be positive.
    fromInteger = ratFromInteger

instance Eq Rational where
    (==) = ratEq

instance Ord Rational where
    compare = ratComp

instance DecEq Rational where
    decEq = ratDecEq

------------------------------------------------------------------------------
-- Rationals Properties

fromDec_reduces : (d:Dec b) -> fromDec Z Z d = Z
fromDec_reduces (Yes _) = Refl
fromDec_reduces (No  _) = Refl

rationalsReduce : (4//8) = 1//2
rationalsReduce = Refl

rationalsReduceWhenAdded : (1//4) + (1//2) = (3//4)
rationalsReduceWhenAdded = Refl

rationalsReduceWhenSubtracted : (1//2) - (1//4) = (1//4)
rationalsReduceWhenSubtracted = Refl

rationalsReduceWhenMultiplied : (1//2) * 2 = 1
rationalsReduceWhenMultiplied = Refl

fromNatReduces : Data.Rational.fromNat (S Z) = (1//1)
fromNatReduces = Refl

ratMultZeroLeft : (right : Rational) -> 0 * right = 0
ratMultZeroLeft r = Refl

