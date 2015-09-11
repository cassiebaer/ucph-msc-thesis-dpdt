module Database.PINQ.Abstract.Types

import Data.Rational
import Database.PowerOfPi
%default total

Epsilon : Type
Epsilon = Rational

Sensitivity : Type
Sensitivity = Rational

Stability : Type
Stability = Rational

data PINQuery : (Schema -> Type) -> Schema -> Stability -> Type  where
  MkPINQuery : Query b s -> PINQuery b s c

data PINGrouping : (Schema -> Type) -> Schema -> Type -> Stability -> Type where
  MkPINGrouping : Grouping t s k -> PINGrouping t s k c

