module Database.PINQ.Types
import public Data.Rational
import public Database.PowerOfPi.Types
import Database.PowerOfPi.Expr
import Database.PowerOfPi.Query
%default total

Epsilon : Type
Epsilon = Rational

Sensitivity : Type
Sensitivity = Rational

Stability : Type
Stability = Rational

data PINQuery : Schema -> Stability -> Type where
  MkPINQuery : Query s -> PINQuery s c

data PINQueryAggregation : Schema -> a -> Sensitivity -> Type where
  MkPINQueryAggregation : QueryAggregation s a -> PINQueryAggregation s a c

------------------------------------------------------------------------------
-- Aggregations (move to sep. file)
------------------------------------------------------------------------------

NoisyCount : PINQuery s c -> (e:Epsilon) -> PINQueryAggregation s Double (c*e)

------------------------------------------------------------------------------
-- Transformations (move to sep. file)
------------------------------------------------------------------------------

-- Select has 1-stability
Select : Expr s Bool -> PINQuery s c -> PINQuery s c

-- Projection has 1-stability
Projection : (f:String -> Maybe String)
          -> PINQuery s c -> PINQuery (projectedSchema f s) c

