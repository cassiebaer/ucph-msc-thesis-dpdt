module Database.PINQ.Abstract.Aggregations

import Data.Rational
import Database.PowerOfPi
import Database.PINQ.Abstract.Types
import Statistics.Distribution.Laplace
import System.Random.CrapGen

||| Represents a Private computation
data Private : Sensitivity -> Type -> Type where
  MkPrivate : (CrapGen -> (a,CrapGen)) -> Private budget a

||| Evaluates a Private computation
evalPrivate : Private s a -> CrapGen -> a
evalPrivate (MkPrivate f) g = fst (f g)

||| Lifts a value into a Private computation.
return : a -> Private 0 a
return x = MkPrivate $ \s => (x,s)

||| Sequencing primitive. Allows us to overload Idris' do-notation
||| N.B. It may be necessary to prefix a 'do' with 'with PINQ '
(>>=) : Private s a -> (a -> Private s' b) -> Private (s + s') b
(>>=) (MkPrivate sf) f = MkPrivate $ \st => let (x,st1)       = sf st
                                                MkPrivate sf' = f x
                                            in sf' st1

||| Represents differentially private aggregations
data Aggregation : Backend -> Sensitivity -> Type where
 -- (>>=)        : Aggregation b c a -> Aggregation b c' a ->         Aggregation b (c+c') a
  NoisyCount   : PINQuery b s c -> (e:Epsilon) ->                   Aggregation b (c*e) 
  NoisyAverage : PINQuery b s c -> (e:Epsilon) -> Expr s Double ->  Aggregation b (c*e)
