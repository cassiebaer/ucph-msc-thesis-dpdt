module Database.PINQ.Aggregations
import Database.PINQ.Types
import Database.PINQ.PINQuery
import Statistics.Distribution.Laplace
import System.Random.CrapGen
%default total

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

||| Returns the number of records in a query with noise drawn from 
||| the Laplace distribution scaled according to stability and sensitivity.
noisyCount : PINQuery s c -> (e:Epsilon) -> Private (c*e) Double
noisyCount pinq e = MkPrivate $ \g => let (rx,g') = rndDouble g
                                          noise   = samplePure 0 (1 / toFloat e) (rx-0.5)
                                          count   = fromInteger $ fromNat $ length (eval (getQuery pinq))
                                       in (count + noise, g')

