module Database.PINQ.Aggregations
import Effects
import Effect.Random
import Database.PowerOfPi.Query
import Database.PINQ.Types
%default total

StdGen : Type
StdGen = Integer

data Private : Sensitivity -> Type -> Type where
  MkPrivate : (StdGen -> (a,StdGen)) -> Private budget a

||| Sequencing primitive. Allows us to overload Idris' do-notation
||| N.B. It may be necessary to prefix a 'do' with 'with PINQ '
(>>=) : Private s a -> (a -> Private s' b) -> Private (s + s') b
(>>=) (MkPrivate sf) f = MkPrivate $ \st => let (x,st1)       = sf st
                                                MkPrivate sf' = f x
                                             in sf' st1

||| Lifts a value into a Private computation.
return : a -> Private 0 a
return x = MkPrivate $ \s => (x,s)

noisyCount : PINQuery s c -> (e:Epsilon) -> Private (c*e) Double
noisyCount (MkPINQuery x) _ = let count = fromInteger $ fromNat $ length (eval x)
                               in MkPrivate $ \s => (count + ?noise,s)

fakeQuery : PINQuery ["Name":::String] 3

mySeqOfQueries : Private (3//5) Double
mySeqOfQueries = do
  x <- noisyCount fakeQuery (1//10)
  noisyCount fakeQuery (1//10)
  return 5

