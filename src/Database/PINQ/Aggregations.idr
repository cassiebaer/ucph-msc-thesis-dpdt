module Database.PINQ.Aggregations
import Effects
import Effect.Random
import Database.PowerOfPi.Query
import Database.PINQ.Types
%default total

data Private : Sensitivity -> Type -> Type where
  MkPrivate : a -> Private budget a

||| Sequencing primitive. Allows us to overload Idris' do-notation
||| N.B. It may be necessary to prefix a 'do' with 'with PINQ '
(>>=) : Private s a -> (a -> Private s' b) -> Private (s + s') b
(>>=) (MkPrivate x) f = let MkPrivate x' = f x in MkPrivate x'

noisyCount : PINQuery s c -> (e:Epsilon) -> Private (c*e) Double
noisyCount (MkPINQuery x) _ = let count = fromInteger $ fromNat $ length (eval x)
                               in MkPrivate (count + ?noise)

fakeQuery : PINQuery ["Name":::String] 3

runPrivate : Private c a -> Eff a [RND]

mySeqOfQueries : Private (3//5) Double
mySeqOfQueries = with Aggregations do
  noisyCount fakeQuery (1//10)
  noisyCount fakeQuery (1//10)

