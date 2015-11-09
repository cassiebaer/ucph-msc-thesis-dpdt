module Database.DPDT.Idris

import public Data.Rational
import Data.Vect
import public Database.DPDT
import public Database.PowerOfPi.Idris

import Statistics.Distribution.Laplace
import public System.Random.CrapGen
%default total

Query : Schema -> Stability -> Type
Query = Query ListRow

Grouping : Schema -> Type -> Stability -> Type
Grouping = Grouping ListRow

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
(>>=) : Private s a -> (a -> Private s' b) -> Private (s + s') b
(>>=) (MkPrivate sf) f = MkPrivate $ \g => let (x,g')        = sf g
                                               MkPrivate sf' = f x
                                            in sf' g'

sequence : Vect n (Private s a) -> Private (n * s) (Vect n a)
sequence {s} [] = return []
sequence {n} ps = MkPrivate $ \g =>
  let gs = map snd (unfoldCrapGenN (S n) g)
      vs = (map evalPrivate ps) <*> (init gs)
   in (vs, last gs)

||| Clamps a value to [-1.0,+1.0]
clamp : Double -> Double
clamp x = if x > 1.0 then 1.0
                     else if x < (-1.0) then (-1.0)
                                        else x

||| Computes the bounds required on the uniform variable to satisfy the
||| constraint that `-1 < tally + noise < 1` where noise is Laplace
bounds : Double -> Double -> (Double,Double)
bounds width tally = let lb = cdf 0 width (-1 - tally)
                         ub = cdf 0 width ( 1 - tally)
                      in (lb,ub)

namespace Query

  Query : Schema -> Stability -> Type
  Query = Query ListRow


  noisyCount : (Query ListRow s c) -> (e:Epsilon) -> Private (c*e) Double
  noisyCount (MkQuery q) eps = MkPrivate $ \g =>
    let (rx,g') = rndDouble g
        noise   = samplePure 0 (1 / toFloat eps) rx
        count   = the Double $ fromInteger $ fromNat $ length (eval q)
     in (count + noise, g')

  noisyAverage : Expr s Double -> (Query ListRow s c) -> (e:Epsilon) -> Private (c*e) Double
  noisyAverage exp (MkQuery q) eps = MkPrivate $ \g =>
    let rs      = map (clamp . eval exp) (eval q)
        (tt,ct) = foldl (\(tt,ct),x => (tt+x,ct+1)) (0.0,0.0) rs
        trueAvg = tt / ct
     in if ct == 0 then -- We need to return a value in the range (-1,+1)
                        let (rx,g') = rndDouble g
                         in ((rx - 0.5)*2,g')
                   else -- We need to add Lap. noise
                        let width   = 2 / toFloat eps
                            (lb,ub) = bounds width trueAvg
                            (rx,g') = rndDouble g
                            noise   = samplePure 0 width (rx * (ub-lb) + lb)
                         in (trueAvg + noise,g')

namespace Grouping

  Grouping : Schema -> (k:Type) -> Stability -> Type
  Grouping = Grouping ListRow

  noisyCount : (Grouping ListRow s k c) -> (e:Epsilon) -> Private (c*e) Double
  noisyCount (MkGrouping q) eps = MkPrivate $ \g =>
    let (rx,g') = rndDouble g
        noise   = samplePure 0 (1 / toFloat eps) rx
        count   = the Double $ fromInteger $ fromNat $ length (eval q)
     in (count + noise, g')

------------------------------------------------------------------------------

returnIsFree : { p1 : Private s1 Double } -> { p2 : Private s2 Double }
            -> do { x <- p1; return 1.0 } = p2 -> s1 = s2
returnIsFree Refl = Refl

sequencingIsAdditive : { p1 : Private s1 a } -> { p2 : Private s2 a }
                    -> { p3 : Private s3 (a,a)  }
                    -> do { x <- p1; y <- p2; return (x,y) } = p3 -> s1 + s2 = s3
sequencingIsAdditive Refl = Refl

