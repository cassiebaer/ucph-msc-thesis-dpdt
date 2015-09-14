module Database.PINQ.Idris

import public Data.Rational
import public Database.PowerOfPi.Idris -- TODO : should be public?
import public Database.PINQ

import System.Random.CrapGen
import Statistics.Distribution.Laplace
%default total

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

noisyCount : (PINQuery Table s c) -> (e:Epsilon) -> Private (c*e) Double
noisyCount (MkPINQuery q) eps = MkPrivate $ \g => 
  let (rx,g') = rndDouble g
      noise   = samplePure 0 (1 / toFloat eps) rx
      count   = the Double $ fromInteger $ fromNat $ length (eval q)
   in (count + noise, g')

noisyAverage : Expr s Double -> (PINQuery Table s c) -> (e:Epsilon) -> Private (c*e) Double
noisyAverage exp (MkPINQuery q) eps = MkPrivate $ \g =>
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

