module Database.Backend.Idris.PINQuery

import Database.PowerOfPi
import Database.PINQ
import Database.Backend.Idris.Expr
import System.Random.CrapGen
import Statistics.Distribution.Laplace
import Database.Backend.Idris.Query
import public Database.Backend.PINQuery
%default total

%assert_total
until : (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where
    go x = if p x then x else go (f x)

clamp : Double -> Double
clamp x = if x > 1.0 then 1.0
                     else if x < (-1.0) then (-1.0)
                                        else x

||| Helper function for Laplace noise. Takes a width parameter
||| (e.g. 1/eps) and a uniform random value drawn from [0,1).
lap : Double -> Double -> Double
lap width rx = samplePure 0 width (rx-0.5)

lap' : Double -> CrapGen -> (Double,CrapGen)
lap' width g = let (rx,g') = rndDouble g
                in (samplePure 0 width (rx-0.5), g')

%assert_total
go : Double -> Double -> CrapGen -> (Double,CrapGen)
go width tally g = let (lx,g') = lap' width g
                       cand    = tally + lx
                    in if (-1.0) < cand && cand < 1.0
                       then (cand,g')
                       else go width tally g'
 
instance Aggregation (PINQuery Idris) where
  noisyCount (MkPINQuery q) eps = MkPrivate $ \g => 
    let (noise,g') = lap' (1 / toFloat eps) g
        count      = fromInteger $ fromNat $ length (eval q)
     in (count + noise, g')

  noisyAverage exp (MkPINQuery q) eps = MkPrivate $ \g =>
    let rs      = map (clamp . eval exp) (eval q)
        (tt,ct) = foldl (\(tt,ct),x => (tt+x,ct+1)) (0.0,0.0) rs
        trueAvg = tt / ct
     in if ct == 0 then -- We need to return a value in the range (-1,+1)
                        let (rx,g') = rndDouble g
                         in ((rx - 0.5)*2,g')
                   else -- We need to add Lap. noise
                        go (2 / toFloat eps) trueAvg g

