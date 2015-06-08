module Laplace

import Data.Floats
import Data.Double

||| Returns the sign of the given number.
signum : Double -> Double
signum x = case compare x 0 of
                LT => -1
                EQ =>  1 -- assume that zero is positive
                GT =>  1

||| Represents the PDF of a Laplace distribution
|||
||| @mu The center of the dist.
||| @b  The spread of the dist.
||| @x  x
pdf : (mu:Double) -> (b:Double) -> (x:Double) -> Double
pdf mu b x = (1/2/b)*exp(-(abs (x - mu))/b)

||| Represents the CDF of a Laplace distribution
|||
||| @mu The center of the dist.
||| @b  The spread of the dist.
||| @p  p
cdf : (mu:Double) -> (b:Double) -> (p:Double) -> Double
cdf mu b p = 0.5 + 0.5 * signum (p - 0.5) * log (1 - 2 * abs (p - 0.5))

||| Produces a value from the Laplace distribution given
||| a random uniform variable in (-0.5,0.5]
|||
||| @mu The center of the dist.
||| @b  The spread of the dist.
||| @u  A random variable drawn uniformly from (-0.5,0.5]
samplePure : (mu:Double) -> (b:Double) -> (u:Double) -> Double
samplePure mu b u = mu - b * signum u * log (1 - 2 * abs u)

