module Effect.Random.Extended

import Effects
import Effect.Random

||| Yields a random double between provided bounds.
partial
rndDouble : Double -> Double -> { [RND] } Eff Double
rndDouble lower upper = do zi <- rndInt 0 maxInt
                           let x = fromInteger zi / fromInteger maxInt
                           return (x * (upper - lower) + lower)
  where maxInt : Integer
        maxInt = 4000000000

