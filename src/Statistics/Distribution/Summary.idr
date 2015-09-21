module Statistics.Distribution.Summary

||| Prints a summary of the list of floats
putSummary : List Float -> IO ()
putSummary xs = do
  let xs = sort xs
  let (sum,n) = the (Pair Double Nat) $ foldl (\(sum,n),x => (sum+x,n+1)) (0,0) xs
  let max = foldl1 (\acc,x => if x > acc then x else acc) xs
  let min = foldl1 (\acc,x => if x < acc then x else acc) xs
  let mean = sum / (fromInteger $ fromNat n)
  let Just lb = (n `div` 20) `List.index'` xs
  let Just ub = ((n `div` 20) * 19) `List.index'` xs
  putStrLn $ "n:    " ++ show n
  putStrLn $ "05%:  " ++ show lb
  putStrLn $ "95%:  " ++ show ub
  putStrLn $ "max:  " ++ show max
  putStrLn $ "min:  " ++ show min
  putStrLn $ "mean: " ++ show mean
