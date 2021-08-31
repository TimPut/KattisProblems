{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as B
import Data.List
import Prelude hiding (fst)

main = do
  ns <- fmap redness . B.unpack <$> B.getLine :: IO [Int]
  let a = kadane ns
      b = kadane (fmap negate ns)
  B.putStrLn . pretty . maximumBy largestEarliest $ [a,b]

redness 'R' = 1
redness _ = -1

pretty (t,i,j) = B.pack . unwords $ fmap show [i+1,j]

kadane :: [Int] -> (Int,Int,Int)
kadane xs = maximumOn fst
            $ scanl' f initState xs
  where
    f :: (Int,Int,Int) -> Int -> (Int,Int,Int)
    f (!t,!i,!j) !x = if t + x >= 0
                  then (t+x,i,j+1)
                  else (0,j+1,j+1)
    initState = (0,0,0) -- (total,startIndex,endIndex)

fst (a,_,_) = a
largestEarliest (t,i,j) (t',i',j') = (t,-i,-j) `compare` (t',-i,-j')

-- maximumBy from base returns the last maximal element, whereas we
-- want the first (west-most) maximal element in kadane
-- This is slightly faster than simply using "maximumBy largestEarliest" everywhere
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' cmp = foldl1' max'
  where max' x y = case cmp x y of
                        LT  -> y
                        _ -> x
maximumOn f = maximumBy' (\a b -> f a `compare` f b)
