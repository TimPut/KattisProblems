import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.List (unfoldr)
import Data.Maybe

main = do
  Just (n, _) <- B.readInt <$> B.getLine
  results <- forM [1..n] runCase :: IO [B.ByteString]
  B.putStr . B.unlines $ results

runCase :: a -> IO B.ByteString
runCase _ = do
  Just (l, _) <- B.readInt <$> B.getLine
  msg <- parse <$> B.getLine :: IO [Int]
  cypherText <- parse <$> B.getLine :: IO [Int]
  table <- V.fromList . parse <$> B.getLine :: IO (V.Vector Int)
  let pairs = zip msg cypherText
      xs = (uncurry $ stepsTo table) <$> pairs
      ns = cycleLength table <$> msg
  pure . B.pack . show . fst . fromJust $ gcrt $ zip xs ns

stepsTo table start finish = go 0 start
  where
    go count ix = if ix == finish
                  then count
                  else go (count+1) (table V.! (ix - 1))

cycleLength table start = go 1 (table V.! (start - 1))
  where
    go count ix = if ix == start
                  then count
                  else go (count+1) (table V.! (ix - 1))

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}

-- Chinese remainder theorem snippet from Brent Yorgey
-- https://byorgey.wordpress.com/2020/03/03/competitive-programming-in-haskell-modular-arithmetic-part-2/

-- egcd a b = (g,x,y)
--   g is the gcd of a and b, and ax + by = g
egcd :: Int -> Int -> (Int, Int, Int)
egcd a 0 = (abs a, signum a, 0)
egcd a b = (g, y, x - (a `div` b) * y)
  where
    (g,x,y) = egcd b (a `mod` b)

gcrt2 :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
gcrt2 (a,n) (b,m)
  | a `mod` g == b `mod` g = Just (((a*v*m + b*u*n) `div` g) `mod` k, k)
  | otherwise             = Nothing
  where
    (g,u,v) = egcd n m
    k = (m*n) `div` g
-- gcrt solves a system of modular equations.  Each equation x = a
-- (mod n) is given as a pair (a,n).  Returns a pair (z, k) such that
-- solutions for x satisfy x = z (mod k), that is, solutions are of
-- the form x = kt + z for integer t.
gcrt :: [(Int, Int)] -> Maybe (Int, Int)
gcrt []         = Nothing
gcrt [e]        = Just e
gcrt (e1:e2:es) = gcrt2 e1 e2 >>= \e -> gcrt (e:es)
