{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Data.List

main = do
  cases <- fmap (fmap fromIntegral . parse) . tail . B.lines <$> B.getContents
  B.putStr . B.unlines $ fmap solve cases

unsafeReadInt bs = let Just parse = B.readInt bs in fst parse
{-# INLINE unsafeReadInt #-}

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}

solve [w,h,m,n,x,y] = case gcrt2 (w'-x,w') (h'-y,h') of
                        Nothing -> "Johnny will die waiting"
                        Just (c,k) -> B.pack $ show c
  where
    w' = w-m
    h' = h-n

-- Chinese remainder theorem snippet from Brent Yorgey
-- https://byorgey.wordpress.com/2020/03/03/competitive-programming-in-haskell-modular-arithmetic-part-2/

-- egcd a b = (g,x,y)
--   g is the gcd of a and b, and ax + by = g
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (abs a, signum a, 0)
egcd a b = (g, y, x - (a `div` b) * y)
  where
    (g,x,y) = egcd b (a `mod` b)

gcrt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
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
gcrt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
gcrt []         = Nothing
gcrt [e]        = Just e
gcrt (e1:e2:es) = gcrt2 e1 e2 >>= \e -> gcrt (e:es)
