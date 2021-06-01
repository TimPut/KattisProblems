import qualified Data.ByteString.Char8 as B
import Data.List

main = do
    ls <- fmap parse . tail . B.lines <$> B.getContents :: IO [[Integer]]
    B.putStr . B.unlines $ fmap solveOut ls

solveOut [a,n,b,m] = B.unwords . fmap (B.pack . show) $ solve a n b m

solve a n b m = let (p,q) = extendedEu n m
                in [(b*p*n + a*q*m) `mod` (n*m), n*m]

extendedEu a 0 = (1, 0)
extendedEu a b = (t, s - q * t)
  where
    (q, r) = quotRem a b
    (s, t) = extendedEu b r

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
