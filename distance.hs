import qualified Data.ByteString.Char8 as B
import Data.List

main = do
  _ <- B.getLine
  xs <- parse <$> B.getContents :: IO [Int]
  print $ axis (everyOther xs) + axis (everyOther . tail $ xs)

axis xs = let xs' = sort xs in sum $ zipWith (-) (zipWith (*) xs' [1..]) (scanl1 (+) xs')

everyOther (x:y:xs) = x:everyOther xs
everyOther xs = xs

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
