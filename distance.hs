import qualified Data.ByteString.Char8 as B
import Data.List

main = do
  (n:xs) <- parse <$> B.getContents :: IO [Int]
  B.putStr . B.pack . show $ axis (everyOther xs) + axis (everyOther . tail $ xs)

axis xs = let xs' = sort xs in foldl1' (+) $ zipWith (-) (zipWith (*) xs' [1..]) (tail $ scanl' (+) 0 xs')

everyOther (x:y:xs) = x:everyOther xs
everyOther xs = xs

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
