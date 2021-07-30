import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad

main = do
  p <- fst . fromJust . B.readInt <$> B.getLine :: IO Int
  results <- forM [1..p] runCase
  B.putStr . B.unlines $ results

runCase k = do
  n <- fst . fromJust . B.readInt . last . B.words <$> B.getLine :: IO Int
  arr <- concat <$> forM [1..(ceiling (fromIntegral n / 10))] (const $ do {l <- B.getLine; return $ parse l})
  pure . B.pack $ show k ++ " " ++ (show $ solve arr)

solve arr = length arr - go 0 arr (sort arr)
  where
    go acc [] _ = acc
    go acc (x:xs) (y:ys) = if x == y then go (acc+1) xs ys else go acc xs (y:ys)
parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
