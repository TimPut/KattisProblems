{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe

main = do
  socks <- tail . parse  <$> B.getContents
  B.putStr . fromMaybe "impossible" . fmap (B.pack . show) $ run 0 socks []

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}

run !acc [] [] = Just acc
run !acc (x:xs) [] = run (acc+1) xs (pure x)
run !acc (x:xs) (y:ys) = run (acc+1) xs (if x == y then ys else (x:y:ys))
run !acc [] _ = Nothing
