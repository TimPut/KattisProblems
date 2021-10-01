import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad

main = do
  (n:raw) <- parse <$> B.getContents :: IO [Int]
  let cases = chunk raw
      r = fmap runCase cases
  B.putStr . B.unlines $ r

chunk [] = []
chunk (l:n:raw) = (l:take n raw):chunk (drop n raw)
runCase (l:ants) = let f (mn,mx) p = (max mn (min (l-p) p),maximum[mx,p,l-p])
                       (a,b) = foldl' f (0,0) ants
                   in B.pack $ show a ++ " " ++ show b

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
