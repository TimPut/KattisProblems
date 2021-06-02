{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Maybe
import Data.List

main = do
  getImage True
getImage first = do 
  n <- fst . fromJust . B.readInt <$> B.getLine :: IO Int
  unless (n == 0) $ do
    when (not first) (B.putStrLn "")
    scanlines <- forM [1..n] (const parseLine) :: IO [ScanLine]
    let rendered = B.concat . renderLine <$> scanlines
        valid = allEqual $ fmap B.length rendered
    B.putStr . B.concat $ rendered
    when (not valid) (B.putStrLn "Error decoding image")
    getImage False

allEqual (x:y:xs) = x == y && allEqual (y:xs)
allEqual _ = True
renderLine (c,n:ns) = (B.replicate n c):renderLine (swap c,ns)
renderLine (_,[]) = pure "\n"

swap '#' = '.'
swap '.' = '#'

parseLine = do
  l <- B.getLine
  let c = B.head l
      ns = parse (B.drop 2 l)
  return (c,ns)

type ScanLine = (Char,[Int])
  
parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
