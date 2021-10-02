{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.List.Split
import Prelude hiding (Either(..))
import Control.Monad
import System.Random

--Row Major
type GameState = [[Int]]
showGameState g = fmap (\c -> if c == ' ' then '\t' else c)
                  . unlines $ fmap (unwords . (fmap show)) g

push cells = pad . merge . filter (>0) $ cells

merge [] = []
merge [x] = [x]
merge (x:y:xs) = if x==y then (x*2):merge xs else x:merge (y:xs)
pad xs = take 4 $ xs ++ [0,0..]

data Command = Left | Up | Right | Down | None
parseCommand n = case n of
  '0' -> Left
  '1' -> Up
  '2' -> Right
  '3' -> Down
  'a' -> Left
  'w' -> Up
  'd' -> Right
  's' -> Down
  _ -> None

move :: GameState -> Command -> GameState
move g Left = fmap push g
move g Right = fmap (reverse . push . reverse) $ g
move g Down = transpose . fmap (reverse . push . reverse) . transpose $ g
move g Up = transpose . fmap push . transpose $ g
move g None = g

main = do
  gs <- fmap parse <$> forM [1..4] (const B.getLine) :: IO GameState
  -- (gs,com) <- second (parseCommand . head) . first (chunksOf 4) . splitAt 16
             -- . parse <$> B.getContents :: IO (GameState, Command)
  com <- head <$> getLine :: IO Char
  if com `elem` ['0'..'3']
    then B.putStr . B.pack . showGameState $ move gs (parseCommand com)
    else (do
      loop gs (parseCommand com)
    
         )
loop gs com = do
  let gs' = move gs com
  gs'' <- fillCell gs'
  B.putStr . B.pack . showGameState $ gs''
  B.putStrLn "\n----\n"
  com' <- parseCommand . head <$> getLine
  loop gs'' com'

fillCell gs = do
  if (0 `elem` concat gs)
    then (do
             r <- randomRIO (0,3)
             c <- randomRIO (0,3)
             valI <- randomRIO (0,1)
             let val = [2,4] !! valI
             if (gs !! r) !! c /= 0 then fillCell gs else pure (set (r,c) val gs))
    else pure gs

set (r,c) v gs = let (ra, rb) = second tail . splitAt r $ gs
                     (ca,cb) = second tail . splitAt c $ gs !! r
                 in ra ++ [ca ++ [v] ++ cb] ++ rb

first f (a,b) = (f a, b)
second f (a,b) = (a, f b)

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
