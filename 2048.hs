import qualified Data.ByteString.Char8 as B
import Data.List
import Data.List.Split
import Prelude hiding (Either(..))

--Row Major
type GameState = [[Int]]
showGameState g = unlines $ fmap (unwords . (fmap show)) g

push cells = pad . merge . filter (>0) $ cells

merge [] = []
merge [x] = [x]
merge (x:y:xs) = if x==y then (x*2):merge xs else x:merge (y:xs)
pad xs = take 4 $ xs ++ [0,0..]

data Command = Left | Up | Right | Down
parseCommand n = case n of
  0 -> Left
  1 -> Up
  2 -> Right
  3 -> Down

move :: GameState -> Command -> GameState
move g Left = fmap push g
move g Right = fmap (reverse . push . reverse) $ g
move g Down = transpose . fmap (reverse . push . reverse) . transpose $ g
move g Up = transpose . fmap push . transpose $ g

main = do
  (gs,com) <- second (parseCommand . head) . first (chunksOf 4) . splitAt 16
             . parse <$> B.getContents :: IO (GameState, Command)
  B.putStr . B.pack . showGameState $ move gs com

first f (a,b) = (f a, b)
second f (a,b) = (a, f b)

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
