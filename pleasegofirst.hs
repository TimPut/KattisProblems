import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Maybe
import Data.Char

main = do
  n <- fst . fromJust . B.readInt <$> B.getLine :: IO Int
  results <- forM [1..n] runCase
  B.putStrLn . B.unlines $ results

runCase _ = do
  k <- fst . fromJust . B.readInt <$> B.getLine :: IO Int
  queue <- B.getLine :: IO B.ByteString
  let result = (*5) . sum . concat . fmap rezero $ flip B.elemIndices (B.reverse queue) <$> cs
  return (B.pack . show $ result)

cs = filter isAlphaNum ['0'..'z']

rezero [] = []
rezero (i:is) = zipWith (-) (i:is) [i..]
