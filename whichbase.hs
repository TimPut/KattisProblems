import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad

main = do
  p <- fst . fromJust . B.readInt <$> B.getLine :: IO Int
  results <- forM [1..p] runCase
  B.putStr . B.unlines $ results

runCase k = do
  n <- fmap read . fmap pure . B.unpack . last . B.words <$> B.getLine :: IO [Int]
  pure . B.pack $ show k ++ " " ++ solve (reverse n)

solve n = unwords $ fmap show [octal,decimal,hexadecimal]
  where
    octal = if any (>=8) n then 0 else sum $ zipWith (*) n (fmap (8^) [0..])
    decimal = sum $ zipWith (*) n (fmap (10^) [0..])
    hexadecimal = sum $ zipWith (*) n (fmap (16^) [0..])
