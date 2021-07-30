import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Word
import Data.Bits

main = do
  p <- fst . fromJust . B.readInt <$> B.getLine :: IO Int
  results <- forM [1..p] runCase
  B.putStr . B.unlines $ results

runCase k = do
  n <- fst . fromJust . B.readInt . last . B.words <$> B.getLine :: IO Int
  pure . B.pack $ show k ++ " " ++ solve n

solve n = let (p,q) = index (fromIntegral n)
          in show p ++ "/" ++ show q

data BTree a = BTree (BTree a) a (BTree a)
  deriving (Show)
left (BTree bt _ _) = bt
right (BTree _ _ bt) = bt
label (BTree _ lbl bt) = lbl
build (p,q) = BTree l (p,q) r
  where
    l = build (p,p+q)
    r = build (p+q,q)

pathTo n = reverse $ tail . dropWhile (== O) $ w2bs n
f O = left
f I = right

index n = label $ foldr (.) id (fmap f . pathTo $ n) (build (1,1))
--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
data Bit = O | I
  deriving (Show, Eq, Ord)

type BitString = [Bit]

bs2w :: BitString -> Word32
bs2w = go 0
  where
    go acc []     = acc
    go acc (I:bs) = go (shiftL acc 1 .|. 1) bs
    go acc (O:bs) = go (shiftL acc 1) bs

w2bs :: Word32 -> BitString
w2bs w = fmap (\i -> if (bit i .&. w) /= 0 then I else O) [31,30..0]
