{-# LANGUAGE StrictData #-}

import Control.Monad (forM_,forM)
import qualified Data.ByteString.Char8 as B
import Data.List (sort)
import Data.Maybe (fromJust)
import Prelude hiding (read,print)

main = do
  t <- fst . fromJust . B.readInt <$> B.getLine :: IO Int
  forM [1..t] runCase

runCase _ = do
  [n,b] <- parseTwo
  let targetMl = n * 1000
      upperBound = targetMl+1
  bottles <- forM [1..b] (const $ parseTwo >>= \[a,b] -> pure (Interval a b))
  -- take the fixpoint of the composition of intervals (use of bottles) bounded (non-strict) above by ml
  let reachable = fix (composeIntervals upperBound) bottles
    -- then print the distance to the nearest interval
  if targetMl >= 450000 then print 0 else print . minimum $ fmap (dist targetMl) reachable
{-# INLINE runCase #-}

data Interval = Interval {-# UNPACK #-} !Int !Int
  deriving (Show,Eq,Ord)

(Interval a b) .+. (Interval c d) = Interval (a+c) (d+b)
{-# INLINE (.+.) #-}

-- leftover wine given a volume and an reachable interval
dist n (Interval a b) = if n < a then n else max 0 (n-b)
{-# INLINE dist #-}

-- we don't care about accomodating volumes beyond the problem, so 'normalize' by capping off
bound u (Interval a b) = Interval (min a u) (min b u)
{-# INLINE bound #-}

-- merge adjacent intervals where possible
union (x@(Interval a b):y@(Interval c d):is) =
  if (b >= c && a <= d)
  -- note that we immediately reinsert a unioned interval into the queue for unioning, this makes (fix . union)
  -- bestcase linear rather than bestcase logarithmic. even better, this makes union idempotent on sorted lists
  -- so we can simply call 'union' in O(n) as the outermost call of composeIntervals rather than (fix . union)
  then union $ Interval (min a c) (max b d):is
  else x:union (y:is)
union is = is -- the union of one or zero intervals is a noop
{-# INLINE union #-}

-- iterative fixed point
fix f x = go (iterate f x)
  where
    go (a:b:as) = if a == b then a else go (b:as)
    {-# INLINE go #-}
{-# INLINE fix #-}

-- note that bottles is typically in sorted order, and the result of the list comprehension is the concatenation
-- of a number of sorted subsequences, so the first (innermost) 'union' fuses many intervals which speeds up the
-- call to sort and the subsequent final union.
composeIntervals ml bottles = union . sort . union
                              $ fmap (bound ml) ([ (a .+. b) | a <- bottles, b <- bottles, a <= b ] ++ bottles)
{-# INLINE composeIntervals #-}

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
parseTwo = do
  bs <-  B.getLine :: IO B.ByteString
  let Just(a,restBs) = B.readInt bs
      Just(b,_) = B.readInt . B.drop 1 $ restBs
  return [a,b]
{-# INLINE parseTwo #-}

print = B.putStrLn . B.pack . show
{-# INLINE print #-}
