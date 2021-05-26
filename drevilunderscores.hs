{-
Dr Evil Underscores via Recursion Schemes
https://codeforces.com/problemset/problem/1285/D

We implement a solution three separate times to illustrate the structure of the desired solution, named hyloRecurse below.
-}

{-# LANGUAGE DeriveFunctor #-}

import           Data.Bits
--import           Data.Functor.Foldable
import           Data.List
import           Data.Word

main = do
  _ <- getLine
  ns <- fmap read . words <$> getLine :: IO [Word32]
  let bs = w2bs <$> ns
  -- Solution 1:
  -- print $ ixRecurse 32 ns

  -- Solution 2:
  -- print . bs2w $ structuralRecurse bs

  -- Solution 3:
  print . bs2w $ hyloRecurse bs

--------------------------------------------------------------------------------
-- Indexed Recursion, where we carry a current index through the recursion.
--------------------------------------------------------------------------------
ixRecurse :: Int -> [Word32] -> Word32
ixRecurse 0 _ = 0
ixRecurse i [] = 0
ixRecurse i ns = go (i-1) zs os
  where
    zs, os :: [Word32]
    (zs,os) = partition (\x -> (bit (i-1) .&. x) /= 0) ns

    go :: Int -> [Word32] -> [Word32] -> Word32
    go i [] [] = 0
    go i [] os = ixRecurse i os
    go i zs [] = ixRecurse i zs
    go i zs os = min (ixRecurse i zs) (ixRecurse i os) + 2^i


-- This problem isn't fundamentally about numbers; we don't do any
-- arithmetic. Fundamentally this problem is about bit strings, which
-- we can entry-wise XOR and to which we can give a lexicographical
-- ordering. So we declare a type of bit strings:
data Bit = O | I
  deriving (Show, Eq, Ord)

type BitString = [Bit]

--------------------------------------------------------------------------------
-- Explicit Recursion on BitStrings
--------------------------------------------------------------------------------
structuralRecurse :: [BitString] -> BitString
structuralRecurse ns = go zs os
  where
    zs, os :: [BitString]
    (zs, os) = partition (\x -> head x == O) . filter (not . null) $ ns

    go :: [BitString] -> [BitString] -> BitString
    go [] []  = []
    go [] os = O:structuralRecurse (tail <$> os)
    go zs [] = O:structuralRecurse (tail <$> zs)
    go zs os = I:min (structuralRecurse . fmap tail $ zs)
                     (structuralRecurse . fmap tail $ os)

--------------------------------------------------------------------------------
-- Recursion Schemes Solution
--------------------------------------------------------------------------------

-- Both of the preceding solutions follow the pattern of recursing
-- down a binary tree and then collapsing the tree down to the root
-- while accumulating a final value. We can factor this into three
-- parts: structure building, structure collapsing, and recursion.

-- We declare a recursive datatype which represents the shape of the
-- call graph of the recursion.
data BTree = Node BTree BTree | Empty
  deriving (Show)

-- We now factor out the recursion into a parameter. Note that
-- Fix (BTreeF) ~ BTree, where Fix is a fixed point combinator.
data BTreeF b = NodeF b b | EmptyF
  deriving (Functor, Show)

-- The recursive composition of a structure building F-coalgebra (a ->
-- F a), and a structure collapsing F-algebra (F a -> a) is known as a
-- hylomorphism.
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = h where h = f . fmap h . g

-- Note that there is no explicit recursion in hyloRecurse. We define
-- a coalgebra which knows how to construct a single layer of the
-- structure, and an algebra which knows how to collapse a single
-- layer of the structure. The function hylo implements recursion on
-- ANY so called Base functor (a functor whose fixed point has the
-- desired structure), and BTreeF is the desired base functor.
hyloRecurse :: [BitString] -> BitString
hyloRecurse = init . hylo alg coalg
  where
    alg :: BTreeF BitString -> BitString
    alg EmptyF        = []
    alg (NodeF [] os) = O:os
    alg (NodeF zs []) = O:zs
    alg (NodeF zs os) = I:min os zs

    coalg :: [BitString] -> BTreeF [BitString]
    coalg [] = EmptyF
    coalg ns = NodeF (tail <$> zs) (tail <$> os)
        where
          (zs, os) = partition (\x -> head x == O) . filter (not . null) $ ns

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
bs2w :: BitString -> Word32
bs2w = go 0
  where
    go acc []     = acc
    go acc (I:bs) = go (shiftL acc 1 .|. 1) bs
    go acc (O:bs) = go (shiftL acc 1) bs

w2bs :: Word32 -> BitString
w2bs w = fmap (\i -> if (bit i .&. w) /= 0 then I else O) [31,30..0]

--------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------
{-
  Kmett (2008) recursion-schemes: Representing common recursion
  patterns as higher-order functions
  http://hackage.haskell.org/package/recursion-schemes

  Meijer, Fokkinga, & Paterson (1991) Functional Programming with
  Bananas, Lenses, Envelopes and Barbed Wire
  https://maartenfokkinga.github.io/utwente/mmf91m.pdf

  Wadler (1990) Recursive types for free!
  http://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt

  Kmett (2009) Recursion Schemes: A Field Guide
  http://ekmett.github.io/reader/2009/recursion-schemes/index.html
-}
