import Data.List
import Data.Ord
main = interact (unlines . fmap (unwords . flip (zipWith (<>)) (cycle ["-A","-B"]) . s . words) . o . tail . lines)
s a = let x = sortOn Down a in last x : init x
o (x:y:xs) = x:o xs
o x = x
