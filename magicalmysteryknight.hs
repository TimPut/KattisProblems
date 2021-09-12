import Data.List (sortOn,transpose,groupBy)
import Data.List.Split
import qualified Data.ByteString.Char8 as B
import Data.Maybe
main = do
  inp <- fmap (fst . fromJust . B.readInt) . B.words <$> B.getContents :: IO [Int]
  let ts = head $ filter (subTour inp) fullTours
  B.putStr . B.unlines . fmap B.unwords . chunksOf 8 $ fmap (B.pack . show) ts

rotl :: [[x]] -> [[x]]
rotl = transpose . map reverse

--fullTours :: [[[Int]]]
fullTours = filter isMagical . concat $ fmap (\s -> fmap (fmap (\a -> 1 + ((a+s-1) `mod` 64))) (forwardTours ++ fmap reverseTour forwardTours)) shifts
  where
    forwardTours = concat . (fmap (fmap concat)) $ (tours' ++ mirroredTours)
    mirroredTours = fmap (take 4 . iterate rotl) $ fmap (fmap reverse) tours
    tours' = fmap (take 4 . iterate rotl) $ tours
    shifts = [0,1]
isMagical ts = all (== 260) . fmap sum . chunksOf 8 $ ts

reverseTour = fmap ((-) 64)

subTour [] [] = True
subTour [] (y:ys) = False
subTour (x:xs) [] = False
subTour (x:xs) (y:ys) = if x == y || x == (-1) then subTour xs ys else False

tours :: [[[Int]]]
tours =
    [[[1,30,47,52,3,28,45,54], [48,51,2,29,46,53,4,27], [31,8,49,42,25,6,55,44], [50,41,32,7,56,43,26,5], [9,62,39,20,33,24,15,58], [40,19,10,61,16,57,34,23], [63,38,17,12,21,36,59,14],
    [18,11,64,37,60,13,22,35]],[[1,30,47,52,3,28,45,54],[48,51,2,29,46,53,4,27],[31,8,49,42,25,6,55,44],[50,41,32,7,56,43,26,5],[39,62,9,20,33,24,15,58],[10,19,40,61,16,57,34,23],
    [63,38,17,12,21,36,59,14],[18,11,64,37,60,13,22,35]],[[1,30,47,52,3,28,45,54],[48,51,2,29,46,53,4,27],[31,12,49,14,21,34,55,44],[50,37,32,35,60,15,26,5],[11,62,13,20,33,22,43,56],
    [38,19,36,61,16,59,6,25],[63,10,17,40,23,8,57,42],[18,39,64,9,58,41,24,7]],[[1,30,47,52,5,28,43,54],[48,51,2,29,44,53,6,27],[31,46,49,4,7,26,55,42],[50,3,32,45,40,57,24,9],
    [33,62,15,20,25,8,41,56],[16,19,34,61,58,39,10,23],[63,14,17,36,21,12,59,38],[18,35,64,13,60,37,22,11]],[[1,30,47,52,5,28,43,54],[48,51,2,29,44,53,6,27],[31,46,49,4,7,26,55,42],
    [50,3,32,45,56,41,8,25],[33,62,15,20,9,24,57,40],[16,19,34,61,58,39,10,23],[63,14,17,36,21,12,59,38],[18,35,64,13,60,37,22,11]],[[1,30,47,52,5,28,43,54],[48,51,2,29,44,53,6,27],
    [31,46,49,4,25,8,55,42],[50,3,32,45,56,41,26,7],[33,62,15,20,9,24,39,58],[16,19,34,61,40,57,10,23],[63,14,17,36,21,12,59,38],[18,35,64,13,60,37,22,11]],[[1,30,47,52,5,28,43,54],
    [48,51,2,29,44,53,26,7],[31,46,49,4,27,6,55,42],[50,3,32,45,56,41,8,25],[33,62,15,20,9,24,57,40],[16,19,34,61,38,59,10,23],[63,14,17,36,21,12,39,58],[18,35,64,13,60,37,22,11]],
    [[1,30,47,52,27,54,43,6],[48,51,2,29,44,5,26,55],[31,46,49,4,53,28,7,42],[50,3,32,45,8,41,56,25],[33,62,15,20,57,24,9,40],[16,19,34,61,12,37,58,23],[63,14,17,36,21,60,39,10],
    [18,35,64,13,38,11,22,59]],[[1,30,47,52,43,54,7,26],[48,51,2,29,6,27,42,55],[31,46,49,4,53,44,25,8],[50,3,32,45,28,5,56,41],[33,62,15,20,37,60,9,24],[16,19,34,61,12,21,40,57],
    [63,14,17,36,59,38,23,10],[18,35,64,13,22,11,58,39]],[[2,5,62,39,26,31,36,59],[63,40,1,4,37,60,25,30],[6,3,38,61,32,27,58,35],[41,64,7,12,49,34,29,24],[8,45,50,33,28,23,16,57],
    [51,42,11,48,13,54,19,22],[46,9,44,53,20,17,56,15],[43,52,47,10,55,14,21,18]],[[2,11,32,53,4,57,46,55],[31,52,3,12,45,54,5,58],[10,1,30,33,24,59,56,47],[51,64,23,44,13,34,25,6],
    [22,9,50,29,60,7,48,35],[63,40,43,8,49,14,17,26],[42,21,38,61,28,19,36,15],[39,62,41,20,37,16,27,18]],[[2,11,58,51,14,39,54,31],[59,50,3,12,53,30,15,38],[10,1,52,57,40,13,32,55],
    [49,60,9,4,29,56,37,16],[64,5,24,45,20,41,28,33],[23,48,61,8,25,36,17,42],[6,63,46,21,44,19,34,27],[47,22,7,62,35,26,43,18]],[[2,11,58,51,30,39,54,15],[59,50,3,12,53,14,31,38],
    [10,1,52,57,40,29,16,55],[49,60,9,4,13,56,37,32],[64,5,24,45,36,41,28,17],[23,48,61,8,25,20,33,42],[6,63,46,21,44,35,18,27],[47,22,7,62,19,26,43,34]],[[2,15,50,29,64,27,38,35],
    [51,30,1,14,39,36,63,26],[16,3,32,49,28,61,34,37],[31,52,13,4,33,40,25,62],[54,17,48,41,12,5,60,23],[45,42,53,20,57,24,11,8],[18,55,44,47,6,9,22,59],[43,46,19,56,21,58,7,10]],
    [[2,15,62,41,60,39,22,19],[63,42,3,16,21,18,59,38],[14,1,44,61,40,57,20,23],[43,64,13,4,17,24,37,58],[12,31,50,45,56,35,6,25],[49,46,11,32,5,26,55,36],[30,51,48,9,28,53,34,7],
    [47,10,29,52,33,8,27,54]],[[2,23,38,47,36,53,10,51],[39,46,3,22,11,50,55,34],[24,1,48,37,54,35,52,9],[45,40,21,4,49,12,33,56],[64,25,44,17,60,29,8,13],[41,20,61,28,5,16,57,32],
    [26,63,18,43,30,59,14,7],[19,42,27,62,15,6,31,58]],[[2,23,42,33,64,31,38,27],[43,34,1,24,37,28,63,30],[22,3,36,41,32,61,26,39],[35,44,21,4,25,40,29,62],[54,5,52,45,20,13,60,11],
    [51,48,55,8,57,10,17,14],[6,53,46,49,16,19,12,59],[47,50,7,56,9,58,15,18]],[[2,23,44,57,6,55,46,27],[59,42,3,24,45,26,7,54],[22,1,58,43,56,5,28,47],[41,60,21,4,25,48,53,8],
    [20,15,40,61,52,9,34,29],[39,64,19,16,33,30,49,10],[14,17,62,37,12,51,32,35],[63,38,13,18,31,36,11,50]],[[2,27,42,47,18,23,62,39],[43,48,1,26,63,40,17,22],[28,3,46,41,24,19,38,61],
    [49,44,25,8,33,64,21,16],[4,29,56,45,20,9,60,37],[53,50,7,32,57,34,15,12],[30,5,52,55,10,13,36,59],[51,54,31,6,35,58,11,14]],[[2,27,42,47,30,55,6,51],[41,46,3,28,5,50,31,56],
    [26,1,48,43,54,29,52,7],[45,40,21,4,49,12,57,32],[64,25,44,17,36,53,8,13],[39,20,61,22,11,16,33,58],[24,63,18,37,60,35,14,9],[19,38,23,62,15,10,59,34]],[[2,27,42,49,30,53,6,51],
    [41,48,3,28,5,50,31,54],[26,1,46,43,56,29,52,7],[47,40,25,4,45,12,55,32],[64,23,44,13,36,57,8,15],[39,20,61,24,11,14,33,58],[22,63,18,37,60,35,16,9],[19,38,21,62,17,10,59,34]],
    [[2,27,42,51,40,29,54,15],[43,50,3,28,53,14,31,38],[26,1,52,41,30,39,16,55],[49,44,25,4,13,56,37,32],[24,5,64,45,36,17,12,57],[63,48,7,22,9,60,33,18],[6,23,46,61,20,35,58,11],
    [47,62,21,8,59,10,19,34]],[[2,27,42,51,40,29,54,15],[43,50,3,28,53,14,31,38],[26,1,52,41,30,39,16,55],[49,44,25,4,13,56,37,32],[24,5,64,45,36,17,12,57],[63,48,21,8,59,10,33,18],
    [6,23,46,61,20,35,58,11],[47,62,7,22,9,60,19,34]],[[2,27,50,43,6,23,62,47],[51,42,1,26,63,48,7,22],[28,3,44,49,24,5,46,61],[41,52,25,4,45,64,21,8],[14,29,40,53,20,9,60,35],
    [39,54,13,32,57,36,19,10],[30,15,56,37,12,17,34,59],[55,38,31,16,33,58,11,18]],[[2,27,50,43,6,23,62,47],[51,42,1,26,63,48,7,22],[28,3,44,49,24,5,46,61],[41,52,25,4,45,64,21,8],
    [54,29,40,13,36,9,60,19],[39,14,53,32,57,20,35,10],[30,55,16,37,12,33,18,59],[15,38,31,56,17,58,11,34]],[[2,27,52,41,14,39,54,31],[43,50,3,28,53,30,15,38],[26,1,42,51,40,13,32,55],
    [49,44,25,4,29,56,37,16],[24,5,64,45,60,17,12,33],[63,48,7,22,9,36,57,18],[6,23,46,61,20,59,34,11],[47,62,21,8,35,10,19,58]],[[2,27,52,41,30,39,54,15],[43,50,3,28,53,14,31,38],
    [26,1,42,51,40,29,16,55],[49,44,25,4,13,56,37,32],[24,5,64,45,36,17,12,57],[63,48,7,22,9,60,33,18],[6,23,46,61,20,35,58,11],[47,62,21,8,59,10,19,34]],[[2,27,52,41,30,39,54,15],
    [43,50,3,28,53,14,31,38],[26,1,42,51,40,29,16,55],[49,44,25,4,13,56,37,32],[24,5,64,45,36,17,12,57],[63,48,21,8,59,10,33,18],[6,23,46,61,20,35,58,11],[47,62,7,22,9,60,19,34]],
    [[2,27,62,37,60,35,6,31],[63,38,1,28,5,32,59,34],[40,3,26,61,36,7,30,57],[25,64,39,4,29,58,33,8],[14,41,24,49,20,9,56,47],[23,50,13,44,53,48,19,10],[42,15,52,21,12,17,46,55],
    [51,22,43,16,45,54,11,18]],[[2,27,62,37,60,35,6,31],[63,38,1,28,5,32,59,34],[40,3,26,61,36,7,30,57],[25,64,39,4,29,58,33,8],[50,41,24,13,48,9,56,19],[23,14,49,44,53,20,47,10],
    [42,51,16,21,12,45,18,55],[15,22,43,52,17,54,11,46]],[[2,31,38,61,36,27,6,59],[39,62,1,32,5,60,35,26],[30,3,64,37,28,33,58,7],[63,40,29,4,57,8,25,34],[42,17,52,13,48,23,56,9],
    [51,14,41,20,53,10,47,24],[18,43,16,49,12,45,22,55],[15,50,19,44,21,54,11,46]],[[2,39,58,31,18,15,54,43],[59,30,3,40,55,42,17,14],[38,1,32,57,16,19,44,53],[29,60,37,4,41,56,13,20],
    [64,5,28,33,24,9,52,45],[27,36,61,8,49,46,21,12],[6,63,34,25,10,23,48,51],[35,26,7,62,47,50,11,22]],[[2,39,58,31,56,41,18,15],[59,30,3,40,17,14,43,54],[38,1,32,57,42,55,16,19],
    [29,60,37,4,13,20,53,44],[64,5,28,33,52,45,12,21],[27,36,61,8,23,10,49,46],[6,63,34,25,48,51,22,11],[35,26,7,62,9,24,47,50]],[[2,39,58,31,56,41,18,15],[59,30,3,40,17,14,55,42],
    [38,1,32,57,44,53,16,19],[29,60,37,4,13,20,43,54],[64,5,28,33,52,45,22,11],[27,36,61,8,21,12,49,46],[6,63,34,25,48,51,10,23],[35,26,7,62,9,24,47,50]],[[2,39,58,31,56,41,18,15],
    [59,30,3,40,17,14,55,42],[38,1,32,57,54,43,16,19],[29,60,37,4,13,20,53,44],[64,5,28,33,52,45,12,21],[27,36,61,8,11,22,49,46],[6,63,34,25,48,51,10,23],[35,26,7,62,9,24,47,50]],
    [[2,39,62,27,6,35,58,31],[63,26,1,38,59,32,7,34],[40,3,28,61,36,5,30,57],[25,64,37,4,29,60,33,8],[14,41,24,49,20,9,56,47],[23,50,13,44,53,48,19,10],[42,15,52,21,12,17,46,55],
    [51,22,43,16,45,54,11,18]],[[2,39,62,27,6,35,58,31],[63,26,1,38,59,32,7,34],[40,3,28,61,36,5,30,57],[25,64,37,4,29,60,33,8],[50,41,24,13,48,9,56,19],[23,14,49,44,53,20,47,10],
    [42,51,16,21,12,45,18,55],[15,22,43,52,17,54,11,46]],[[2,43,50,25,48,23,6,63],[51,26,1,44,5,64,47,22],[28,3,42,49,24,45,62,7],[41,52,27,4,61,8,21,46],[54,29,40,13,36,19,60,9],
    [39,14,53,32,57,10,35,20],[30,55,16,37,12,33,18,59],[15,38,31,56,17,58,11,34]],[[2,43,50,25,48,23,6,63],[51,26,1,44,5,64,47,22],[42,3,28,49,24,45,62,7],[27,52,41,4,61,8,21,46],
    [40,29,54,13,36,19,60,9],[53,14,39,32,57,10,35,20],[30,55,16,37,12,33,18,59],[15,38,31,56,17,58,11,34]],[[2,43,50,25,48,63,6,23],[51,26,1,44,5,24,47,62],[28,3,42,49,64,45,22,7],
    [41,52,27,4,21,8,61,46],[14,29,40,53,36,59,20,9],[39,54,13,32,17,10,35,60],[30,15,56,37,12,33,58,19],[55,38,31,16,57,18,11,34]],[[2,43,50,25,64,23,6,47],[51,26,1,44,5,48,63,22],
    [28,3,42,49,24,7,46,61],[41,52,27,4,45,62,21,8],[14,29,40,53,20,9,60,35],[39,54,13,32,57,36,19,10],[30,15,56,37,12,17,34,59],[55,38,31,16,33,58,11,18]],[[2,43,50,25,64,23,6,47],
    [51,26,1,44,5,48,63,22],[28,3,42,49,24,7,46,61],[41,52,27,4,45,62,21,8],[54,29,40,13,36,9,60,19],[39,14,53,32,57,20,35,10],[30,55,16,37,12,33,18,59],[15,38,31,56,17,58,11,34]],
    [[2,43,50,25,64,23,6,47],[51,26,1,44,5,48,63,22],[28,3,42,49,24,61,46,7],[41,52,27,4,45,8,21,62],[54,29,40,13,36,19,60,9],[39,14,53,32,57,10,35,20],[30,55,16,37,12,33,18,59],
    [15,38,31,56,17,58,11,34]],[[2,43,50,25,64,23,6,47],[51,26,1,44,5,48,63,22],[42,3,28,49,24,7,46,61],[27,52,41,4,45,62,21,8],[40,29,54,13,36,9,60,19],[53,14,39,32,57,20,35,10],
    [30,55,16,37,12,33,18,59],[15,38,31,56,17,58,11,34]],[[2,43,50,25,64,23,6,47],[51,26,1,44,5,48,63,22],[42,3,28,49,24,61,46,7],[27,52,41,4,45,8,21,62],[40,29,54,13,36,19,60,9],
    [53,14,39,32,57,10,35,20],[30,55,16,37,12,33,18,59],[15,38,31,56,17,58,11,34]],[[2,43,58,5,52,15,30,55],[59,6,3,16,57,54,51,14],[44,1,42,53,4,31,56,29],[7,60,17,64,9,40,13,50],
    [18,45,8,41,32,49,28,39],[61,24,63,20,37,10,33,12],[46,19,22,25,48,35,38,27],[23,62,47,36,21,26,11,34]],[[2,43,58,5,52,15,30,55],[59,6,3,16,57,54,51,14],[44,1,42,53,4,31,56,29],
    [7,60,17,64,9,40,13,50],[18,45,8,41,32,49,28,39],[61,24,63,36,21,10,33,12],[46,19,22,25,48,35,38,27],[23,62,47,20,37,26,11,34]],[[2,43,58,19,30,39,22,47],[59,18,3,44,21,46,31,38],
    [42,1,20,57,40,29,48,23],[17,60,41,4,45,24,37,32],[64,5,56,13,36,9,28,49],[55,16,61,8,25,52,33,10],[6,63,14,53,12,35,50,27],[15,54,7,62,51,26,11,34]],[[2,43,58,19,46,39,22,31],
    [59,18,3,44,21,30,47,38],[42,1,20,57,40,45,32,23],[17,60,41,4,29,24,37,48],[64,5,56,13,52,9,28,33],[55,16,61,8,25,36,49,10],[6,63,14,53,12,51,34,27],[15,54,7,62,35,26,11,50]],
    [[2,43,58,19,56,45,6,31],[59,18,3,44,5,30,47,54],[42,1,20,57,46,55,32,7],[17,60,41,4,29,8,53,48],[64,21,16,37,52,33,28,9],[15,40,61,24,11,26,49,34],[22,63,38,13,36,51,10,27],
    [39,14,23,62,25,12,35,50]],[[2,43,58,53,4,15,30,55],[59,6,3,16,57,54,51,14],[44,1,42,5,52,31,56,29],[7,60,17,64,9,40,13,50],[18,45,8,41,32,49,28,39],[61,24,63,20,37,10,33,12],
    [46,19,22,25,48,35,38,27],[23,62,47,36,21,26,11,34]],[[2,43,58,53,4,15,30,55],[59,6,3,16,57,54,51,14],[44,1,42,5,52,31,56,29],[7,60,17,64,9,40,13,50],[18,45,8,41,32,49,28,39],
    [61,24,63,36,21,10,33,12],[46,19,22,25,48,35,38,27],[23,62,47,20,37,26,11,34]],[[2,51,64,15,54,29,18,27],[63,14,3,52,17,26,55,30],[50,1,16,61,32,53,28,19],[13,62,49,4,25,20,31,56],
    [48,39,12,33,60,41,6,21],[11,36,45,40,5,24,57,42],[38,47,34,9,44,59,22,7],[35,10,37,46,23,8,43,58]],[[2,59,62,7,18,43,46,23],[61,6,1,42,63,24,19,44],[58,3,60,17,8,45,22,47],
    [5,16,53,64,41,20,25,36],[52,57,4,9,32,37,48,21],[15,54,13,40,49,28,35,26],[12,51,56,31,10,33,38,29],[55,14,11,50,39,30,27,34]],[[2,59,62,7,18,43,46,23],[61,6,1,42,63,24,19,44],
    [58,3,60,17,8,45,22,47],[5,16,53,64,41,36,25,20],[52,57,4,9,32,21,48,37],[15,54,13,40,49,28,35,26],[12,51,56,31,10,33,38,29],[55,14,11,50,39,30,27,34]],[[2,59,62,7,18,43,46,23],
    [61,6,1,42,63,24,19,44],[58,3,60,17,8,45,22,47],[53,16,5,64,41,20,25,36],[4,57,52,9,32,37,48,21],[15,54,13,40,49,28,35,26],[12,51,56,31,10,33,38,29],[55,14,11,50,39,30,27,34]],
    [[2,59,62,7,18,43,46,23],[61,6,1,42,63,24,19,44],[58,3,60,17,8,45,22,47],[53,16,5,64,41,36,25,20],[4,57,52,9,32,21,48,37],[15,54,13,40,49,28,35,26],[12,51,56,31,10,33,38,29],
    [55,14,11,50,39,30,27,34]],[[3,6,47,58,31,10,51,54],[46,59,4,7,50,53,30,11],[5,2,57,48,9,32,55,52],[60,45,8,1,56,49,12,29],[43,20,61,24,33,28,37,14],[62,23,44,17,40,13,34,27],
    [19,42,21,64,25,36,15,38],[22,63,18,41,16,39,26,35]],[[3,6,59,32,61,18,47,34],[58,31,4,7,46,33,62,19],[5,2,29,60,17,64,35,48],[30,57,8,1,36,45,20,63],[55,28,37,44,9,16,49,22],
    [40,43,56,25,52,21,10,13],[27,54,41,38,15,12,23,50],[42,39,26,53,24,51,14,11]],[[3,6,59,48,61,10,23,50],[58,47,4,7,22,49,62,11],[5,2,45,60,9,64,51,24],[46,57,8,1,52,21,12,63],
    [31,44,53,20,33,40,25,14],[56,19,32,41,28,13,34,37],[43,30,17,54,39,36,15,26],[18,55,42,29,16,27,38,35]],[[3,10,55,64,5,30,59,34],[54,63,4,9,60,33,6,31],[11,2,61,56,29,8,35,58],
    [62,53,12,1,36,57,32,7],[19,22,37,52,13,28,43,46],[38,51,18,21,44,47,14,27],[23,20,49,40,25,16,45,42],[50,39,24,17,48,41,26,15]],[[3,22,49,56,5,20,47,58],[50,55,4,21,48,57,6,19],
    [23,2,53,44,25,8,59,46],[54,51,24,1,60,45,18,7],[15,36,43,52,17,26,9,62],[42,39,16,33,12,61,30,27],[35,14,37,40,29,32,63,10],[38,41,34,13,64,11,28,31]],[[3,26,35,62,23,14,47,50],
    [34,63,2,25,48,51,22,15],[27,4,61,36,13,24,49,46],[64,33,28,1,52,45,16,21],[5,60,37,32,17,12,53,44],[38,29,8,57,42,55,20,11],[59,6,31,40,9,18,43,54],[30,39,58,7,56,41,10,19]],
    [[3,26,35,62,23,14,47,50],[34,63,2,25,48,51,22,15],[27,4,61,36,13,24,49,46],[64,33,28,1,52,45,16,21],[5,60,37,32,17,12,53,44],[38,29,58,7,56,41,20,11],[59,6,31,40,9,18,43,54],
    [30,39,8,57,42,55,10,19]],[[3,26,41,52,15,30,39,54],[50,43,2,27,40,53,14,31],[25,4,51,42,29,16,55,38],[44,49,28,1,56,37,32,13],[5,24,61,48,17,12,57,36],[64,45,8,21,60,33,18,11],
    [23,6,47,62,9,20,35,58],[46,63,22,7,34,59,10,19]],[[3,26,41,52,15,30,39,54],[50,43,2,27,40,53,14,31],[25,4,51,42,29,16,55,38],[44,49,28,1,56,37,32,13],[5,24,61,48,17,12,57,36],
    [64,45,22,7,60,33,18,11],[23,6,47,62,9,20,35,58],[46,63,8,21,34,59,10,19]],[[3,26,47,54,15,30,51,34],[46,55,2,27,52,33,14,31],[25,4,53,48,29,16,35,50],[56,45,28,1,36,49,32,13],
    [5,24,41,60,17,12,37,64],[44,57,8,21,40,61,18,11],[23,6,59,42,9,20,63,38],[58,43,22,7,62,39,10,19]],[[3,26,47,54,15,30,51,34],[46,55,2,27,52,33,14,31],[25,4,53,48,29,16,35,50],
    [56,45,28,1,36,49,32,13],[5,24,41,60,17,12,37,64],[44,57,22,7,40,61,18,11],[23,6,59,42,9,20,63,38],[58,43,8,21,62,39,10,19]],[[3,26,51,42,15,30,39,54],[50,43,2,27,40,53,14,31],
    [25,4,41,52,29,16,55,38],[44,49,28,1,56,37,32,13],[5,24,61,48,17,12,57,36],[64,45,8,21,60,33,18,11],[23,6,47,62,9,20,35,58],[46,63,22,7,34,59,10,19]],[[3,26,51,42,15,30,39,54],
    [50,43,2,27,40,53,14,31],[25,4,41,52,29,16,55,38],[44,49,28,1,56,37,32,13],[5,24,61,48,17,12,57,36],[64,45,22,7,60,33,18,11],[23,6,47,62,9,20,35,58],[46,63,8,21,34,59,10,19]],
    [[3,38,59,32,61,6,27,34],[58,31,4,37,28,33,62,7],[39,2,29,60,5,26,35,64],[30,57,40,1,36,63,8,25],[41,16,55,20,45,24,49,10],[56,19,42,13,52,9,46,23],[15,54,17,44,21,48,11,50],
    [18,43,14,53,12,51,22,47]],[[3,38,59,32,61,6,27,34],[58,31,4,37,28,33,62,7],[39,2,29,60,5,64,35,26],[30,57,40,1,36,25,8,63],[15,42,17,56,9,48,23,50],[18,55,14,41,24,51,10,47],
    [43,16,53,20,45,12,49,22],[54,19,44,13,52,21,46,11]],[[3,38,59,32,61,6,27,34],[58,31,4,37,28,33,62,7],[39,2,29,60,5,64,35,26],[30,57,40,1,36,25,8,63],[41,16,43,20,45,22,49,24],
    [56,19,54,13,52,11,46,9],[15,42,17,44,21,48,23,50],[18,55,14,53,12,51,10,47]],[[3,38,59,32,61,6,27,34],[58,31,4,37,28,33,62,7],[39,2,29,60,5,64,35,26],[30,57,40,1,36,25,8,63],
    [41,16,53,20,45,12,49,24],[56,19,44,13,52,21,46,9],[15,42,17,54,11,48,23,50],[18,55,14,43,22,51,10,47]],[[3,38,59,32,61,6,27,34],[58,31,4,37,28,33,62,7],[39,2,29,60,5,64,35,26],
    [30,57,40,1,36,25,8,63],[41,16,55,20,45,10,49,24],[56,19,42,13,52,23,46,9],[15,54,17,44,21,48,11,50],[18,43,14,53,12,51,22,47]],[[4,23,50,55,6,25,58,39],[49,54,5,24,57,38,7,26],
    [22,3,56,51,28,1,40,59],[53,48,21,2,37,64,27,8],[20,35,52,29,14,9,60,41],[47,32,13,36,63,44,15,10],[34,19,30,45,12,17,42,61],[31,46,33,18,43,62,11,16]],[[6,1,54,29,52,27,48,43],
    [55,30,5,2,47,44,51,26],[4,7,32,53,28,49,42,45],[31,56,3,8,41,46,25,50],[62,9,58,33,24,19,40,15],[57,34,61,12,37,16,23,20],[10,63,36,59,18,21,14,39],[35,60,11,64,13,38,17,22]],
    [[6,3,30,57,28,55,42,39],[31,58,5,2,43,40,27,54],[4,7,60,29,56,25,38,41],[59,32,1,8,37,44,53,26],[18,61,36,45,16,9,24,51],[33,46,17,64,21,52,15,12],[62,19,48,35,10,13,50,23],
    [47,34,63,20,49,22,11,14]],[[6,3,34,61,32,19,46,59],[35,62,5,2,47,60,31,18],[4,7,64,33,20,29,58,45],[63,36,1,8,57,48,17,30],[38,9,56,49,28,21,44,15],[53,50,37,12,41,16,27,24],
    [10,39,52,55,22,25,14,43],[51,54,11,40,13,42,23,26]],[[6,3,46,57,44,31,18,55],[47,58,5,2,19,56,43,30],[4,7,60,45,32,41,54,17],[59,48,1,8,53,20,29,42],[10,61,52,21,40,33,16,27],
    [49,22,9,64,13,28,39,36],[62,11,24,51,34,37,26,15],[23,50,63,12,25,14,35,38]],[[6,3,58,51,30,43,54,15],[59,50,7,4,53,14,31,42],[2,5,52,57,44,29,16,55],[49,60,1,8,13,56,41,32],
    [64,9,24,45,40,33,28,17],[23,48,61,12,25,20,37,34],[10,63,46,21,36,39,18,27],[47,22,11,62,19,26,35,38]],[[6,3,62,41,8,43,50,47],[61,40,7,4,49,46,9,44],[2,5,38,63,42,11,48,51],
    [39,60,1,16,37,52,45,10],[58,29,36,21,64,17,12,23],[35,32,59,26,15,22,53,18],[28,57,30,33,20,55,24,13],[31,34,27,56,25,14,19,54]],[[6,19,32,61,28,21,34,59],[31,62,7,20,33,60,25,22],
    [18,5,64,29,24,27,58,35],[63,30,17,8,57,36,23,26],[50,47,4,41,16,9,56,37],[1,44,49,46,53,40,15,12],[48,51,42,3,10,13,38,55],[43,2,45,52,39,54,11,14]],[[6,23,48,63,26,1,50,43],
    [47,62,5,24,49,44,27,2],[22,7,64,45,4,25,42,51],[61,46,21,8,41,52,3,28],[20,9,36,57,32,13,40,53],[35,60,17,12,37,56,29,14],[10,19,58,33,16,31,54,39],[59,34,11,18,55,38,15,30]],
    [[6,23,64,45,4,25,50,43],[63,46,5,24,49,44,3,26],[22,7,48,61,28,1,42,51],[47,62,21,8,41,52,27,2],[34,9,60,17,56,29,40,15],[59,20,33,12,37,16,53,30],[10,35,18,57,32,55,14,39],
    [19,58,11,36,13,38,31,54]],[[6,23,64,45,4,25,50,43],[63,46,5,24,49,44,3,26],[22,7,48,61,28,51,42,1],[47,62,21,8,41,2,27,52],[34,9,60,17,56,29,40,15],[59,20,33,12,37,16,53,30],
    [10,35,18,57,32,55,14,39],[19,58,11,36,13,38,31,54]],[[6,27,50,47,18,39,14,59],[51,48,7,26,15,58,17,38],[28,5,46,49,40,19,60,13],[45,52,25,8,57,16,37,20],[4,29,56,41,24,33,12,61],
    [53,44,3,32,9,62,21,36],[30,1,42,55,34,23,64,11],[43,54,31,2,63,10,35,22]],[[6,27,62,33,60,3,38,31],[63,34,5,28,37,32,59,2],[26,7,36,61,4,39,30,57],[35,64,25,8,29,58,1,40],
    [50,9,48,21,44,17,56,15],[47,24,51,12,53,14,41,18],[10,49,22,45,20,43,16,55],[23,46,11,52,13,54,19,42]],[[6,27,62,33,60,3,38,31],[63,34,5,28,37,32,59,2],[26,7,36,61,4,57,30,39],
    [35,64,25,8,29,40,1,58],[24,9,22,45,20,43,56,41],[49,46,51,12,53,14,19,16],[10,23,48,21,44,17,42,55],[47,50,11,52,13,54,15,18]],[[6,27,62,33,60,3,38,31],[63,34,5,28,37,32,59,2],
    [26,7,36,61,4,57,30,39],[35,64,25,8,29,40,1,58],[24,9,50,45,20,15,56,41],[49,46,23,12,53,42,19,16],[10,51,48,21,44,17,14,55],[47,22,11,52,13,54,43,18]],[[6,27,62,33,60,3,38,31],
    [63,34,5,28,37,32,59,2],[26,7,36,61,4,57,30,39],[35,64,25,8,29,40,1,58],[24,9,52,45,20,13,56,41],[49,46,21,12,53,44,19,16],[10,23,48,51,14,17,42,55],[47,50,11,22,43,54,15,18]],
    [[6,27,62,33,60,3,38,31],[63,34,5,28,37,32,59,2],[26,7,36,61,4,57,30,39],[35,64,25,8,29,40,1,58],[50,9,48,21,44,17,56,15],[47,24,51,12,53,14,41,18],[10,49,22,45,20,43,16,55],
    [23,46,11,52,13,54,19,42]],[[6,31,50,55,10,15,58,35],[49,54,7,32,57,34,11,16],[30,5,56,51,14,9,36,59],[53,48,25,8,33,64,17,12],[4,29,52,41,24,13,60,37],[47,44,1,26,63,40,21,18],
    [28,3,42,45,20,23,38,61],[43,46,27,2,39,62,19,22]],[[6,31,58,41,4,39,46,35],[59,42,5,32,45,36,3,38],[30,7,44,57,40,1,34,47],[43,60,29,8,33,48,37,2],[28,17,62,13,56,9,52,23],
    [61,14,27,20,49,24,55,10],[18,63,16,25,12,53,22,51],[15,26,19,64,21,50,11,54]],[[6,35,60,29,4,37,62,27],[59,32,5,36,61,28,1,38],[34,7,30,57,40,3,26,63],[31,58,33,8,25,64,39,2],
    [18,9,56,45,16,41,24,51],[55,46,17,12,21,52,15,42],[10,19,48,53,44,13,50,23],[47,54,11,20,49,22,43,14]],[[6,35,60,29,4,37,62,27],[59,32,5,36,61,28,1,38],[34,7,30,57,40,3,26,63],
    [31,58,33,8,25,64,39,2],[46,9,56,17,52,41,24,15],[55,18,45,12,21,16,51,42],[10,47,20,53,44,49,14,23],[19,54,11,48,13,22,43,50]],[[6,35,62,41,8,43,18,47],[61,40,7,36,17,46,9,44],
    [34,5,38,63,42,11,48,19],[39,60,33,16,37,20,45,10],[58,29,4,21,64,49,12,23],[3,32,59,26,15,22,53,50],[28,57,30,1,52,55,24,13],[31,2,27,56,25,14,51,54]],[[6,39,58,31,56,41,10,19],
    [59,30,7,40,9,18,43,54],[38,5,32,57,42,55,20,11],[29,60,37,8,17,12,53,44],[64,33,4,25,52,45,16,21],[3,28,61,36,13,24,49,46],[34,63,26,1,48,51,22,15],[27,2,35,62,23,14,47,50]],
    [[6,47,62,27,10,19,58,31],[63,26,7,46,59,30,11,18],[48,5,28,61,20,9,32,57],[25,64,45,8,29,60,17,12],[44,49,4,21,40,13,56,33],[1,24,41,52,35,54,37,16],[50,43,22,3,14,39,34,55],
    [23,2,51,42,53,36,15,38]],[[6,51,64,1,54,39,10,35],[63,2,5,52,9,36,55,38],[4,7,50,61,40,53,34,11],[49,62,3,8,33,12,37,56],[26,23,48,41,60,17,32,13],[47,44,27,24,29,14,57,18],
    [22,25,42,45,20,59,16,31],[43,46,21,28,15,30,19,58]],[[7,2,49,60,9,64,47,22],[50,59,8,1,48,21,10,63],[3,6,57,44,61,46,23,20],[58,51,4,13,24,11,62,37],[5,30,43,56,45,36,19,26],
    [52,55,14,29,12,25,38,35],[31,42,53,16,33,40,27,18],[54,15,32,41,28,17,34,39]],[[7,10,23,60,25,62,35,38],[22,59,8,11,34,37,26,63],[9,6,57,24,61,28,39,36],[58,21,12,5,40,33,64,27],
    [19,56,41,48,13,4,29,50],[44,47,20,53,32,49,14,1],[55,18,45,42,3,16,51,30],[46,43,54,17,52,31,2,15]],[[7,10,53,64,3,42,51,30],[62,55,6,9,52,31,2,43],[11,8,63,54,41,4,29,50],
    [56,61,12,5,32,49,44,1],[13,34,17,60,45,40,23,28],[18,57,14,33,24,27,48,39],[35,16,59,20,37,46,25,22],[58,19,36,15,26,21,38,47]],[[7,10,63,54,3,42,51,30],[62,55,6,9,52,31,2,43],
    [11,8,53,64,41,4,29,50],[56,61,12,5,32,49,44,1],[13,34,17,60,45,40,23,28],[18,57,14,33,24,27,48,39],[35,16,59,20,37,46,25,22],[58,19,36,15,26,21,38,47]],[[7,22,47,62,9,20,35,58],
    [46,63,8,21,34,59,18,11],[23,6,61,48,19,10,57,36],[64,45,24,5,60,33,12,17],[25,4,49,44,29,16,37,56],[50,43,28,1,40,53,32,13],[3,26,41,52,15,30,55,38],[42,51,2,27,54,39,14,31]],
    [[7,22,47,62,9,20,35,58],[46,63,8,21,34,59,18,11],[23,6,61,48,19,10,57,36],[64,45,24,5,60,33,12,17],[25,4,49,44,29,16,37,56],[50,43,28,1,54,39,32,13],[3,26,41,52,15,30,55,38],
    [42,51,2,27,40,53,14,31]],[[7,22,47,62,19,10,35,58],[46,63,8,21,34,59,18,11],[23,6,61,48,9,20,57,36],[64,45,24,5,60,33,12,17],[25,4,49,44,29,16,37,56],[50,43,28,1,40,53,32,13],
    [3,26,41,52,15,30,55,38],[42,51,2,27,54,39,14,31]],[[7,22,47,62,19,10,35,58],[46,63,8,21,34,59,18,11],[23,6,61,48,9,20,57,36],[64,45,24,5,60,33,12,17],[25,4,49,44,29,16,37,56],
    [50,43,28,1,54,39,32,13],[3,26,41,52,15,30,55,38],[42,51,2,27,40,53,14,31]],[[7,24,41,58,19,10,63,38],[42,57,8,23,62,39,18,11],[25,6,59,40,9,20,37,64],[56,43,22,1,48,61,12,17],
    [5,26,47,60,21,16,49,36],[44,55,2,29,34,51,32,13],[27,4,53,46,15,30,35,50],[54,45,28,3,52,33,14,31]],[[7,30,43,52,1,54,47,26],[42,51,6,29,48,27,2,55],[31,8,49,44,53,4,25,46],
    [50,41,32,5,28,45,56,3],[9,60,39,22,33,58,15,24],[40,21,10,59,16,23,34,57],[61,38,19,12,63,36,17,14],[20,11,62,37,18,13,64,35]],[[7,30,43,52,1,54,47,26],[42,51,6,29,48,27,2,55],
    [31,8,49,44,53,4,25,46],[50,41,32,5,28,45,56,3],[9,62,39,20,33,24,15,58],[40,19,10,61,16,57,34,23],[63,38,17,12,21,36,59,14],[18,11,64,37,60,13,22,35]],[[7,30,43,52,1,54,47,26],
    [42,51,6,29,48,27,2,55],[31,8,49,44,53,4,25,46],[50,41,32,5,28,45,56,3],[9,62,39,20,33,58,15,24],[40,19,10,61,16,23,34,57],[63,38,17,12,21,36,59,14],[18,11,64,37,60,13,22,35]],
    [[7,30,43,52,1,54,47,26],[42,51,6,29,48,27,2,55],[31,8,49,44,53,4,25,46],[50,41,32,5,28,45,56,3],[39,60,9,22,33,58,15,24],[10,21,40,59,16,23,34,57],[61,38,19,12,63,36,17,14],
    [20,11,62,37,18,13,64,35]],[[7,30,43,52,1,54,47,26],[42,51,6,29,48,27,2,55],[31,8,49,44,53,4,25,46],[50,41,32,5,28,45,56,3],[39,62,9,20,33,24,15,58],[10,19,40,61,16,57,34,23],
    [63,38,17,12,21,36,59,14],[18,11,64,37,60,13,22,35]],[[7,30,43,52,1,54,47,26],[42,51,6,29,48,27,2,55],[31,8,49,44,53,4,25,46],[50,41,32,5,28,45,56,3],[39,62,9,20,33,58,15,24],
    [10,19,40,61,16,23,34,57],[63,38,17,12,21,36,59,14],[18,11,64,37,60,13,22,35]],[[7,34,49,28,9,32,47,54],[50,27,8,33,48,53,10,31],[35,6,25,52,29,12,55,46],[26,51,36,5,56,45,30,11],
    [37,62,19,24,13,4,43,58],[20,23,38,61,44,57,14,3],[63,18,21,40,1,16,59,42],[22,39,64,17,60,41,2,15]],[[7,42,25,56,9,40,23,58],[54,27,8,41,24,57,38,11],[43,6,55,26,39,10,59,22],
    [28,53,44,5,60,21,12,37],[1,48,29,52,33,16,61,20],[30,51,4,45,62,19,36,13],[47,2,49,32,15,34,17,64],[50,31,46,3,18,63,14,35]],[[7,42,25,56,39,10,23,58],[54,27,8,41,24,57,38,11],
    [43,6,55,26,9,40,59,22],[28,53,44,5,60,21,12,37],[1,48,29,52,33,16,61,20],[30,51,4,45,62,19,36,13],[47,2,49,32,15,34,17,64],[50,31,46,3,18,63,14,35]],[[7,42,25,56,39,10,23,58],
    [54,27,8,41,24,57,38,11],[43,6,55,26,9,40,59,22],[28,53,44,5,60,21,12,37],[45,4,49,32,13,36,17,64],[52,29,46,3,20,61,14,35],[1,48,31,50,33,16,63,18],[30,51,2,47,62,19,34,15]],
    [[7,42,55,26,39,10,23,58],[54,27,8,41,24,57,38,11],[43,6,25,56,9,40,59,22],[28,53,44,5,60,21,12,37],[1,48,29,52,33,16,61,20],[30,51,4,45,62,19,36,13],[47,2,49,32,15,34,17,64],
    [50,31,46,3,18,63,14,35]],[[7,46,55,30,57,44,3,18],[54,31,6,45,4,19,42,59],[47,8,29,56,43,58,17,2],[32,53,48,5,20,1,60,41],[9,28,33,52,37,16,21,64],[34,49,26,11,24,61,40,15],
    [27,10,51,36,13,38,63,22],[50,35,12,25,62,23,14,39]],[[10,7,50,59,14,23,62,35],[51,58,11,8,61,34,15,22],[6,9,60,49,24,13,36,63],[57,52,5,12,33,64,21,16],[4,31,48,53,20,25,42,37],
    [47,56,3,32,41,38,17,26],[30,1,54,45,28,19,40,43],[55,46,29,2,39,44,27,18]],[[10,35,58,63,18,23,14,39],[57,64,11,36,13,38,17,24],[34,9,62,59,22,19,40,15],[61,56,33,12,37,16,25,20],
    [8,3,60,29,52,21,46,41],[55,32,5,2,47,44,49,26],[4,7,30,53,28,51,42,45],[31,54,1,6,43,48,27,50]],[[11,14,33,56,9,52,31,54],[34,57,10,13,32,55,8,51],[15,12,59,36,49,6,53,30],
    [58,35,16,5,60,29,50,7],[39,18,61,28,37,48,3,26],[62,21,38,17,4,27,44,47],[19,40,23,64,45,42,25,2],[22,63,20,41,24,1,46,43]],[[11,14,33,58,9,50,31,54],[34,59,10,13,32,55,8,49],
    [15,12,57,36,51,6,53,30],[60,35,20,5,56,29,48,7],[39,16,61,24,37,52,3,28],[62,21,38,19,4,25,44,47],[17,40,23,64,45,42,27,2],[22,63,18,41,26,1,46,43]],[[11,14,43,64,45,18,31,34],
    [42,63,12,15,30,33,46,19],[13,10,61,44,17,48,35,32],[62,41,16,9,36,29,20,47],[7,60,37,28,49,56,1,22],[40,27,8,57,4,21,50,53],[59,6,25,38,55,52,23,2],[26,39,58,5,24,3,54,51]],
    [[11,18,59,36,13,22,63,38],[58,35,12,17,64,37,14,23],[19,10,33,60,21,16,39,62],[34,57,20,9,40,61,24,15],[55,8,45,32,49,4,41,26],[46,31,56,5,44,25,50,3],[7,54,29,48,1,52,27,42],
    [30,47,6,53,28,43,2,51]],[[11,20,47,42,23,14,53,50],[46,41,12,21,52,49,24,15],[19,10,43,48,13,22,51,54],[40,45,18,1,60,55,16,25],[9,64,39,44,17,2,29,56],[38,35,6,61,32,59,26,3],
    [63,8,33,36,5,28,57,30],[34,37,62,7,58,31,4,27]],[[11,22,47,50,3,26,63,38],[46,49,10,23,64,39,2,27],[21,12,51,48,25,4,37,62],[52,45,24,9,40,61,28,1],[13,20,41,56,29,36,5,60],
    [44,53,14,17,8,57,32,35],[19,16,55,42,33,30,59,6],[54,43,18,15,58,7,34,31]],[[11,34,13,60,7,62,23,50],[58,15,10,35,22,51,6,63],[33,12,59,14,61,8,49,24],[16,57,36,9,52,21,64,5],
    [37,32,53,20,41,4,25,48],[56,17,40,29,46,27,44,1],[31,38,19,54,3,42,47,26],[18,55,30,39,28,45,2,43]],[[11,34,13,60,7,62,47,26],[58,15,10,35,46,27,6,63],[33,12,59,14,61,8,25,48],
    [16,57,36,9,28,45,64,5],[37,32,53,20,41,4,49,24],[56,17,40,29,52,21,44,1],[31,38,19,54,3,42,23,50],[18,55,30,39,22,51,2,43]],[[14,11,64,59,16,39,30,27],[63,60,15,12,29,26,17,38],
    [10,13,58,61,40,19,28,31],[57,62,9,20,25,32,37,18],[8,3,56,45,36,41,24,47],[55,52,7,4,21,46,33,42],[2,5,50,53,44,35,48,23],[51,54,1,6,49,22,43,34]],[[15,22,39,64,17,2,59,42],
    [38,63,16,21,60,41,18,3],[23,14,61,40,1,20,43,58],[62,37,24,13,44,57,4,19],[25,12,35,52,29,6,45,56],[36,51,26,9,48,55,30,5],[11,34,49,28,53,32,7,46],[50,27,10,33,8,47,54,31]],
    [[15,30,53,40,3,42,51,26],[38,55,14,29,52,27,2,43],[31,16,39,54,41,4,25,50],[56,37,32,13,28,49,44,1],[17,12,57,36,45,24,5,64],[58,33,10,19,8,61,48,23],[11,18,35,60,21,46,63,6],
    [34,59,20,9,62,7,22,47]],[[15,30,53,40,3,42,51,26],[38,55,14,29,52,27,2,43],[31,16,39,54,41,4,25,50],[56,37,32,13,28,49,44,1],[17,12,57,36,45,24,5,64],[58,33,20,9,62,7,48,23],
    [11,18,35,60,21,46,63,6],[34,59,10,19,8,61,22,47]],[[18,15,58,9,56,7,50,47],[59,12,19,16,49,46,53,6],[14,17,10,57,8,55,48,51],[11,60,13,20,45,52,5,54],[62,21,44,29,40,25,36,3],
    [43,30,61,24,33,4,39,26],[22,63,32,41,28,37,2,35],[31,42,23,64,1,34,27,38]],[[19,10,39,62,7,22,59,42],[38,63,20,9,60,41,6,23],[11,18,61,40,21,8,43,58],[64,37,12,17,44,57,24,5],
    [13,32,49,36,25,4,45,56],[50,35,16,29,48,53,26,3],[31,14,33,52,1,28,55,46],[34,51,30,15,54,47,2,27]],[[19,10,39,62,21,8,59,42],[38,63,20,9,60,41,6,23],[11,18,61,40,7,22,43,58],
    [64,37,12,17,44,57,24,5],[13,32,49,36,25,4,45,56],[50,35,16,29,48,53,26,3],[31,14,33,52,1,28,55,46],[34,51,30,15,54,47,2,27]],[[19,10,61,40,7,22,59,42],[38,63,20,9,60,41,6,23],
    [11,18,39,62,21,8,43,58],[64,37,12,17,44,57,24,5],[13,32,49,36,25,4,45,56],[50,35,16,29,48,53,26,3],[31,14,33,52,1,28,55,46],[34,51,30,15,54,47,2,27]],[[19,10,61,40,21,8,59,42],
    [38,63,20,9,60,41,6,23],[11,18,39,62,7,22,43,58],[64,37,12,17,44,57,24,5],[13,32,49,36,25,4,45,56],[50,35,16,29,48,53,26,3],[31,14,33,52,1,28,55,46],[34,51,30,15,54,47,2,27]]]