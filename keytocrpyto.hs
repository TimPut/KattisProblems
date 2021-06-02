import Data.Char (ord,chr)

main = do
  [ciphertext,key] <- lines <$> getContents
  let message = zipWith (.-) ciphertext (key++message)
  putStr message
  
(.-) a b = chr' $ (ord' a - ord' b) `mod` 26
chr' a = chr $ fromIntegral a + ord 'A'
ord' a = fromIntegral (ord a - ord 'A')
