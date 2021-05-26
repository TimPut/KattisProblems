import Text.Parsec
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as M
import Data.Maybe

main = do
  _ <- B.getLine
  rs <- fmap (read . B.unpack) . B.words <$> B.getLine :: IO [Double]
  let r = M.fromList $ zip [1..] rs
  bs <- B.getContents :: IO B.ByteString
  let parseResult = parse ast "error" bs
  case parseResult of
    Left err -> print err
    Right circuit -> print . eval r $ circuit

data Circuit a = Parallel [Circuit a]
               | Series [Circuit a]
               | Resistor a

eval env (Parallel cs) = recip . sum . fmap recip . fmap (eval env) $ cs
eval env (Series cs) = sum . fmap (eval env) $ cs 
eval env (Resistor r) = env M.! r 

ast :: Parsec B.ByteString () (Circuit M.Key)
ast = do
  ast <- try parallel <|> try series <|> resistor
  return ast
parallel = between (char '(') (char ')') $ do
  cs <- sepBy1 ast (char '|')
  return (Parallel cs)
series = between (char '(') (char ')') $ do
  cs <- sepBy1 ast (char '-')
  return (Series cs)
resistor = do
  char 'R'
  ds <- many1 digit
  return (Resistor ((read :: String -> Int) ds))
