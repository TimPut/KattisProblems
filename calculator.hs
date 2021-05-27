import Text.Parsec.Token
import Text.Parsec
import Text.Parsec.Language
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Text.Printf

main = do
  bs <- B.lines <$> B.getContents :: IO [B.ByteString]
  sequence_ $ fmap (\l ->
                      case parse expr "error" (B.unpack l) of
                        Left err -> print err
                        Right expression -> printf "%.2f\n" . eval $ expression) bs

data Expr a = Add (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)            
            | Var a
  deriving (Show)

eval :: (Integral a) => Expr a -> Double
eval (Var a) = fromIntegral a
eval (Add e f) = eval e + eval f
eval (Sub e f) = eval e - eval f
eval (Mul e f) = eval e * eval f
eval (Div e f) = eval e / eval f

expr :: Parsec String () (Expr Integer)
expr    = term   `chainl1` addop

term    = factor `chainl1` mulop
factor  = parens lexer expr <|> (fmap Var (integer lexer))

mulop   =   do{ spaces; char '*'; spaces; return Mul }
        <|> do{ spaces; char '/'; spaces; return Div }

addop   =   do{ spaces; char '+'; spaces; return Add }
        <|> do{ spaces; char '-'; spaces; return Sub }

lexer = makeTokenParser emptyDef
