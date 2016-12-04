module MyLisp where

import Control.Monad
import Data.Maybe
import Text.ParserCombinators.Parsec

data LVal = LAtom String
          | LList [LVal]
          | LNumber Double
          | LString String
          | LBool Bool
          | LFunction [LVal] LVal
          | LIf LVal LVal LVal
            deriving (Eq, Ord, Show, Read)

type LScope = [(LVal, LVal)]

run :: String -> Either ParseError LVal
run s = eval [] <$> readExpr s

eval :: LScope -> LVal -> LVal
eval scope (LAtom atom) = fromJust $ lookup (LAtom atom) scope
eval _ (LList [LAtom "quote", val]) = val
eval scope (LList (LFunction binds body : args)) = eval (zip binds (map (eval scope) args) ++ scope) body
eval scope (LList (LAtom func : args)) = case lookup (LAtom func) scope of
                                             Just f -> eval scope (LList (f : args))
                                             _ -> (fromJust $ lookup func primitives) $ map (eval scope) args
eval scope (LIf p t f) = case eval scope p of
                             LBool False -> eval scope f
                             _ -> eval scope t
eval _ n = n

primitives :: [(String, [LVal] -> LVal)]
primitives = [("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop (/))
             , ("log", numericBinop logBase)
             , ("mod", numericBinop (\a b -> fromInteger $ mod (floor a) (floor b)))
             , ("show", LString . show . head)
             , ("=", \[a, b] -> LBool $ a == b)
             ]

numericBinop :: (Double -> Double -> Double) -> [LVal] -> LVal
numericBinop op params = LNumber . foldl1 op $ map unpackNum params

unpackNum :: LVal -> Double
unpackNum (LNumber n) = n
unpackNum n = error $ "is is not number: " ++ show n

readExpr :: String -> Either ParseError LVal
readExpr = parse parseExpr "MyLisp"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseExpr :: Parser LVal
parseExpr = spaces *>
            (try parseIf <|>
             try parseFunction <|>
             parseList <|>
             parseQuoted <|>
             parseString <|>
             parseNumber <|>
             parseAtom
            )

parseAtom :: Parser LVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ case first : rest of
                 "#t" -> LBool True
                 "#f" -> LBool False
                 atom -> LAtom atom

parseList :: Parser LVal
parseList = char '(' *> liftM LList (sepBy parseExpr spaces) <* char ')'

parseNumber :: Parser LVal
parseNumber = liftM (LNumber . read) $ many1 digit

parseString :: Parser LVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ LString x

parseQuoted :: Parser LVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ LList [LAtom "quote", x]

parseFunction :: Parser LVal
parseFunction = do
    _ <- char '('
    _ <- string "lambda"
    _ <- spaces
    args <- maybeAtoms <$> parseList
    _ <- spaces
    f <- parseExpr
    _ <- char ')'
    return $ LFunction (fromJust args) f

parseIf :: Parser LVal
parseIf = do
    _ <- char '('
    _ <- string "if"
    p <- parseExpr
    t <- parseExpr
    f <- parseExpr
    _ <- char ')'
    return $ LIf p t f

maybeAtoms :: LVal -> Maybe [LVal]
maybeAtoms (LList xs) = mapM maybeAtom xs
maybeAtoms _ = Nothing

maybeAtom :: LVal -> Maybe LVal
maybeAtom x@(LAtom _) = Just x
maybeAtom _ = Nothing
