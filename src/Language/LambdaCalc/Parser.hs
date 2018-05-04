{-# Language RankNTypes #-}
-- | A macro-aware parser for the λ-calculus using my 
-- regular syntax (# for macros, ` for foreign,
-- x ::= [ e ] for x is a macro inserting e)
module Language.LambdaCalc.Parser where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import qualified Language.LambdaCalc.Syntax as S
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

data Pos = Pos {_row, _col :: Int}
  deriving (Eq, Ord, Show)
               
data Tok = LParen | RParen | Var String | Macro String | Assign
         | LBrace | RBrace | Foreign String | Lambda | Dot
  deriving (Eq, Ord, Show)

type AnnotatedTok = (Tok, Pos)

annotatePosition :: String -> [(Char, Pos)]
annotatePosition = go 1 1
  where
    go _ _ [] = []
    go row _ ('\n':xs) = go (row+1) 1 xs
    go row col (x:xs) = (x, (Pos row col)): go row (col+1) xs
  
splitWord :: [(Char, Pos)] -> (String, [(Char, Pos)])
splitWord [] = ("", [])
splitWord xs@((_,Pos{_row = row}):_) = (map fst s, rest)
  where
    (s, rest) = break continuing xs
    continuing (c,Pos{_row = row'}) = c `elem` "[]().λ " || row /= row'

tokenize :: String -> [(Tok, Pos)]
tokenize = go . annotatePosition
  where
    go [] = []
    go ((x,p):xs) = case x of
      '(' -> (LParen, p): go xs
      ')' -> (RParen, p): go xs
      '[' -> (LBrace, p): go xs
      ']' -> (RBrace, p): go xs
      'λ' -> (Lambda, p): go xs
      '.' -> (Dot , p): go xs
      ':' | map fst (take 2 xs) == ":=" -> (Assign, p): go (drop 2 xs)
      ' ' -> go xs
      '#' -> let (s, rest) = splitWord xs in (Macro s, p): go rest
      '`' -> let (s, rest) = splitWord xs in (Foreign s, p): go rest
      _ -> (Var [x], p): go xs

data ParserState = ParserState { _toks :: [AnnotatedTok]
                               , _macros :: Map String S.Lambda}
  deriving Show

-- CPS is still in vogue, right?
newtype Parser a = Parser {runParser :: forall r . ParserState ->
                            (a -> ParserState -> Either [String] r)
                            -> Either [String] r} 

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s k ->
    let k' x s' = k (f x) s' in p s k'

instance Applicative Parser where
  pure x = Parser $ \s k -> k x s
  (Parser pf) <*> (Parser px) = Parser $ \s k -> pf s $ \f s' ->
    px s' (k . f)

instance Alternative Parser where
  empty = Parser $ \_ _ -> Left []
  -- Every problem stuck together in an unreadable format? Yes please
  -- tbh I didn't really think this through at all
  -- we'll just delet them
  (Parser x) <|> (Parser y) = Parser $ \s k -> case x s k of
    Left _ -> case y s k of
      Left _ -> Left [] -- (err ++ err')
      res -> res
    res -> res
instance Monad Parser where
  (Parser p) >>= f = Parser $ \s k -> p s $ \x s' ->
    runParser (f x) s' k

throw :: String -> Parser a
throw err = Parser $ \_ _ -> Left [err]

eof :: Parser Bool
eof = Parser $ \s k -> k (null (_toks s)) s

next :: Parser AnnotatedTok
next = Parser $ \s k -> case _toks s of
  [] -> Left ["End of input encountered"]
  x:xs -> k x (s {_toks = xs})

lookupMacro :: String -> Parser S.Lambda
lookupMacro m = Parser $ \s k -> case M.lookup m (_macros s) of
  Nothing -> Left [unwords ["Couldn't find macro", m]]
  Just x -> k x s

addMacro :: String -> S.Lambda -> Parser ()
addMacro name def = Parser $ \s k -> k ()
  (s {_macros = M.insert name def (_macros s)})

unexpected :: String -> Pos -> Parser a
unexpected name p = throw (unwords [ "Expected", name, "at row"
                                   , (show $ _row p), "col", (show $ _col p)])

var :: Parser S.Lambda
var = do
  x <- next
  case x of
    (Var v, _) -> return $ S.Var v
    (_, p) -> unexpected "var" p

macro :: Parser S.Lambda
macro = do
  x <- next
  case x of
    (Macro m, _) -> lookupMacro m
    (_, p) -> unexpected "macro" p

foreignFun :: Parser S.Lambda
foreignFun = do
  x <- next
  case x of
    (Foreign f, _) -> return $ S.Foreign f
    (_, p) -> unexpected "foreign" p

dot :: Parser ()
dot = do
  x <- next
  case x of
    (Dot, _) -> return ()
    (_, p) -> unexpected "dot" p

assign :: Parser ()
assign = do
  x <- next
  case x of
    (Assign, _) -> return ()
    (_, p) -> unexpected "::=" p

lambda :: Parser S.Lambda
lambda = do
  x <- next
  case x of
    (Lambda, _) -> do
      vars <- some var
      dot
      rest <- expr
      return $ foldr S.Bind rest [v | S.Var v <- vars]
    (_, p) -> unexpected "λ" p

parens :: Parser a -> Parser a
parens parser = do
  x <- next
  case x of
    (LParen, _) -> do
      res <- parser
      close <- next
      case close of
        (RParen, _) -> return res
        (_, p) -> unexpected ")" p
    (_,p) -> unexpected "(" p

brackets :: Parser a -> Parser a
brackets parser = do
  x <- next
  case x of
    (LBrace, _) -> do
      res <- parser
      close <- next
      case close of
        (RBrace, _) -> return res
        (_, p) -> unexpected "]" p
    (_,p) -> unexpected "[" p

expr :: Parser S.Lambda
expr = foldl1 S.App <$> some simpleExpr

simpleExpr :: Parser S.Lambda
simpleExpr = var <|> macro <|> foreignFun <|> lambda <|> parens expr

-- Read in a macro definition
definition :: Parser ()
definition = do
  vars <- some var
  assign
  let name = concat [c | S.Var c <- vars]
  def <- brackets expr
  addMacro name def

-- A program is just a bunch of macros and then the expression itself
program :: Parser S.Lambda
program = do
  _ <- many definition
  res <- expr
  finished <- eof
  case finished of
    True -> return res
    False -> throw ("Expected end of file")

definitionsOnly :: Parser ()
definitionsOnly = do
  _ <- many definition
  finished <- eof
  case finished of
    True -> return ()
    False -> throw ("Expected end of file")

parseProgram :: Map String S.Lambda -> String -> Either [String] S.Lambda
parseProgram stdlib prog = runParser program
  (ParserState (tokenize prog) stdlib) (\x _ -> Right x)

generateStdlib :: String -> Either [String] (Map String S.Lambda)
generateStdlib defs = runParser definitionsOnly
  (ParserState (tokenize defs) M.empty) (\_ s -> Right (_macros s))

λlib :: QuasiQuoter
λlib = QuasiQuoter { quoteExp = \s -> case generateStdlib s of
                       Right m -> liftData m
                       Left err -> error (unlines err)
                   , quotePat = undefined
                   , quoteType = undefined
                   , quoteDec = undefined}
