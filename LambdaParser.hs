module LambdaParser (
  Expr(Variable, Definition, Application),
  parseLambda
) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

data Expr = Variable String
          | Definition Expr Expr
          | Application Expr Expr
          deriving (Show, Eq)

variable :: ParsecT String u Identity Expr
variable = Variable <$> many1 (letter)

functionDefinition :: ParsecT String u Identity Expr
functionDefinition = do
  char 'λ'
  var <- variable
  char '.'
  fExpr <- expr
  return $ Definition var fExpr

functionApplication :: ParsecT String u Identity Expr
functionApplication = do
  char '('
  expr1 <- expr
  char ' '
  expr2 <- expr
  char ')'
  return $ Application expr1 expr2

expr :: ParsecT String u Identity Expr
expr = try functionDefinition
    <|> try variable
    <|> try functionApplication

parseLambda :: String -> Either ParseError Expr
parseLambda s = parse expr [] s
