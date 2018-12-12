module LambdaEvaluator (evaluateLambdaExpression) where

import Data.List (partition)
import Text.Parsec.Error (ParseError)
import LambdaParser

mapVariables :: (String -> String) -> Expr -> Expr
mapVariables f (Variable x) = Variable (f x)
mapVariables f (Definition x y) = Definition (mapVariables f x) (mapVariables f y)
mapVariables f (Application x y) = Application (mapVariables f x) (mapVariables f y)

nextName :: String -> [String] -> String
nextName name taken = let next = (name ++ "~") in
  if (elem next taken)
    then nextName next taken
    else next

rebind :: [String] -> Expr -> Expr
rebind taken (Definition (Variable name) x) = if (elem name taken)
  then let v' = (nextName name taken) in Definition (Variable v') (rebind (v':taken) x)
  else Definition (Variable name) (rebind taken x)
rebind taken (Application x y) = Application (rebind taken x) (rebind taken y)
rebind taken (Variable name) = if (elem name taken)
  then if (length taken > 1)
        then Variable (head taken)
        else Variable (nextName name taken)
  else Variable name

alphaConvert :: Expr -> Expr
alphaConvert (Application (Definition x y) (Variable z)) = let rx = (rebind [z] x) in
    Application (Definition rx (rebind [(getVarName rx), z] y))
                (Variable z) where
                  getVarName (Variable x) = x
alphaConvert x = x

applyApplication :: Expr -> Expr
applyApplication (Application definition argument) = applyLambdaExpr argument definition
applyApplication x = x

applyLambdaExpr :: Expr -> Expr -> Expr
applyLambdaExpr argument (Definition (Variable boundVariable) fnBody) = case fnBody of
  Variable v      -> if (v == boundVariable) then argument else Variable v

  Definition a b  -> (Definition a . betaReduction) $ mapVariables f b where
    f deepVar = if (deepVar == boundVariable) then getVarName argument else deepVar
    getVarName (Variable v) = v

  Application a b -> applyLambdaExpr argument
                      $ Definition (Variable boundVariable)
                      $ betaReduction (Application a b)
applyLambdaExpr _ x = x

betaReduction :: Expr -> Expr
betaReduction (Application (Variable x) y) = Application (Variable x) y
betaReduction (Application (Definition x y) (Variable z)) = applyApplication $ alphaConvert (Application (Definition x y) (Variable z))
betaReduction (Application (Definition x y) argument) = applyLambdaExpr argument (Definition x y)
betaReduction (Application (Application x y) argument) = applyLambdaExpr argument $ betaReduction $ applyApplication (Application x y)
betaReduction x = x

cleanupNames :: Expr -> Expr
cleanupNames x = mapVariables f x where
  f x = let parts = partition (\x -> x == '~') x in
    (snd parts) ++ show (length $ fst parts)

evaluateLambdaExpression :: String -> Either ParseError Expr
evaluateLambdaExpression expr = fmap betaReduction $ parseLambda expr

main = evaluateLambdaExpression "(λa.λb.λc.λd.λe.a x)"
