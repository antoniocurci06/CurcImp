module Interpreter where
import Grammar
import Parser

-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022

-- Each variable has two fields: name and value.
data Variable = Variable {name  :: String,
                          value :: Type } deriving Show

-- Environment
type Environment = [Variable]

emptyState :: Environment
emptyState = empty

arEvaluation :: Environment -> AExp -> Maybe Int
arEvaluation _ (Constant i) = Just i  
arEvaluation env (AVariable s) =
  case get env s of
    Just (int v) -> Just v
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

arEvaluation env (AArray s i) =
  case get env s of
    Just (ArrayType a) -> Just (readArray a j)
      where Just j = arfEvaluation env i 
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"
arEvaluation env (Add a b) = (+) <$> arEvaluation env a <*> arEvaluation env b --Applicative
arEvaluation env (Sub a b) = (-) <$> arEvaluation env a <*> arEvaluation env b
arEvaluation env (Mul a b) = (*) <$> arEvaluation env a <*> arEvaluation env b

boolEvaluation :: Environment -> boolExp -> Maybe Bool
boolEvaluation _ (Boolean b) = Just b
boolEvaluation env (BVariable s) =
  case get env s of
    Just (BoolType v) -> Just v
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"
boolEvaluation env (lessThan a b) = pure (<) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (greaterThan a b) = pure (>) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (equalTo a b) = pure (==) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (notEqualTo a b) = pure (/=) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (lessEqualThan a b) = pure (<=) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (greaterEqualThan a b) = pure (>=) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (AND a b) = pure (&&) <*> (boolEvaluation env a) <*> (boolEvaluation env b)
boolEvaluation env (OR a b) = pure (||) <*> (boolEvaluation env a) <*> (boolEvaluation env b)
boolEvaluation env (NOT a) = not <$> boolEvaluation env a      


programExec :: Environment -> [Command] -> Environment

programExec env [] = env

programExec env (Skip : cs) = programExec env cs
programExec env ((arDeclaration s ex) : cs) =
  case arEvaluation env ex of
    Just ex' -> case get env s of
      Just _ -> error "Another Declaration Was Found!"
      Nothing -> programExec (insert env s (int ex')) cs
    Nothing -> error "Arithmetic Expression Was Invalid"

programExec env ((boolDeclaration s ex) : cs) =
  case boolEvaluation env ex of
    Just ex' -> case get env s of
      Just _ -> error "Another Declaration Was Found!"
      Nothing -> programExec (insert env s (BoolType ex')) cs
    Nothing -> error "Boolean Expression Was Invalid"

programExec env ((arDeclaration s i) : cs) =
  case get env s of
    Just _ -> error "Another Declaration Was Found!"
    Nothing -> programExec (insert env s (stack (stackDeclaration j))) cs
    where Just j = arEvaluation env i

programExec env ((arAssignment s ex) : cs) =
  case get env s of
    Just (int _) -> programExec (insert env s (int ex')) cs
      where
        Just ex' = arEvaluation env ex
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec env ((boolAssignment s ex) : cs) =
  case get env s of
    Just (BoolType _) -> programExec (insert env s (bool ex')) cs
      where
        Just ex' = boolEvaluation env ex
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec env ((ArAssignment s i ex) : cs) =
  case get env s of
    Just (ArrayType a) -> programExec (insert env s (stack (push a j ex'))) cs
      where
        Just ex' = arEvaluation env ex
        Just j = arEvaluation env i
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec env ((ifThenElse b nc nc') : cs) =
  case boolEvaluation env b of
    Just True -> programExec env (nc ++ cs)
    Just False -> programExec env (nc' ++ cs)
    Nothing -> error "Boolean Expression Was Invalid"

programExec env ((whiledo b c) : cs) =
  case boolEvaluation env b of
    Just True -> programExec env (c ++ [While b c] ++ cs)
    Just False -> programExec env cs
    Nothing -> error "Boolean Expression Was Invalid"