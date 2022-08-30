module Interpreter where
import Grammar
import Parser

-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022

-- Each variable has two fields: name and value.
data Variable = Variable {name  :: String,
                          value :: Value } deriving Show

-- Environment
type Environment = [Variable]

emptyState :: Environment
emptyState = empty

arEvaluation :: Environment -> AExp -> Maybe Integer
arEvaluation _ (Constant i) = Just i  
arEvaluation env (ArId s) =
  case get env s of
    Just (T_Integer v) -> Just v
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

arEvaluation env (Stack s i) =
  case get env s of
    Just (T_Stack a) -> Just (readArray a j)
      where Just j = arfEvaluation env i 
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"
arEvaluation env (Add a b) = (+) <$> arEvaluation env a <*> arEvaluation env b --Applicative
arEvaluation env (Sub a b) = (-) <$> arEvaluation env a <*> arEvaluation env b
arEvaluation env (Mul a b) = (*) <$> arEvaluation env a <*> arEvaluation env b

boolEvaluation :: Environment -> boolExp -> Maybe Bool
boolEvaluation _ (Boolean b) = Just b
boolEvaluation env (BoolId s) =
  case get env s of
    Just (T_Boolean v) -> Just v
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"
boolEvaluation env (LessThan a b) = pure (<) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (GreaterThan a b) = pure (>) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (EqualTo a b) = pure (==) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (NotEqualTo a b) = pure (/=) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (LessEqualThan a b) = pure (<=) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (GreaterEqualThan a b) = pure (>=) <*> (arithExprEval env a) <*> (arithExprEval env b)
boolEvaluation env (AND a b) = pure (&&) <*> (boolEvaluation env a) <*> (boolEvaluation env b)
boolEvaluation env (OR a b) = pure (||) <*> (boolEvaluation env a) <*> (boolEvaluation env b)
boolEvaluation env (NOT a) = not <$> boolEvaluation env a      


programExec :: Environment -> [Command] -> Environment

programExec env [] = env

programExec env (Skip : cs) = programExec env cs
programExec env ((ArDeclaration s ex) : cs) =
  case arEvaluation env ex of
    Just ex' -> case get env s of
      Just _ -> error "Another Declaration Was Found!"
      Nothing -> programExec (insert env s (Integer ex')) cs
    Nothing -> error "Arithmetic Expression Was Invalid"

programExec env ((BoolDeclaration s ex) : cs) =
  case boolEvaluation env ex of
    Just ex' -> case get env s of
      Just _ -> error "Another Declaration Was Found!"
      Nothing -> programExec (insert env s (T_Boolean ex')) cs
    Nothing -> error "Boolean Expression Was Invalid"

programExec env ((ArDeclaration s i) : cs) =
  case get env s of
    Just _ -> error "Another Declaration Was Found!"
    Nothing -> programExec (insert env s (stack (stackDeclaration j))) cs
    where Just j = arEvaluation env i

programExec env ((ArAssignment s ex) : cs) =
  case get env s of
    Just (Integer _) -> programExec (insert env s (Integer ex')) cs
      where
        Just ex' = arEvaluation env ex
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec env ((BoolAssignment s ex) : cs) =
  case get env s of
    Just (T_Boolean _) -> programExec (insert env s (bool ex')) cs
      where
        Just ex' = boolEvaluation env ex
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec env ((ArAssignment s i ex) : cs) =
  case get env s of
    Just (T_Stack a) -> programExec (insert env s (stack (push a j ex'))) cs
      where
        Just ex' = arEvaluation env ex
        Just j = arEvaluation env i
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec env ((IfThenElse b nc nc') : cs) =
  case boolEvaluation env b of
    Just True -> programExec env (nc ++ cs)
    Just False -> programExec env (nc' ++ cs)
    Nothing -> error "Boolean Expression Was Invalid"

programExec env ((Whiledo b c) : cs) =
  case boolEvaluation env b of
    Just True -> programExec env (c ++ [While b c] ++ cs)
    Just False -> programExec env cs
    Nothing -> error "Boolean Expression Was Invalid"