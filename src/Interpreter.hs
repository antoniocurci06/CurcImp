-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022

module Interpreter where
import Grammar
import Parser

-- Each variable has two fields: name and value.
data Variable = Variable {name  :: String,  value :: Value } deriving Show

-- Environment
type Environment = [Variable]

emptyState :: Environment
emptyState = empty

arEvaluation :: Environment -> ArExp -> Maybe Int
arEvaluation _ (Constant i) = Just i  
arEvaluation env (ArId s) =
  case value s of
    Just (T_Integer v) -> Just v
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

arEvaluation env (Stack s i) =
  case value s of
    Just (T_Stack a) -> Just (readArray a j)
      where Just j = arEvaluation env i 
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

arEvaluation env (Sum a b) = (+) <$> arEvaluation env a <*> arEvaluation env b
arEvaluation env (Difference a b) = (-) <$> arEvaluation env a <*> arEvaluation env b
arEvaluation env (Multiplied_by a b) = (*) <$> arEvaluation env a <*> arEvaluation env b

boolEvaluation :: Environment -> BoolExp -> Maybe Bool
boolEvaluation _ (T_Boolean b) = Just b
boolEvaluation env (BoolId s) =
  case value s of
    Just (T_Boolean v) -> Just v
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

boolEvaluation env (LessThan a b) = pure (<) <*> (arEvaluation env a) <*> (arEvaluation env b)
boolEvaluation env (GreaterThan a b) = pure (>) <*> (arEvaluation env a) <*> (arEvaluation env b)
boolEvaluation env (EqualTo a b) = pure (==) <*> (arEvaluation env a) <*> (arEvaluation env b)
boolEvaluation env (NotEqualTo a b) = pure (/=) <*> (arEvaluation env a) <*> (arEvaluation env b)
boolEvaluation env (LessEqualThan a b) = pure (<=) <*> (arEvaluation env a) <*> (arEvaluation env b)
boolEvaluation env (GreaterEqualThan a b) = pure (>=) <*> (arEvaluation env a) <*> (arEvaluation env b)
boolEvaluation env (AND a b) = pure (&&) <*> (boolEvaluation env a) <*> (boolEvaluation env b)
boolEvaluation env (OR a b) = pure (||) <*> (boolEvaluation env a) <*> (boolEvaluation env b)
boolEvaluation env (NOT a) = not <$> boolEvaluation env a      

programExec :: Environment -> [Command] -> Environment

programExec progExe [] = progExe

programExec progExe (Skip : cs) = programExec progExe cs
programExec progExe ((ArDeclaration s ex) : cs) =
  case arEvaluation progExe ex of
    Just ex' -> case ex s of
      Just _ -> error "Another Declaration Was Found!"
      Nothing -> programExec (insert progExe s (T_Integer ex')) cs
    Nothing -> error "Arithmetic Expression Was Invalid"

programExec progExe ((BoolDeclaration s ex) : cs) =
  case boolEvaluation progExe ex of
    Just ex' -> case ex s of
      Just _ -> error "Another Declaration Was Found!"
      Nothing -> programExec (insert progExe s (T_Boolean ex')) cs
    Nothing -> error "Boolean Expression Was Invalid"

programExec progExe ((ArDeclaration s i) : cs) =
  case arEvaluation progExe s of
    Just _ -> error "Another Declaration Was Found!"
    Nothing -> programExec (insert progExe s (stack (stackDeclaration j))) cs
    where Just j = arEvaluation progExe i

programExec progExe ((ArAssignment s ex) : cs) =
  case ArAssignment progExe s of
    Just (T_Integer _) -> programExec (insert progExe s (T_Integer ex')) cs
      where
        Just ex' = arEvaluation progExe ex
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec progExe ((BoolAssignment s ex) : cs) =
  case get progExe s of
    Just (T_Boolean _) -> programExec (insert progExe s (T_Boolean ex')) cs
      where
        Just ex' = boolEvaluation progExe ex
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec progExe ((ArAssignment s i ex) : cs) =
  case get progExe s of
    Just (T_Stack a) -> programExec (insert progExe s (stack (push a j ex'))) cs
      where
        Just ex' = arEvaluation progExe ex
        Just j = arEvaluation progExe i
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

programExec progExe ((IfElse b nc nc') : cs) =
  case boolEvaluation progExe b of
    Just True -> programExec progExe (nc ++ cs)
    Just False -> programExec progExe (nc' ++ cs)
    Nothing -> error "Boolean Expression Was Invalid"

programExec progExe ((Whiledo b c) : cs) =
  case boolEvaluation progExe b of
    Just True -> programExec progExe (c ++ [While b c] ++ cs)
    Just False -> programExec progExe cs
    Nothing -> error "Boolean Expression Was Invalid"