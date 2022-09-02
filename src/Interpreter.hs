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

arEvaluation :: Environment -> arExp -> Maybe Integer
arEvaluation _ (Constant i) = Just i  
arEvaluation env (ArId s) =
  case get env s of
    Just (T_Integer v) -> Just v
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"

arEvaluation env (Stack s i) =
  case get env s of
    Just (T_Stack a) -> Just (readArray a j)
      where Just j = arEvaluation env i 
    Just _ -> error "Mismatching Types!"
    Nothing -> error "No variable was found!"
arEvaluation env (Sum a b) = (+) <$> arEvaluation env a <*> arEvaluation env b --Applicative
arEvaluation env (Difference a b) = (-) <$> arEvaluation env a <*> arEvaluation env b
arEvaluation env (Multiplied_by a b) = (*) <$> arEvaluation env a <*> arEvaluation env b

boolEvaluation :: Environment -> boolExp -> Maybe Bool
boolEvaluation _ (T_Boolean b) = Just b
boolEvaluation env (BoolId s) =
  case get env s of
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

execProgr :: Environment -> [Command] -> Environment

execProgr e [] = e 

execProgr e  (Skip : cs) = execProgr e cs

execProgr e ((IfElse b nc nc') : cs) =
        case boolEvaluation e b of
                Just True -> execProgr e (nc ++ cs)
                Just False-> execProgr e (nc'++ cs)
                Nothing -> error "If Then Else Construct Produced an Error"


execProgr e ((Whiledo b nc) : cs) =
        case boolEvaluation e b of
                Just True -> execProgr e (nc ++ [(Whiledo b nc)] ++ cs)
                Just False -> execProgr e cs
                Nothing -> error "While Construct Produced an Error"

execProgr e ((ArAssignment s a) : cs ) =
        case searchVariable e s of
                Just (T_Integer _ ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (T_Integer z)
                                        where Just z = arEvaluation e a
                Just _ -> error "Mismatch in types was found"
                Nothing -> error "Error in Assignment" 


execProgr e ((BoolAssign s b) : cs ) =
        case searchVariable e s of
                Just (T_Boolean _ ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (T_Boolean z)
                                        where Just z = boolEvaluation e b
                Just _ -> error "Mismatching Types!"
                Nothing -> error "Error assign" 

execProgr e ((ArDeclaration s a ) : cs ) =
        case arEvaluation e a of
                Just exp -> case searchVariable e s of
                        Just _ -> error "Variable Already Declared"
                        Nothing -> execProgr (modifyEnv e var) cs
                               where var = Variable s (T_Integer z)
                                        where Just z = arEvaluation e a
                Nothing -> error "Declaration Produced an Error"

execProgr e ((BoolDeclare s a ) : cs ) =
        case boolEvaluation e a of
                Just exp -> case searchVariable e s of
                        Just _ -> error "Variable Already Declared"
                        Nothing -> execProgr (modifyEnv e var) cs
                               where var = Variable s (T_Boolean z)
                                        where Just z = boolEvaluation e a
                Nothing -> error "Declaration Produced an Error"


execProgr e ((StackAssignment s i a) : cs ) =
        case searchVariable e s of
                Just (T_Stack x ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (T_Stack z)
                                        where z = insertElemArray x j a' 
                                                where   
                                                        Just a'= arEvaluation e a 
                                                        Just j = arEvaluation e i
                Just _ -> error "Mismatching Types!"
                Nothing -> error "Error assign" 


execProgr e ((StackDeclaration s a) : cs ) =
        case searchVariable e s of
                Just _ -> error "Variable Already Declared"
                Nothing -> execProgr (modifyEnv e var) cs
                         where var = Variable s (T_Stack z)
                                 where z = stackDeclaration j 
                                        where Just j = arEvaluation e a
                Nothing -> error "Declaration Produced an Error"

execProgr env ((ArrFullAssign v exp) : cs) =
  case searchVariable env v of
    Just (T_Stack a) -> case arrExprEval env exp of
                            Just b -> if length a == length  b 
                            then
                                execProgr (modifyEnv env (Variable v (T_Stack b))) cs
                            else error "Length not valid!"
                            Nothing -> error "aExp evaluation of array failed"
    Nothing -> error "No variable was found!"