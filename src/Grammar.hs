module Grammar where
-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022

-- Values
data Value = 
    T_Integer Int
    | T_Boolean Bool
    | T_Char Char
    | T_String String
    | T_Stack [Int]
    deriving Show

-- Arithmetic Expressions
data ArExp =
        Constant Int
        | ArId String  
        | Stack String ArExp
        | Sum ArExp ArExp
        | Difference ArExp ArExp
        | Multiplied_by ArExp ArExp
        | Divided_by ArExp ArExp
        | Power ArExp ArExp
        deriving Show

data StringExp = 
    StringId String
    | Concatenation StringExp StringExp


-- Boolean Expressions
data BoolExp =
          Bool Bool
        | BoolId String
        | LessThan ArExp ArExp
        | GreaterThan ArExp ArExp
        | EqualTo ArExp ArExp
        | NotEqualTo ArExp ArExp
        | LessEqualThan ArExp ArExp
        | GreaterEqualThan ArExp ArExp
        | AND BoolExp BoolExp
        | OR BoolExp BoolExp
        | NOT BoolExp
        deriving Show

-- Commands
data Command =
    Skip
    | IfElse BoolExp [Command] [Command]
    | Whiledo BoolExp [Command]
    | ArAssignment String ArExp
    | BoolAssignment String BoolExp  
    | ArDeclaration String ArExp
    | BoolDeclaration String BoolExp
    | StackDeclaration String ArExp 
    | StackAssignment String ArExp ArExp     
    | Push String ArExp
    | Pop String

-- Program
type Program = [Command]