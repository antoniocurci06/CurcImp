-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022

module Grammar where

-- Values
data Value = 
    int Int
    | bool Bool
    | char Data.char
    | string String
    | stack[Int]
    | stack[String]
    | stack[Int]
    deriving Show

-- Arithmetic Expressions
data ArExp =              -- all expression that give Integer as result
        constant Int
        | arId String
        | stackId String ArExp   
        | sum ArExp ArExp
        | difference ArExp ArExp
        | multiplied_by ArExp ArExp
        | divided_by ArExp ArExp
        | power ArExp ArExp
        deriving Show

-- Stack Expressions
data StackExp = 
    stack [ArExp]
    | stackId String

-- Boolean Expressions
data BoolExp =
          bool Bool
        | boolId String
        | lessThan ArExp ArExp
        | greaterThen ArExp ArExp
        | equalTo ArExp ArExp
        | notEqualTo ArExp ArExp
        | lessEqualThan ArExp ArExp
        | greaterEqualThan ArExp ArExp
        | AND BoolExp BoolExp
        | OR BoolExp BoolExp
        | NOT BoolExp
        deriving Show

-- Commands
data Command =
    skip
    | IfElse BoolExp [Command] [Command] 
	| Whiledo BoolExp [Command]
	| ArAssignment String ArExp
    | BoolAssignment String BoolExp  
	| ArDeclaration String ArExp
    | BoolDeclaration String BoolExp
    | stackDeclaration String ArExp 
    | stackAssignment String StackExp     
	deriving Show

-- Program
type Program = [Command]