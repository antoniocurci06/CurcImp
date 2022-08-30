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
    deriving Show

-- Arithmetic Expressions
data ArExp =
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

data stringExp = 
    string [stringExp]
    | stringId String
    | concatenation stringExp stringExp


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
    | push String ArExp
    | pop String ArExp
	deriving Show

-- Program
type Program = [Command]