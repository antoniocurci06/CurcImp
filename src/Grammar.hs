module Grammar where

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

data StackExp = 
    stack [ArExp]
    | stackId String

data BoolExp =                -- all expression that give Bool as result
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

data Command =
    skip
    | IfElse BoolExp [Command] [Command] -- probabile lista di comandi
	| Whiledo BoolExp [Command]
	| ArAssignment String ArExp
    | BoolAssignment String BoolExp  
	| ArDeclaration String ArExp
    | BoolDeclaration String BoolExp
    | stackDeclaration String ArExp 
    | stackAssignment String StackExp     -- array di tipo intero solo
	deriving Show
