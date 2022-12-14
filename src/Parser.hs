-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022

module Parser where
import Grammar
import Interpreter

-- Parser declaration
newtype Parser a = P (String -> [(a,String)]) 

instance Functor Parser where 
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g (P p) = P (\input -> case p input of 
        [] -> []
        [(v, out)] -> [(g v, out)]) 

instance Applicative Parser where                     
    --pure :: a -> Parser a 
    pure v = P(\input -> [(v, input)])                

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    (P pg) <*> px = P(\input -> case pg input of  
                [] -> []                              
                [(g, out)] -> case fmap g px of        
                                (P p) -> p out)                                     
  
instance Monad Parser where                         
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (P p) >>= f = P (\input -> case p input of 
                    [] -> []                    -- fail if the application fails
                    [(v, out)] -> case f v of   -- apply f to the result v to give another parser f v 
                                (P p) -> p out)
                    
class Monad f => Alternative f where  
  empty :: f a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  some :: f a -> f [a]
  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x
  chain :: f a -> f (a -> a -> a) -> f a          -- chain operator
  chain p op = do a <- p; rest a
        where
            rest a = (do f <- op; b <- p; rest (f a b)) <|> return a


instance Alternative Parser where     
  empty = P (const [])

  (P p) <|> (P q) =
    P( \input -> case p input of
          [] -> q input
          [(v, out)] -> [(v, out)]
      )

-- Derived Primitives, 13.6 from the Haskell Book
-- General Operator

item :: Parser Char  
item =
    P (\input -> case input of 
        [] -> []
        (x : xs) -> [(x, xs)]) 

sat :: (Char -> Bool) -> Parser Char 
sat p = do 
  x <- item
  if p x then return x else empty

-- Digits handling
digitSet :: [Char]
digitSet = ['0' .. '9']

digit :: Parser Char 
digit = sat isDigit

isDigit :: Char -> Bool
isDigit c = elem c digitSet

-- Chars handling
lowerSet :: [Char]
lowerSet = ['a' .. 'z']

lower :: Parser Char 
lower = sat isLower

isLower :: Char -> Bool
isLower c = elem c lowerSet

upperSet :: [Char] 
upperSet = ['A' .. 'Z']

upper :: Parser Char 
upper = sat isUpper

isUpper :: Char -> Bool
isUpper c = elem c upperSet

letterSet :: Parser Char 
letterSet = sat isLetter

isLetter :: Char -> Bool
isLetter c = elem c upperSet || elem c lowerSet

isAlphaNum :: Char -> Bool
isAlphaNum c = isLetter c || isDigit c

alphanum :: Parser Char 
alphanum = sat isAlphaNum

char :: Char -> Parser Char 
char x = sat (== x)

-- String Handling
string :: String -> Parser String 
string [] = return []
string (x:xs) = do 
      char x 
      string xs
      return (x:xs)

-- Identifiers for variables
ident :: Parser String 
ident = do 
    x <- lower
    xs <- many alphanum 
    return (x:xs)

-- Natural numbers
nat :: Parser Int
nat = do 
    xs <- some digit
    return (read xs)

-- Space handling
space :: Parser ()
space = do 
    many (sat isSpace)
    return ()

-- Integer numbers
int :: Parser Int 
int = do 
    char '-'
    n <- nat
    return (-n)
    <|> nat

-- Handling Spacing
spaceSet :: [Char]
spaceSet = ['\n', '\t', '\r', ' ']

isSpace :: Char -> Bool
isSpace x = elem x spaceSet

token :: Parser a -> Parser a 
token p = do 
    space
    v <- p 
    space 
    return v

identifier :: Parser String 
identifier = token ident

natural :: Parser Int 
natural = token nat

integer :: Parser Int 
integer = token int

symbol :: String -> Parser String 
symbol xs = token (string xs)

{-- nats :: Parser [Int] 
nats = do 
      symbol "["
      n <- natural
      ns <- many (do symbol "," natural) 
      symbol "]"
      return (n:ns)--}

-- Arithmetic Expressions
arExp  :: Parser ArExp         
arExp = do chain arTerm op
    where                        
      op = 
          (do symbol "added to"; return Sum)
          <|> do symbol "subtracted to"; return Difference 

arTerm :: Parser ArExp       
arTerm = do chain arFactor op     
    where 
      op = 
        (do symbol "multiplied by"; return Multiplied_by)
        <|> (do symbol "divided by"; return Divided_by)
        <|> do symbol "to the power of"; return Power

{--stringExp :: Parser StringExp
stringExp = 
  (StringID <$>
   String)
  do
    symbol "+"
    return Concatenation
--}


-- !!! ARRAY DA VEDERE STACK
arFactor :: Parser ArExp
arFactor = do
    (Constant <$> integer)      
        <|> do
            i <- identifier     
            do
                symbol "["      
                n <- arExp  
                symbol "]"
                return (StackId i n)
                <|> return (ArId i) 
        <|> do
            symbol "("                   
            a <- arExp 
            symbol ")"
            return a

boolExp :: Parser BoolExp
boolExp = chain boolTerm op
  where op = do
            symbol "OR"
            return OR

boolTerm :: Parser BoolExp
boolTerm = chain boolFactor op
  where op = do
            symbol "AND"
            return AND
    
boolFactor :: Parser BoolExp
boolFactor =
  do
    symbol "True"
    return (Bool True)
    <|> do
      symbol "False"
      return (Bool False)
      <|> do
        symbol "Not"
        NOT <$> boolExp
        <|> do
          symbol "("
          b <- boolExp
          symbol ")"
          return b
    <|> do 
        a1 <- arExp 
        do
            symbol "is less than"
            a2 <- arExp 
            return (LessThan a1 a2)
            <|> do
              symbol "is greater than"
              a2 <- arExp 
              return (GreaterThan a1 a2)
            <|> do
              symbol "is less or equal to"
              a2 <- arExp 
              return (LessEqualThan a1 a2)
            <|> do
              symbol "is greater or equal to"
              a2 <- arExp 
              return (GreaterEqualThan a1 a2)
            <|> do
              symbol "is equal to"
              a2 <- arExp 
              return (EqualTo a1 a2)
            <|> do
              symbol "is not equal to"
              a2 <- arExp 
              return (NotEqualTo a1 a2)
    <|> (BoolId <$> identifier) 

-- Commands declared in the Grammar.hs file
command :: Parser Command
command =
    arDeclaration
    <|> boolDeclaration 
    <|> arDeclaration 
    <|> arAssignment
    <|> boolAssignment
    -- <|> stackAssignment
    <|> stackDeclaration
    <|> ifElse
    <|> whiledo
    <|> skip 

program :: Parser [Command]         
program = 
  do many command 

arDeclaration :: Parser Command
arDeclaration = 
  do
    symbol "int"             
    i <- identifier
    symbol "="
    r <- ArDeclaration i <$> arExp      
    return r

boolDeclaration :: Parser Command
boolDeclaration =
  do
    symbol "bool"             -- bool id=True;
    i <- identifier
    symbol "="
    r <- BoolDeclaration i <$> boolExp
    return r

stackDeclaration  :: Parser Command
stackDeclaration  =
  do
    symbol "stack"       
    i <- identifier
    symbol "["
    j <- arExp 
    symbol "]"
    return (StackDeclaration i j)  
      

arAssignment :: Parser Command
arAssignment =
  do
    i <- identifier
    symbol "="
    r <- ArAssignment i <$> arExp 
    return r
    
boolAssignment  :: Parser Command
boolAssignment  =
  do
    i <- identifier
    symbol "="
    r <- BoolAssignment  i <$> boolExp
    return r

push :: Parser Command
push =
  do 
    symbol "push"
    symbol "("
    i <- identifier
    symbol ","
    a <- arExp
    symbol ")"
    return (Push i a)

pop :: Parser Command
pop =
  do 
    symbol "pop"
    symbol"("
    i <- identifier
    symbol ")"
    return (Pop i)
    
{--
stackAssignment  :: Parser Command
stackAssignment  =
  do
    i <- identifier  
    do           
      symbol "["
      j <- arExp 
      symbol "]"
      symbol "="
      r <- StackAssignment  i j <$> arExp 

      return r
      <|>
        do 
          symbol "="
          symbol "["
          j <- arExp
          k <- many (do symbol ","; arExp)
          symbol "]"
    
          return (arrFullAssign i (stackId (j:k)))
      <|>
        do 
          symbol "["
          symbol "]"
          symbol "="
          x <- identifier
          symbol "["
          symbol "]"
    
          return (arrFullAssign i (stackId x)) 
--}

skip  :: Parser Command
skip  =
  do
    symbol "skip"
    return Skip 

ifElse :: Parser Command
ifElse =
  do
    symbol "if"
    symbol "("
    b <- boolExp
    symbol ")"
    symbol "{"
    thenP <- program
    symbol "}"
    do
      symbol "else"
      symbol "{"
      elseP <- program
      symbol "}"
      return (IfElse b thenP elseP)
      <|> do
        return (IfElse b thenP [Skip])

whiledo :: Parser Command
whiledo =
  do
    symbol "do{"
    p <- program
    symbol "}"
    symbol "while("
    b <- boolExp
    symbol ")"

    return (Whiledo b p)



parse :: String -> ([Command], String)
parse s = case p s of
  [] -> ([], "")
  [(c, s)] -> (c, s)
  where
    (P p) = program


parseFailed :: ([Command], String) -> Bool
parseFailed (_, "") = False
parseFailed (_, _) = True

getParsedCommands :: ([Command], String) -> [Command]
getParsedCommands (c, _) = c

getRemainingInput :: ([Command], String) -> String
getRemainingInput (_, s) = s