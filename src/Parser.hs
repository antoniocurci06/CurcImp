module Parser where
import Grammar

-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022


-- Parser declaration
newtype Parser a = P (String -> [(a,String)]) 

instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser b 
fmap g p = P (\inp -> case parse p inp of
  [] -> []
  [(v,out)] -> [(g v, out)])

instance Applicative Parser where 
-- pure :: a -> Parser a
pure v = P (\inp -> [(v,inp)])
-- <*> :: Parser (a -> b) -> Parser a -> Parser b 
pg <*> px = P (\inp -> case parse pg inp of
          [] -> []
          [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b 
p >>= f = P (\inp -> case parse p inp of
  [] -> []
  [(v,out)] -> parse (f v) out)


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

--class Applicative f => Alternative f where 
--  empty :: f a
  --(<|>) :: f a -> f a -> f a
  --many :: f a -> f [a]
  --some :: f a -> f [a]

  --many x = some x <|> pure []
--  some x = pure (:) <*> x <*> many x

instance Alternative Parser where 
    -- empty :: Parser a
    empty = P (\inp -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a 
    p <|> q = P (\inp -> case parse p inp of
        [] -> parse q inp 
        [(v,out)] -> [(v,out)])


-- Derived Primitives, 13.6 from the Haskell Book

item :: Parser Char  
item =
    P (\input -> case input of 
        [] -> []
        (x : xs) -> [(x, xs)]) 

-- General Operator
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
isDigit c = c `elem` digits


-- Chars handling
lowerSet :: [Char]
lowerSet = ['a' .. 'z']

lower :: Parser Char 
lower = sat isLower

isLower :: Char -> Bool
isLower c = c `elem` lower

upperSet :: [Char] 
upperSet = ['A' .. 'Z']

upper :: Parser Char 
upper = sat isUpper

isUpper :: Char -> Bool
isUpper c = c `elem` upper

letterSet :: Parser Char 
letterSet = sat isAlpha

isAlpha :: Char -> Bool
isAlpha = c `elem` upper || c `elem` lower

alphanum :: Parser Char 
alphanum = sat isAlphaNum

isAlphanum :: Char -> Bool
isAlphanum = c `elem` upper || c `elem` lower || c `elem` digits

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
    xs <- many alphanum return (x:xs)

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
space :: [Char]
space = ['\n', '\t', '\r', ' ']

isSpace :: Char -> Bool
isSpace x = elem ´x´ space

token :: Parser a -> Parser a 
token p = do 
    space
    v <- p space 
    return v

identifier :: Parser String 
identifier = token ident

natural :: Parser Int 
natural = token nat

integer :: Parser Int 
integer = token int

symbol :: String -> Parser String 
symbol xs = token (string xs)

nats :: Parser [Int] 
nats = do 
    symbol "["
    n <- natural
    ns <- many (do symbol "," natural) 
    symbol "]"
    return (n:ns)

-- Arithmetic Expressions
arExp  :: Parser ArExp         
arExp = do chain arTerm op
    where                        
      op = 
          (do symbol "+"; return Sum)
          <|> do symbol "-"; return Difference 

arTerm :: Parser ArExp       
arTerm = do chain arFactor op     
    where 
      op = 
        (do symbol "*"; return Multiplied_by)
        <|> (do symbol "/"; return Divided_by)
        <|> do symbol "^"; return Power

stringExp :: Parser StringExp
stringExp = 
  do
    symbol "+"; return Concatenation


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
                return (Stack i n)
                <|> return (ArId i) 
        <|> do
            symbol "("                   
            a <- arExp 
            symbol ")"
            return a

boolFactor :: Parser BoolExpr
boolFactor =
  do
    symbol "True"
    return (Bool True)
    <|> do
      symbol "False"
      return (Bool False)
    <|> do
      symbol "Not"
      Not <$> bExp
    <|> do
      symbol "("
      b <- bExp
      symbol ")"
      return b
    <|> do 
        a1 <- arExp 
        do
            symbol "<"
            a2 <- arExp 
            return (LessThan a1 a2)
            <|> do
              symbol ">"
              a2 <- arExp 
              return (GreaterThan a1 a2)
            <|> do
              symbol "<="
              a2 <- arExp 
              return (LessEqualThan a1 a2)
            <|> do
              symbol ">="
              a2 <- arExp 
              return (GreaterEqualThan a1 a2)
            <|> do
              symbol "=="
              a2 <- arExp 
              return (EqualTo a1 a2)
            <|> do
              symbol "!="
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
    <|> BoolAssignmentment 
    <|> stackAssignment
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
    r <- arDeclaration i <$> arExp      
    return r

boolDeclaration :: Parser Command
boolDeclaration =
  do
    symbol "bool"             -- bool id=True;
    i <- identifier
    symbol "="
    r <- boolDeclaration i <$> bExp
    return r

stackDeclaration  :: Parser Command
stackDeclaration  =
  do
    symbol "stack"       
    i <- identifier
    symbol "["
    j <- arExp 
    symbol "]"
    return (stackDeclaration i j)  
      

arAssignment :: Parser Command
arAssignment =
  do
    i <- identifier
    symbol "="
    r <- arAssignment i <$> arExp 
    return r
    
boolAssignment  :: Parser Command
boolAssignment  =
  do
    i <- identifier
    symbol "="
    r <- boolAssignment  i <$> bExp
    return r

stackAssignment  :: Parser Command
stackAssignment  =
  do
    i <- identifier  
    do           
      symbol "["
      j <- arExp 
      symbol "]"
      symbol "="
      r <- stackAssignment  i j <$> arExp 

      return r
      <|>
        do 
          symbol "="
          symbol "["
          j <- arExp
          k <- many (do symbol ","; arExp)
          symbol "]"
    
          return (ArrFullAssign i (stackId (j:k)))
      <|>
        do 
          symbol "["
          symbol "]"
          symbol "="
          x <- identifier
          symbol "["
          symbol "]"
    
          return (ArrFullAssign i (stackId x)) 

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
    b <- bExp
    symbol ")"
    symbol "{"
    thenP <- program
    symbol "}"
    do
      symbol "else"
      symbol "{"
      elseP <- program
      symbol "}"
      return (ifElse b thenP elseP)
      <|> do
        return (ifElse b thenP [Skip])

whiledo :: Parser Command
whiledo =
  do
    symbol "do{"
    p <- program
    symbol "}"
    symbol "while("
    b <- bExp
    symbol ")"

    return (whiledo b p)

push :: Parser Command
push =
  do 
    symbol "push("
    i <- identifier
    symbol ","
    a <- arExp
    symbol ")"
    return (push i a)

pop :: Parser Command
pop =
  do 
    symbol "pop("
    i <- identifier
    symbol ")"
    return (pop i)


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


