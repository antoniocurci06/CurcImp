-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022

module Parser where

import Grammar

newtype Parser a = P (String -> [(a,String)]) 


instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser b 
 fmap g p = P (\inp -> case parse p inp of
    [] -> []
    [(v,out)] -> [(g v, out)])

instance Applicative Parser where 
-- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])
    <*> :: Parser (a -> b) -> Parser a -> Parser b pg <*> px = P (\inp -> case parse pg inp of
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

class Applicative f => Alternative f where 
    empty :: f a
    (<|>) :: f a -> f a -> f a
    many :: f a -> f [a]
    some :: f a -> f [a]
    many x = some x <|> pure []
    some x = pure (:) <*> x <*> many x

instance Alternative Parser where 
    -- empty :: Parser a
    empty = P (\inp -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a p <|> q = P (\inp -> case parse p inp of
        [] -> parse q inp 
        [(v,out)] -> [(v,out)])


-- Derived Primitives, 13.6 from the Haskell Book

item :: Parser Char  
item =
    P (\input -> case input of 
        [] -> []
        (x : xs) -> [(x, xs)]) 

-- General Operator
sat :: (Char -> Bool) -> Parser 
Char sat p = do x <- item
if p x then return x else empty

-- Digits handling
digit :: [Char]
digit = ['0' .. '9']

digit :: Parser Char 
digit = sat isDigit

isDigit :: Char -> Bool
isDigit c = c `elem` digits


-- Chars handling
lower :: [Char]
lower = ['a' .. 'z']

lower :: Parser Char 
lower = sat isLower

isLower :: Char -> Bool
isLower c = c `elem` lower

upper :: [Char] 
upper = ['A' .. 'Z']

upper :: Parser Char 
upper = sat isUpper

isUpper :: Char -> Bool
isUpper c = c `elem` upper

letter :: Parser Char 
letter = sat isAlpha

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
        char x string xs 
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
int = do char ’-’
    n <- nat
    return (-n) 
    <|> nat

-- Handling Spacing
space :: [Char]
space = ['\n', '\t', '\r', ' ']

isSpace :: Char -> Bool
isSpace x = elem ´x´ space

token :: Parser a -> Parser a 
token p = do space
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
nats = do symbol "["
    n <- natural
    ns <- many (do symbol "," natural) 
    symbol "]"
    return (n:ns)

-- Arithmetic Expressions
aExp  :: Parser ArExp         
aExp = do chain aTerm op
    where                        
      op = 
          (do symbol "+"; return sum)
          <|> do symbol "-"; return difference 

aTerm :: Parser ArExp       
aTerm = do chain aFactor op     
    where 
      op = 
        (do symbol "*"; return multiplied_by)
        <|> (do symbol "/"; return divided_by)
        <|> do symbol "^"; return power

-- !!! ARRAY DA VEDERE STACK
arFactor :: Parser ArExp
arFactor = do
    (Constant <$> integer)      
        <|> do
            i <- identifier     
            do
                symbol "["      
                n <- aExp  
                symbol "]"
                return (ArrVariable i n)
                <|> return (ArithVariable i) 
        <|> do
            symbol "("                   
            a <- aExp 
            symbol ")"
            return a

boolFactor :: Parser BoolExpr
boolFactor =
  do
    symbol "True"
    return (Boolean True)
    <|> do
      symbol "False"
      return (Boolean False)
    <|> do
      symbol "Not"
      Not <$> bExp
    <|> do
      symbol "("
      b <- bExp
      symbol ")"
      return b
    <|> do 
        a1 <- aExp 
        do
            symbol "<"
            a2 <- aExp 
            return (lessThan a1 a2)
            <|> do
              symbol ">"
              a2 <- aExp 
              return (greaterThen a1 a2)
            <|> do
              symbol "<="
              a2 <- aExp 
              return (lessEqualThan a1 a2)
            <|> do
              symbol ">="
              a2 <- aExp 
              return (greaterEqualThan a1 a2)
            <|> do
              symbol "=="
              a2 <- aExp 
              return (equalTo a1 a2)
            <|> do
              symbol "!="
              a2 <- aExp 
              return (notEqualTo a1 a2)
    <|> (boolId <$> identifier) 
