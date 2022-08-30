module Stack where
import Grammar

data Stack a = Stack [a]
               deriving (Eq,Ord)

readAll :: (Show a) => [a] -> String
readAll []     = ""
readAll (x:xs) = if null xs then show x else show x ++ "->" ++ readAll xs

instance (Show a) => Show (Stack a)
 where
 show (Stack l) = readAll l

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack s) = Stack (x:s)

stackPop :: Stack a -> Maybe (Stack a, a)

