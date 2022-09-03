module Stack where

-- type Stack = [Int]

readAll :: (Show a) => [a] -> String
readAll []     = ""
readAll (x:xs) = if null xs then show x else show x ++ "->" ++ readAll xs

{-- 
instance (Show a) => Show (Stack a)
 where
 show (Stack l) = readAll l

push :: a -> Stack a -> Stack a
push x (Stack s) = Stack (x:s)

pop :: Stack a -> Maybe (Stack a, a)
pop 

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> [a] -> ((),[a])  -- return a tuple containing a 'nothing' and a new stack
push elem stack = ((), (:) elem stack)

pop :: [a] -> (a, [a])  -- return a tuple containing the popped element and the new stack
pop [] = error "Can't pop from an empty stack!"
pop ((:) x stack) = (x, stack)

readLast :: Stack -> Int
readLast [] = error "Empty Array!"
readLast stack  = stack head

--}

type Stack a = [a]

emptyStack :: Stack a
emptyStack = []

isEmptyStack :: Stack a -> Bool
isEmptyStack = null

push :: a -> Stack a -> Stack a
push = (:)

top :: Stack a -> a
top = head

pop :: Stack a -> Stack a
pop = tail