module Stack where

-- type Stack = [Int]

readAll :: (Show a) => [a] -> String
readAll []     = ""
readAll (x:xs) = if null xs then show x else show x ++ "->" ++ readAll xs

{-- 
instance (Show a) => Show (Stack a)
 where
 show (Stack l) = readAll l

push :: -> Stack -> Stack a
push x (Stack s) = Stack (x:s)

pop :: Stack -> Maybe (Stack a, a)
pop 

emptyStack :: Stack a
emptyStack = Stack []

push :: -> [a] -> ((),[a])  -- return tuple containing 'nothing' and new stack
push elem stack = ((), (:) elem stack)

pop :: [a] -> (a, [a])  -- return tuple containing the popped element and the new stack
pop [] = error "Can't pop from an empty stack!"
pop ((:) x stack) = (x, stack)

readLast :: Stack -> Int
readLast [] = error "Empty array!"
readLast stack  = stack head

--}

type Stack = [Int]

emptyStack :: Int -> Stack
emptyStack a = []

isEmptyStack :: Stack -> Bool
isEmptyStack = null

push :: Stack -> Int -> Stack
push xs item = item : xs

top :: Stack -> Int
top = head

pop :: Stack -> Stack
pop = tail