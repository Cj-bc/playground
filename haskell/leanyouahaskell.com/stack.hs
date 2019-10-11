newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    -- prop> fmap id s == s
    fmap h (State f) = State $ \s ->
                            let (a, newState) = f s
                                g = h a
                            in (g, newState)

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    State h <*> State f = State $ \s ->
                            let g = h f
                            in ()


instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s ->
                        let (a, newState) = h s
                            (State g) = f a
                        in (g, newState)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push n xs = ((), n:xs)

stackMainLp :: Stack -> (Int, Stack)
stackMainLp st = let
    (_, st') = push 3 st
    (n, st'') = pop st'
    in pop st''
