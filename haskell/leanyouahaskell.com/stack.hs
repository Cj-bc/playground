newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    -- prop> fmap id s == s
    -- prop> fmap (f . g) s == fmap f (fmap g s)
    -- fmap (f . g) c = fmap ((\a -> b) . (\c -> a)) c
    --                = fmap (\c -> b) c
    --                = b
    --
    -- fmap f (fmap g c) = fmap (\a -> b) (fmap (\c -> a) c)
    --                   = fmap (\a -> b) a
    --                   = b
    --
    -- fmap f (fmap g (State c)) = fmap f (fmap g (State (\s -> (c, s'))))
    --                           = fmap f (State \s -> (g c, s'))
    --                           = State \s -> (f . g c, s')
    --
    -- fmap (f . g) (State c) = fmap (f . g) (State (\s -> (c, s')))
    --                        = State \s -> (f . g c, s')
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
