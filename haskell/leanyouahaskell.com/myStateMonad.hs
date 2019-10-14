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
    -- prop: pure f <*> x = fmap f x {{{
    -- prop> pure f <*> x = fmap f x
    --
    --   pure f <*> (State \s -> (a, s))
    --         = (State \s -> (f, s)) <*> (State \s -> (a, s))
    --         = State $ \s ->
    --               let (f', newState) = runState (State \s -> (f, s)) s
    --                   (h', newState') = runState (State \s -> (a, s)) newState
    --               in (f' h', newState')
    --         = State $ \s ->
    --               let (f', newState) = (f, s)
    --                   (h', newState') = (a, newState)
    --               in (f' h', newState')
    --         = State $ \s ->
    --               let (f, s) = (f, s)
    --                   (h', newState) = (a, s)
    --               in (f h', newState')
    --         = State $ \s -> (f a, s)
    --
    --   fmap f (State \s -> (a, s))
    --          = State $ \s ->
    --                let (a, newState) = (\s -> (a, s)) s
    --                    g = f a
    --                in (g, newState)
    --          = State $ \s ->
    --                let (a, newState) = (a, s)
    --                    g = f a
    --                in (g, newState)
    --          = State $ \s -> (f a, s)
    -- }}}
    --
    -- prop: pure id <*> v = v {{{
    -- prop> pure id <*> v = v
    --
    --   pure id <*> (State $ \s -> (a, s))
    --      = (State $ \s -> (id, s)) <*> (State $ \s -> (a, s))
    --      = State $ \s ->
    --          let (f', newState)  = runState (State $ \s -> (id, s)) s
    --              (h', newState') = runState (State $ \s -> (a, s)) newState
    --          in (f' h', newState')
    --      = State $ \s ->
    --          let (f', newState)  = (\s -> (id, s)) s
    --              (h', newState') = (\s -> (a, s)) newState
    --          in (f' h', newState')
    --      = State $ \s ->
    --          let (f', newState)  = (id, s)
    --              (h', newState') = (a, newState)
    --          in (f' h', newState')
    --      = State $ \s ->
    --          let (f', newState)  = (id, s)
    --              (h', newState') = (a, s)
    --          in (id h', newState')
    --      = State $ \s -> (id a, s)
    --      = State $ \s -> (a, s)
    -- }}}
    --
    -- prop: pure (.) <*> u <*> v <*> w = u <*> (v <*> w) {{{
    -- prop> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    --
    --  pure (.) <*> (State (\s -> (a, s))) <*> (State (\s -> (b, s))) <*> (State (\s -> (c, s)))
    --      = (pure (.) <*> (State (\s -> (a, s))))
    --          <*> (State (\s -> (b, s)))
    --          <*> (State (\s -> (c, s)))
    --      = (fmap (.) (State (\s -> (a, s))))
    --          <*> (State (\s -> (b, s)))
    --          <*> (State (\s -> (c, s)))
    --      = (State (\s ->
    --              let (a, newState) = (\s -> (a, s)) s
    --                  g = (.) a
    --              in (g, newState)))
    --          <*> (State (\s -> (b, s)))
    --          <*> (State (\s -> (c, s)))
    --      = (State (\s -> ((.) a, s)))
    --          <*> (State (\s -> (b, s)))
    --          <*> (State (\s -> (c, s)))
    --      = (State (\s ->
    --              let (f', newState)  = runState (State (\s -> ((.) a, s))) s
    --                  (h', newState') = runState (State (\s -> (b, s))) newState
    --              in (f' h', newState)))
    --          <*> (State (\s -> (c, s)))
    --      = (State (\s ->
    --              let (f', newState)  = (\s -> ((.) a, s)) s
    --                  (h', newState') = (\s -> (b, s)) newState
    --              in (f' h', newState)))
    --          <*> (State (\s -> (c, s)))
    --      = (State (\s ->
    --              let (f', newState)  = ((.) a, s)
    --                  (h', newState') = (b, newState)
    --              in (f' h', newState)))
    --          <*> (State (\s -> (c, s)))
    --      = (State (\s -> ((.) a b, s)))
    --          <*> (State (\s -> (c, s)))
    --      = (State (\s -> (a . b, s)))
    --          <*> (State (\s -> (c, s)))
    --      = State $ \s ->
    --          let (f', newState)  = runState (State (\s -> (a . b, s))) s
    --              (h', newState') = runState (State (\s -> (c, s))) newState
    --          in (f' h', newState')
    --      = State $ \s ->
    --          let (f', newState)  = (a . b, s)
    --              (h', newState') = (c, newState)
    --          in (f' h', newState')
    --      = State $ \s -> ((a . b) c, s)
    --
    --
    --  (State (\s -> (a, s))) <*> ((State (\s -> (b, s))) <*> (State (\s -> (c, s))))
    --      = (State (\s -> (a, s))) <*>
    --          (State (\s ->
    --                  let (f', newState)  = runState (State (\s -> (b, s))) s
    --                      (h', newState') = runState (State (\s -> (c, s))) newState
    --                  in (f' h', newState')))
    --      = (State (\s -> (a, s))) <*>
    --          (State (\s ->
    --                  let (f', newState)  = (b, s)
    --                      (h', newState') = (c, newState)
    --                  in (f' h', newState')))
    --      = (State (\s -> (a, s))) <*>
    --          (State (\s -> (b c, s)))
    --      = State $ \s ->
    --          let (f', newState)  = runState (State (\s -> (a, s))) s
    --              (h', newState') = runState (State (\s -> (b c, s))) newState
    --          in (f' h', newState')
    --      = State $ \s ->
    --          let (f', newState)  = (a, s)
    --              (h', newState') = (b c, newState)
    --          in (f' h', newState')
    --      = State $ \s -> (a (b c), s)
    --      = State $ \s -> ((a . b) c, s)
    --
    -- }}}
    --
    -- prop: pure f <*> pure x = pure (f x) {{{
    -- prop> pure f <*> pure x = pure (f x)
    --
    --  pure f <*> pure x
    --      = pure f <*> (State (\s -> (x, s)))
    --      = fmap f (State (\s -> (x, s)))
    --      = State $ \s ->
    --          let (a, newState) = runState (State (\s -> (x, s))) s
    --              g = f a
    --          in (g, newState)
    --      = State $ \s ->
    --          let (a, newState) = (x, s)
    --              g = f a
    --          in (g, newState)
    --      = State $ \s -> (f x, s)
    --
    --  pure (f x) = State $ \s -> (f x, s)
    --
    -- }}}
    --
    -- prop: u <*> pure y = pure ($ y) <*> u {{{
    -- prop> u <*> pure y = pure ($ y) <*> u
    --
    --  (State (\s -> (a, s))) <*> pure y
    --      = (State (\s -> (a, s))) <*> (State (\s -> (y, s)))
    --      = State $ \s ->
    --          let (f', newState)  = runState (State (\s -> (a, s))) s
    --              (h', newState') = runState (State (\s -> (y, s))) newState
    --          in (f' h', newState')
    --      = State $ \s ->
    --          let (f', newState)  = (a, s)
    --              (h', newState') = (y, newState)
    --          in (f' h', newState')
    --      = State $ \s -> (a y, s)
    --
    --
    --  pure ($ y) <*> (State (\s -> (a, s)))
    --      = fmap ($ y) (State (\s -> (a, s)))
    --      = State $ \s ->
    --          let (n, newState) = (\s -> (a, s)) s
    --              g = ($ y) n
    --          in (g, newState)
    --      = State $ \s ->
    --          let (n, newState) = (a, s)
    --              g = n y
    --          in (g, newState)
    --      = State $ \s -> (a y, s)
    --
    -- }}}
    --
    pure a = State $ \s -> (a, s)
    f <*> h = State $ \s ->
                let (f', newState) = runState f s
                    (h', newState') = runState h newState
                in (f' h', newState')

instance Monad (State s) where
    return = pure
    h >>= f = State $ \s ->
                let (g, newState) = runState h s
                in runState (f g) $ newState

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
