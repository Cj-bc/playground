import System.Random

-- mine {{{
-- replicateM' :: Int -> m a -> m [a]
-- replicateM' 1 ac = do
--   x <- ac
--   return [x]
-- replicateM' n ac = do
--   x <- ac
--   rest <- replicateM' (n - 1) ac
--   return $ [x] ++ [rest]
--   }}}
replicateM' 0 _         = return []
replicateM' n a | n > 0 = (:) <$> a <*> replicateM' (n - 1) a
-- base pattern is '0', where I declared '1'
-- <$> extract the return of action, then pass it to the previous function.
-- <*> is the same but used when multiple <$> is needed.

-- mine {{{
-- replicateM_' 1 ac = ac >> return ()
-- replicateM_' n ac = ac >> replicateM_' (n - 1) ac
-- }}}
replicateM_' 0 ac = return ()
replicateM_' n ac = ac >> replicateM_' (n - 1) ac
-- This is almost similar to mine
-- Use '0' as base pattern is better.

-- mine {{{
-- forM' [] fn     = return []
-- forM' (x:xs) fn = return $ (++) [fn x] <$> forM' xs fn
-- }}}
forM' [] _     = return []
forM' (x:xs) f = (:) <$> f x <*> forM' xs f
-- no need of 'return' in second pattern.

-- mine {{{
-- forM_' [] fn      = return ()
-- forM_' (x:xs) fn  = fn x >> forM_' xs fn
-- }}}
forM_' [] fn  = return []
forM_' (x:xs) fn = fn x >> forM_' xs fn
-- Returns empty list instead of ()


when' expr ac = if expr then ac else return ()

-- unless' expr ac = if expr then return () else ac
unless' expr = when' $ not expr

main :: IO ()
main = do
  let dice = getStdRandom $ randomR (1, 6) :: IO Int
--  print =<< replicateM' 5 dice
--  putStrLn "---"
  replicateM_' 3 $ do
    print =<< dice
  putStrLn "---"
  a <- forM' [1..3] $ \i -> do
    print i
    return i
  print a
  putStrLn "---"
  forM_' [1..3] $ \i -> do
    print i
  putStrLn "---"
  let y f = f ( y f)
  y $ \f -> do
    r <- dice
    print r
    when' (r /= 1) f
  putStrLn "---"
  y $ \f -> do
    r <- dice
    print r
    unless' (r == 6) f
