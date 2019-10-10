import Control.Monad hiding (guard)
import Control.Monad.Writer hiding (guard)


type KnightPos = (Int, Int)
type Route = [KnightPos]

-- | Just re-implementation of guard
guard :: MonadPlus m => Bool -> m ()
guard True  = return ()
guard False = mzero

-- | Return list of 'KnightPos' knight can move to from given 'KnightPos'
--
-- This is written in book p.307
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+1, r+2), (c+1, r-2), (c-1, r+2), (c-1, r-2)
                ,(c+2, r+1), (c+2, r-1), (c-2, r+1), (c-2, r-1)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

-- | Return True if knight can move from s to e in 3 turn
--
-- This is written in book p.308
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 s e = e `elem` (moveKnight s >>= moveKnight >>= moveKnight)


-- | Return list of route to reach e from s.
--
-- [] if it's impossible
routeIn3 :: KnightPos -> KnightPos -> [Route]
routeIn3 s e = do
    first <- moveKnight s
    second <- moveKnight first
    third <- moveKnight second
    guard (e == third)
    return [first, second, third]

-- | upgraded 'canReachIn3' which also returns 'Route's as Monoid value
canReachIn3Writer :: KnightPos -> KnightPos -> Writer [Route] Bool
canReachIn3Writer s e = writer (flag, possibility)
    where
      possibility = routeIn3 s e
      flag = possibility /= []
