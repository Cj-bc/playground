import Control.Monad hiding (guard)


type KnightPos = (Int, Int)

guard :: MonadPlus m => Bool -> m ()
guard True  = return ()
guard False = mzero

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+1, r+2), (c+1, r-2), (c-1, r+2), (c-1, r-2)]
    guard (c' `elem` [0..8] && r' `elem` [0..8])
    return (c', r')
