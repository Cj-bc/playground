import Data.IORef

counter :: IORef Int -> IO Int
counter n = do
  n' <- (+ 1) <$> readIORef n
  writeIORef n n'
  return n'

main = do
  a <- newIORef 0
  let f = counter a
  putStrLn =<< show <$> f
  putStrLn =<< show <$> f
  putStrLn =<< show <$> f
