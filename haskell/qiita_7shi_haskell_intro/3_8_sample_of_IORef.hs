import Data.IORef

-- Firstly, initialize 'c', then
-- returns function so that
-- every time we execute 'f'
-- 'c' won't be initialized.
counter = do
    c <- newIORef 0
    return $ do
        c' <- readIORef c
        writeIORef c $ c' + 1
        readIORef c


main = do
  f <- counter
  -- Use 'print' instad of 'putStrLn' because
  -- 'putStrLn' accepts only String
  print =<< f
  print =<< f
  print =<< f
