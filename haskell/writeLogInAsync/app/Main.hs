{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Control.Monad.Writer 
import Control.Exception (mask, finally, catch, SomeException)
main = do
  a <- async $ mask $ \unmask -> do
        (_, logged) <- runWriterT $ do
          tell "Hello first log.\n"
          liftIO . void . unmask $ (threadDelay 1000000) `catch` (\(e :: SomeException) -> pure ())
          tell "Log after long process\n"

        putStrLn logged

  threadDelay 5000
  cancel a
