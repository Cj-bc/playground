import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Control.Monad.Writer 
import Control.Exception (mask, finally, catch)
main = do
  a <- async $ mask $ \unmask -> (do
        (_, logged) <- runWriterT $ do
          tell "Hello first log.\n"
          liftIO . void . unmask $ threadDelay 100000
          tell "Log after long process\n"

        putStrLn logged) `catch` (return . const ())

  threadDelay 5000
  cancel a
