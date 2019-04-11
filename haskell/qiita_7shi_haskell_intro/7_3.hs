import Control.Monad

main = do
  print $ join $ join $
    forM [1..3] $ \x ->
      forM "abc" $ \y ->
        return (x, y)
