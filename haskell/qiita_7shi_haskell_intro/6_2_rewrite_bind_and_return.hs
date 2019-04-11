{-# LANGUAGE UnboxedTuples #-}
import GHC.Base

return' a = IO $ \s -> (# s, a #)
-- `IO` should take (# s, a #) as argument

bind :: IO a -> (a -> IO b) -> IO b
bind prevM f = IO $ \s ->
  let (# s1, prev #)  = unIO prevM s
      (# s2, x #)     = unIO (f prev) s1
  in (# s2, x #)

main :: IO ()
main = return' "hello" `bind` putStr `bind` print
