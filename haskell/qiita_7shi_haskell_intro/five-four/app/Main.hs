import System.Random
import Control.Monad

-- mine {{{
-- round' x = if (x - truncated) > 0.5 then truncated else truncated + 1
--   where
--     truncated = fromIntegral $ truncate x
--     }}}
round' x = truncate (x + 0.5)
-- Nicer way to define round.

main :: IO ()
main = do
  xs <- forM [1..100] $ \i ->
              sum <$> replicateM 12 (getStdRandom (randomR (0.0, 1.0)) :: IO Double)
  let xs' = flip map xs $ \x -> (round' x) - 6

--  mine {{{
--  forM_ [-3..3] $ \x -> do
--        putStr $ reverse (drop 2 (reverse (show x))) ++ ": "
--        replicateM (length [n | n <- xs', n == x]) $ putStr "*"
--        putStrLn " "
--        }}}
  forM_ [-3..3] $ \x -> do
      let c = length $ filter (== x) xs'
      let x' = show x
      let n = replicate (2 - length x') ' ' ++ x'
      putStrLn $ n ++ ": " ++ replicate c '*'
-- Specify padding by using replicate (n)
