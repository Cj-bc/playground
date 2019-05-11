module Die
( die
, dies
) where

import System.Random

die :: RandomGen g => g -> Int
die gen = fst $ randomR (1, 6) gen


dies :: RandomGen g => g -> [Int]
dies gen = randomRs (1, 6) gen
