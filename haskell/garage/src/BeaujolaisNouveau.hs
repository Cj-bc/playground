module BeaujolaisNouveau where

import Control.Arrow
import System.Random
import Control.Monad

main :: IO ()
main = do
    let beaujolaisNouveau = [ ["ボ", ""]
                            , ["ジョ", ""]
                            , ["レ", "ー"]
                            , ["ヌ", "ー"]
                            , ["ボ", "ー"]
                            ]
    res <- (liftM concat $ liftM concat $ shuffle1st beaujolaisNouveau)
    putStrLn res

shuffle1st :: [[a]] -> IO [[a]]
shuffle1st xs = do
    xs' <- shuffle $ fstL xs
    return $ zipWith (\x y -> [x, y]) xs' (sndL xs)


shuffle :: [a] -> IO [a]
shuffle (x:[])= return [x]
shuffle xs = do
        pickedIndex <- randomRIO (0, (length xs) - 1)
        let picked = xs !! pickedIndex

        rest <- shuffle $ restOf (pickedIndex + 1) xs
        return $ picked : rest

restOf :: Int -> [a] -> [a]
restOf _ (x:[]) = []
restOf n xs     = take (n - 1) xs ++ (drop n xs)

fstL :: [[a]] -> [a]
fstL []     = []
fstL (x:xs) = head x : fstL xs

sndL :: [[a]] -> [a]
sndL []     = []
sndL (x:xs) = head (tail x) : sndL xs
