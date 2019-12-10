module TicTacToe where

data Mark = O | X deriving (Show, Eq) -- ^ Mark, O or X
data Board = Board [Maybe Mark]  -- ^ Actual game board
type Coord = (Int, Int) -- ^ Coord is from (0, 0) to (8, 8). (Horizontal, Vertical)

initializeBoard :: Board -- ^ Initial state for Board
initializeBoard = Board $ replicate 9 Nothing

coordToIndex :: Coord -> Int
coordToIndex (h, v) = h + 3 * v


putMark :: Mark -> Board -> Int -> Board
putMark m (Board b) idx = if (b !! idx == Nothing)
                        then Board $ (take idx b) ++ [Just m] ++ (drop (idx+1) b)
                        else Board b


-- | True if game is done
-- This code is very dirty. Should be reimplement with smarter way
isFinished :: Board -> Bool
isFinished (Board b) = let horizontal = [take 3 b, drop 3 (take 6 b), drop 6 b]
                           vertical   = [ [b !! 0, b !! 3, b !! 6]
                                        , [b !! 1, b !! 4, b !! 7]
                                        , [b !! 2, b !! 5, b !! 8]]
                           diagonal   = [ [b !! 0, b !! 4, b !! 8]
                                        , [b !! 2, b !! 4, b !! 6]]
                           allLines   = horizontal ++ vertical ++ diagonal
                           areSame (x:[]) = True
                           areSame (x:xs) = x == head xs && areSame xs
                           hasLine xs = if Nothing `elem` xs
                                        then False else areSame xs
                           result     = fmap hasLine allLines
                        in True `elem` result

