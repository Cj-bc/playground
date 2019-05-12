main = do
        formula <- getLine
        print $ calcRevPolish formula


calcRevPolish :: (Num a, Read a) => String -> a
calcRevPolish = head . foldl foldingFunc [] . words
        where
                foldingFunc (x:y:ys) "+" = (x + y):ys
                foldingFunc (x:y:ys) "-" = (y - x):ys
                foldingFunc (x:y:ys) "*" = (x * y):ys
                foldingFunc xs num       = (read num):xs
