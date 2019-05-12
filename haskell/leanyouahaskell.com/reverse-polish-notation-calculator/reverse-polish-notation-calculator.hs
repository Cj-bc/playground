main = do
        formula <- getLine
        putStrLn $ show $ foldl calcRevPolish [] $ words formula


calcRevPolish :: [Int] -> Char -> Maybe [Int]
calcRevPolish stack '+' | length stack > 2 = let stacklen  = length stack
                                                 (fst:sec:_) = drop (stacklen - 2) stack
                                                 newStack  = take (stacklen - 2) stack
                                             in Just $ stack ++ [(fst + sec)]
                        | otherwise        = Nothing
calcRevPolish stack '-' | length stack > 2 = let stacklen  = length stack
                                                 (fst:sec:_) = drop (stacklen - 2) stack
                                                 newStack  = take (stacklen - 2) stack
                                             in Just $ stack ++ [(fst - sec)]
                        | otherwise        = Nothing
calcRevPolish stack '*' | length stack > 2 = let stacklen  = length stack
                                                 (fst:sec:_) = drop (stacklen - 2) stack
                                                 newStack  = take (stacklen - 2) stack
                                             in Just $ stack ++ [(fst * sec)]
                        | otherwise        = Nothing
calcRevPolish stack '/' | length stack > 2 = let stacklen  = length stack
                                                 (fst:sec:_) = drop (stacklen - 2) stack
                                                 newStack  = take (stacklen - 2) stack
                                             in Just $ stack ++ [(fst / sec)]
                        | otherwise        = Nothing
calcRevPolish stack num = stack ++ (read num :: Int)
