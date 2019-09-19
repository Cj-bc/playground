ahogebunbetu :: String -> IO String
ahogebunbetu "1" = pure "一本タイプ"
ahogebunbetu _ = pure "複数本タイプ"

-- main = do
--   a <- getLine
--   putStrLn $ ahogebunbetu a
main :: IO ()
main = getLine >>= ahogebunbetu >>= putStrLn
