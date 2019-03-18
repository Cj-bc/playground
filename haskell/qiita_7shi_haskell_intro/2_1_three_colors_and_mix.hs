data Color = Red | Blue | Green | Yellow | Cyan | Magenta | White
              deriving (Show, Enum, Eq)

mix :: Color -> Color -> Color
mix first second  | first == second = first
                  | (fromEnum first) + (fromEnum second) == 1 = Magenta
                  | (fromEnum first) + (fromEnum second) == 2 = Yellow
                  | (fromEnum first) + (fromEnum second) == 3 = Cyan
                  | (fromEnum first) + (fromEnum second) >= 7 = White
                  | first == Magenta && second == Green       = White
                  | first == Yellow  && second == Blue        = White
                  | first == Cyan    && second == Red         = White
                  | otherwise       = mix second first

main = do
  putStrLn $ show $ mix Red Blue
  putStrLn $ show $ mix Red Cyan
