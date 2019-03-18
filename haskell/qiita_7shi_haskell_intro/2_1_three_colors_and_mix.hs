data Color = Red | Blue | Green | Yellow | Cyan | Magenta
              deriving (Show, Enum, Eq)

mix :: Color -> Color -> Color
mix first second  | first == second = first
                  | (fromEnum first) + (fromEnum second) == 1 = Magenta
                  | (fromEnum first) + (fromEnum second) == 2 = Yellow
                  | (fromEnum first) + (fromEnum second) == 3 = Cyan

main = do
  putStrLn $ show $ mix Red Blue
