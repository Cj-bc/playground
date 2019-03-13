-- original     : ax + by = c
-- perpendicular: bx - ay = d
--
-- Calculate 'd' by inserting (p,q) to (x,y)
--
--    ax      + by      = c
-- -) b(a/b)x - a(a/b)y = d(a/b)
-- -----------------------------
--        {b + a(a/b)}y = c - d(a/b)
--                    y = {c - d(a/b)}/{b + a(a/b)}
--
-- Push y to 'bx - ay = d'

perpPoint (p, q) (a, b, c) = (x, y)
  where d = b*p - a*q
        y = (c - d*(a/b))/(b + a*(a/b))
        x = (d + a*y) / b

main = do
  print $ perpPoint (1, 1) (1, 1, 1)
