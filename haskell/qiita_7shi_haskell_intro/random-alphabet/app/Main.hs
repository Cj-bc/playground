import System.Random

repeatTillSmallZ 'z' = do
              print "z"
              print "END"
repeatTillSmallZ x   = do
              print x
              w <- getStdRandom $ randomR ('a', 'z')
              repeatTillSmallZ w

main = do
  word <- getStdRandom $ randomR ('a', 'z')
  repeatTillSmallZ word
