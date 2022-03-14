module C where

-- This language accepts any sentence with an even length

data Alphabet = Zero | One | Epsilon
  deriving (Show)

type Sentence = [Alphabet]

s :: Sentence -> Bool
s (a : as) = case a of
  Zero -> x as
  One -> x as
  Epsilon -> True
s [] = True

x :: Sentence -> Bool
x (a : as) = case a of
  Zero -> y as
  One -> y as
  Epsilon -> True
x [] = False

y :: Sentence -> Bool
y (a : as) = case a of
  Zero -> x as
  One -> x as
  Epsilon -> False
y [] = True

main :: IO ()
main = do
  print $ s [One, Zero, One] -- False
  print $ s [Zero, One, Zero, Zero, Zero] -- False
  print $ s [Zero, One, Zero, Zero, Epsilon, Zero] -- False
  print $ s [Epsilon] -- True
  print $ s [Zero, One, Zero, Zero] -- True
  print $ s [Zero, One] -- True
  print $ s [] -- True