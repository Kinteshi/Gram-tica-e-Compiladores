module A where

-- This language must have the '100' substring in any sentence

data Alphabet = Zero | One | Epsilon
  deriving (Show)

type Sentence = [Alphabet]

s :: Sentence -> Bool
s (a : as) = case a of
  Zero -> x as
  One -> y as
  Epsilon -> False
s [] = False

x :: Sentence -> Bool
x (a : as) = case a of
  Zero -> x as
  One -> y as
  Epsilon -> False
x [] = False

y :: Sentence -> Bool
y (a : as) = case a of
  Zero -> z as
  One -> y as
  Epsilon -> False
y [] = False

z :: Sentence -> Bool
z (a : as) = case a of
  Zero -> w as
  One -> x as
  Epsilon -> False
z [] = True

w :: Sentence -> Bool
w (a : as) = case a of
  Zero -> w as
  One -> w as
  Epsilon -> False
w [] = True

main :: IO ()
main = do
  print $ s [] -- False
  print $ s [Epsilon] -- False
  print $ s [Zero, One, Zero, Zero, Epsilon, Zero] -- False
  print $ s [Zero, One] -- False
  print $ s [Zero, One, Zero] -- False
  print $ s [One, Zero, Zero] -- True
  print $ s [Zero, One, Zero, Zero, Zero] -- True
  print $ s [One, Zero, Zero, Zero] -- True
  print $ s [One, Zero, Zero] -- True
  print $ s [One, One, Zero, Zero] -- True
