module Main where

data Alphabet = Zero | One | Epsilon
  deriving (Show)

type Sentence = [Alphabet]

-- Accepts any sequence that starts with a 0 and rejects everything else

-- Start symbol
s :: Sentence -> Bool
s (a : as) = case a of
  Zero -> x as -- Accepts if there is a 0 and then continues to the next symbol
  One -> False -- Rejects any sequence that does not start with a 0
  Epsilon -> True -- Epsilon is always accepted as it is the empty string
s [] = True -- Accept the empty string

-- X 'state'
x :: Sentence -> Bool
x (a : as) = case a of
  Zero -> x as -- Accepts and continues to the next symbol
  One -> x as -- Accepts and continues to the next symbol
  Epsilon -> False -- Epsilon is not allowed in the middle of a sequence, reject
x [] = True -- If it gets to the end, it should be valid

main :: IO ()
main = do
  print $ s [Epsilon] -- True
  print $ s [One, Zero, One] -- False
  print $ s [Zero, One, Zero, Zero, Zero] -- True
  print $ s [Zero, One, Zero, Zero, Epsilon, Zero] -- False