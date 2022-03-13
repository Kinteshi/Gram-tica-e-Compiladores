module Main where

data Alphabet = Zero | One | Epsilon
  deriving (Show)

type Sentence = [Alphabet]

-- Accepts any sequence that starts with a 0 and rejects everything else

-- Start symbol
s :: Sentence -> Bool
s (a : as) = case a of
  Epsilon -> True -- Epsilon is always accepted as it is the empty string
  Zero -> case as of
    [] -> True
    _ -> x as
  One -> case as of
    [] -> True
    _ -> y as
s [] = True -- Accept the empty string

x :: Sentence -> Bool
x (a : as) = case a of
  Zero -> case as of
    [] -> True
    _ -> x as
  One -> case as of
    [] -> False
    _ -> x as
  Epsilon -> False
x [] = True -- Accept the empty string

y :: Sentence -> Bool
y (a : as) = case a of
  Zero -> case as of
    [] -> False
    _ -> x as
  One -> case as of
    [] -> True
    _ -> y as
  Epsilon -> False
y [] = True -- Accept the empty string

main :: IO ()
main = do
  print $ s [Epsilon] -- True
  print $ s [One, Zero, One] -- False
  print $ s [Zero, One, Zero, Zero, Zero] -- True
  print $ s [Zero, One, Zero, Zero, Epsilon, Zero] -- False
  print $ s [Zero, One, Zero, Zero, One] -- False