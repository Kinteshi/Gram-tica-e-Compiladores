module D where

-- This language accepts only C-style block comments

data Alphabet = Bar | Asterisk | Anything
  deriving (Show)

type Sentence = [Alphabet]

s :: Sentence -> Bool
s (a : as) = case a of
  Bar -> x as
  _ -> False
s [] = True

x :: Sentence -> Bool
x (a : as) = case a of
  Asterisk -> y as
  _ -> False
x [] = False

y :: Sentence -> Bool
y (a : as) = case a of
  Asterisk -> w as
  _ -> y as
y [] = False

w :: Sentence -> Bool
w (a : as) = case a of
  Bar -> z as
  Asterisk -> w as
  Anything -> y as
w [] = False

z :: Sentence -> Bool
z (a : as) = case a of
  Asterisk -> w as
  _ -> y as
z [] = True

main :: IO ()
main = do
  print $ s [Bar, Asterisk, Anything, Asterisk, Anything, Asterisk, Anything, Asterisk, Asterisk, Bar, Bar] -- False
  print $ s [Bar, Anything, Asterisk, Bar] -- False
  print $ s [Bar, Asterisk, Anything, Anything, Anything, Anything, Asterisk, Bar] -- True
  print $ s [Bar, Asterisk, Anything, Asterisk, Anything, Asterisk, Anything, Asterisk, Asterisk, Bar] -- True
  print $ s [Bar, Asterisk, Anything, Asterisk, Anything, Asterisk, Anything, Asterisk, Bar, Asterisk, Bar] -- True