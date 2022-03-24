module GR01 where

data Alphabet = A | B
    deriving (Show)

type Sentence = [Alphabet]

-- Grammar requires to each sentence to have at least five A's

s :: Sentence -> Bool
s (a : as) = case a of
    A -> x as
    B -> s as
s [] = False

x :: Sentence -> Bool
x (a : as) = case a of
    A -> y as
    B -> x as
x [] = False

y :: [Alphabet] -> Bool
y (a : as) = case a of
    A -> w as
    B -> y as
y [] = False

w :: [Alphabet] -> Bool
w (a : as) = case a of
    A -> z as
    B -> w as
w [] = False

z :: [Alphabet] -> Bool
z (a : as) = case a of
    A -> k as
    B -> z as
z [] = False

k :: [Alphabet] -> Bool
k (a : as) = case a of
    A -> k as
    B -> k as
k [] = True


main :: IO ()
main = do
    print $ s [A] -- False
    print $ s [A, A] -- False
    print $ s [A, A, A, A] -- False
    print $ s [A, B, A, A, A, B] -- False
    print $ s [A, A, A, A, A] -- True
    print $ s [A, B, A, A, A, A, B] -- True
    print $ s [A, B, A, A, A, A, A, B] -- True
    print $ s [A, B, A, A, A, A, A, A, B] -- True
    print $ s [A, B, A, A, A, A, A, A, A, B] -- True
    print $ s $ replicate 6 A -- True
    print $ s $ replicate 3 A -- False