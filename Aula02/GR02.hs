module GR02 where

-- Grammar with sentences of size as factor of 3

data Alphabet = A | B | Epsilon
    deriving Show

type Sentence = [Alphabet]


s :: Sentence -> Bool
s (a:as) = case a of
    Epsilon -> True
    _ -> x as
s [] = True


x :: Sentence -> Bool
x (a:as) = case a of
    Epsilon -> False
    _ -> y as
x [] = False

y :: Sentence -> Bool
y (a:as) = case a of
    Epsilon -> False
    _ -> z as
y [] = False

z :: Sentence -> Bool
z (a:as) = case a of
    Epsilon -> False
    _ -> x as
z [] = True

main = do
    print $ s [] -- True
    print $ s $ replicate 1 A -- False
    print $ s $ replicate 2 A -- False
    print $ s $ replicate 3 A -- True
    print $ s $ replicate 4 A -- False
    print $ s $ replicate 5 A -- False
    print $ s $ replicate 6 A -- True
    print $ s $ replicate 7 A -- False
    print $ s $ replicate 8 A -- False
    print $ s $ replicate 9 A -- True
