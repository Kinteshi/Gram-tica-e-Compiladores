{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GR03 where

-- Grammar where each sentence have length that is not a factor of 3

data Alphabet = A | B
    deriving Show

type Sentence = [Alphabet]

s :: Sentence -> Bool
s a = case a of
    [A]    -> True
    [B]    -> True
    []     -> False
    (A:as) -> x as
    (B:as) -> x as

x :: Sentence -> Bool
x a = case a of
    [A]    -> True
    [B]    -> True
    (A:as) -> y as
    (B:as) -> y as

y :: Sentence -> Bool
y a = case a of
    [A]    -> False
    [B]    -> False
    (A:as) -> s as
    (B:as) -> s as


main :: IO ()
main = do
    print $ s [] -- False
    print $ s $ replicate 1 A -- True
    print $ s $ replicate 2 A -- True
    print $ s $ replicate 3 A -- False
    print $ s $ replicate 4 A -- True
    print $ s $ replicate 5 A -- True
    print $ s $ replicate 6 A -- False
    print $ s $ replicate 7 A -- True
    print $ s $ replicate 8 A -- True
    print $ s $ replicate 9 A -- False