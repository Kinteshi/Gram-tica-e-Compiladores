{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GR04 where 

-- Grammar requires that each sentence starts and ends with different symbols

data Alphabet = A | B

type Sentence = [Alphabet]

s :: [Alphabet] -> Bool
s a = case a of
    [_] -> False
    [] -> False
    (A:as) -> x as
    (B:as) -> y as

x :: [Alphabet] -> Bool
x a = case a of
    [B] -> True
    [A] -> False
    (_:as) -> x as

y :: [Alphabet] -> Bool
y a = case a of
    [A] -> True
    [B] -> False
    (_:as) -> y as

main :: IO ()
main = do
    print $ s [A,B,A,B,A] -- False
    print $ s [A,B,B,B,A] -- False
    print $ s [B,B,B,B,B] -- False
    print $ s [B,B,B,A,B] -- False
    print $ s [A,B,A,B,B] -- True
    print $ s [A,B,B,A,B] -- True
    print $ s [B,B,B,B,A] -- True
    print $ s [B,B,A] -- True