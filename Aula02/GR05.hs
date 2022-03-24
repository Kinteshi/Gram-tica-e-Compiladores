module GR05 where


-- Grammar requires that each sentence has a substring 100 and a even length

data Alphabet = Zero | One

type Sentence = [Alphabet]

s :: Sentence -> Bool
s a = case a of -- Previous state is even (empty)
    [] -> False
    [_] -> False
    (One:as) -> x as
    (Zero:as) -> w as

x :: Sentence -> Bool -- Previous state is odd
x a = case a of
    [] -> False
    [_] -> False
    (One:as) -> k as
    (Zero:as) -> y as

y :: Sentence -> Bool
y a = case a of
    [] -> False
    [_] -> False
    (One:as) -> x as
    (Zero:as) -> z as

z :: Sentence -> Bool
z a = case a of
    [] -> False
    [_] -> True
    (_:as) -> m as

w :: Sentence -> Bool
w a = case a of
    [] -> False
    [_] -> False
    (One:as) -> k as
    (Zero:as) -> s as

k :: Sentence -> Bool -- previous state was even
k a = case a of
    [] -> False
    [_] -> False
    (One:as) -> x as
    (Zero:as) -> l as

l :: Sentence -> Bool
l a = case a of
    [] -> False
    [Zero] -> True
    [One] -> False
    (Zero:as) -> m as
    (One:as) -> k as

m :: Sentence -> Bool
m as = case as of
    [] -> False
    [_] -> False
    (_:as) -> z as

main :: IO ()
main = do
    -- putStrLn "Enter a sentence"
    -- sentence <- getLine
    -- putStrLn $ show $ s (map (\x -> if x == '0' then Zero else One) sentence)
    print $ s [One, Zero, Zero] -- False
    print $ s [Zero, One, Zero, One] -- False
    print $ s [Zero, One, Zero, Zero, Zero] -- False
    print $ s [One, Zero, Zero, One] -- True
    print $ s [One, Zero, Zero, Zero] -- True
    print $ s [Zero, One, Zero, Zero] -- True
    print $ s [Zero, One, Zero, Zero, Zero, One] -- True