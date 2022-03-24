module GLC01 where

import Control.Monad.Omega ( each, Omega(..) )


-- Grammar
-- Terminals = {a, b, c}
-- S -> XY
-- X -> aXb | ab
-- Y -> bYc | bc

type Rule = [Producer]
type NonTerminal = [Rule]
type Terminal = Char
data Producer = T Terminal | NT NonTerminal

ta, tb, tc :: Producer
ta = T 'a'
tb = T 'b'
tc = T 'c'

-- s, x, y :: Producer
-- s = NT [[x, y]] -- S -> XY
-- x = NT [[ta, tb], [ta, x, tb]] -- X -> aXb | ab
-- y = NT [[tb, tc], [tb, y, tc]] -- Y -> bYc | bc

s, x, y :: Producer
s = NT [[ta, x], [tb, y]]
x = NT [[tb], [tb, x], [ta, x]]
y = NT [[ta], [ta, y], [tb, y]]

-- t0, t1 :: Producer
-- t0 = T '0'
-- t1 = T '1'


-- Generate all possible productions
generateLanguage :: Producer -> Omega String
generateLanguage (T c) = return [c]
generateLanguage (NT rules) = do
    rule <- each rules
    words <- mapM generateLanguage rule
    return $ concat words


generateLanguageWithMaxLength :: Int -> Producer -> Omega String
generateLanguageWithMaxLength maxLength p = do
    w <- generateLanguage p
    if length w <= maxLength
        then return w
        else return []


omegaGenerateLanguage :: Producer -> [String]
omegaGenerateLanguage = runOmega . generateLanguage

maxLength :: Foldable t => Int -> t a -> Bool
maxLength m s = length s <= m

main :: IO ()
main = do
    print $ filter (maxLength 10) $ take 100 $ omegaGenerateLanguage s
    -- print $ runOmega $ generateLanguageWithMaxLength 10 s
