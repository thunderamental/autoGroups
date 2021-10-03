module Grigorchuk where

import Data.List
import Data.List.Split

-- uses Data.List.Split for splitOn. 
-- before loading in GHCI, in terminal, run: cabal install --lib split

-- reduces a word in bcd to a symbol in bcd.
subreduction :: [Char] -> [Char]
subreduction [] = []
subreduction [x] = [x]
subreduction (x:y:xs) 
    | x == y = subreduction xs -- any doubles are wiped
    | otherwise = case (x,y) of ('c','b') -> subreduction ('d':xs) -- bcd reduction rules
                                ('b','c') -> subreduction ('d':xs)
                                ('b','d') -> subreduction ('c':xs)
                                ('d','b') -> subreduction ('c':xs)
                                ('c','d') -> subreduction ('b':xs)
                                ('d','c') -> subreduction ('b':xs)
                                _ -> error "attempted subreduction of word NOT of bcd"

-- reduces a word in abcd to form a*x_1*a*x_2.. or x_1*a*x_2*a.. according to relations
reduction :: [Char] -> [Char]
reduction [] = []
reduction w@(x:xs) = purge $ intercalate "a" rw
    where pw = splitOn "a" w -- the non-a sub-words. 
          rw = map subreduction pw
-- no need to pattern match if word starts with a or not.
-- splitOn and intercalate handles for us VERY nicely (empty strings)

-- I originally forgot to delete double a's. here is a band-aid fix. 
purge :: [Char] -> [Char]
purge [] = []
purge [x] = [x]
purge ('a':'a':xs) = purge xs
purge (x:xs) = x: purge xs

-- checks if word is a valid axax.. or xaxa.. string
valid :: [Char] -> Bool
valid [] = True
valid [x] = True
valid ('a':x:xs) = (x /= 'a') && (valid (x:xs))
valid (x:'a':xs) = (x /= 'a') && (valid ('a':xs))
valid _ = False

reduce :: [Char] -> [Char]
reduce w = until valid reduction w -- heh heh, isnt this a nice sentence?



-- in terms of the zero and one subtree actions, the words are already in axaxax form.
-- now merely parse them and do the reducing of that output in the final process.

-- what a word in abcd does to the left subtree (zero) - output is unreduced
zero :: [Char] -> [Char]
zero [] = []
zero "a" = "a"
zero ('a':y:[]) = "a" <> [y]
zero ('a':y:'a':ws)
    | y == 'b' = ('c':(zero ws))
    | y == 'c' = ('d':(zero ws))
    | y == 'd' = ('b':(zero ws))
    | otherwise = error "aXa processing; X is not b, c or d"
zero (y:ws)
    | y == 'b' = ('a':(zero ws))
    | y == 'c' = ('a':(zero ws))
    | y == 'd' = (zero ws)
    | otherwise = error "(..a)X(a..) processing; X is not b, c or d"

-- what a word in abcd does to the right subtree (one) - output is unreduced
one :: [Char] -> [Char]
one [] = []
one "a" = "a"
one ('a':y:[]) = "a" <> [y]
one ('a':y:'a':ws)
    | y == 'b' = ('a':(one ws))
    | y == 'c' = ('a':(one ws))
    | y == 'd' = (one ws)
    | otherwise = error "(..a)X(a..) processing; X is not b, c or d"
one (y:ws)
    | y == 'b' = ('c':(one ws))
    | y == 'c' = ('d':(one ws))
    | y == 'd' = ('b':(one ws))
    | otherwise = error "aXa processing; X is not b, c or d"

-- finally, to count a's
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

-- OK, there are all the building blocks. Let's assemble them.
grigorWord :: [Char] -> Bool
grigorWord [] = True
grigorWord [a] = False
grigorWord w
    | (count 'a' r) `mod` 2 == 1 = False
    | otherwise = (grigorWord $ reduce $ zero r) && (grigorWord $ reduce $ one r)
    where r = reduce w

{-
it might be enlightening to ONLY see this as a word group for now.
instead of its 'true' realisation as automorphisms of the binary tree.
So the 'operation' is concatenation. Inverses are easy!-- abcde are involutions.

-}

invert :: [Char] -> [Char]
invert xs = reverse xs -- for completeness.

-- symbolic conjugation
wordconjugate :: [Char] -> [Char] -> [Char]
wordconjugate x y = reverse y ++ x ++ y

-- elemental conjugation
conjugate :: [Char] -> [Char] -> [Char]
conjugate y x = reduce $ reverse y ++ x ++ y -- Was conjugating in wrong order previously.

