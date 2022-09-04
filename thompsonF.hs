{-
We can define elements of the F_n groups as integer lists.

-}

-- elimination of 'bad pairs'; this comes from the conjugate relation of F_2.
-- extend to n > 1.

import Data.Maybe
import Data.List

data Ind = Plus | Minus | Neutral deriving (Eq, Show)
type Elt = (Ind, Int) -- generator, not word. treat [Elt] as word.
type SemiElt = (Elt, Elt)

-- Read instance
inConvert :: String -> Maybe Elt
inConvert ('+':x) = Just (Plus, read x :: Int)
inConvert ('-':x) = Just (Minus, read x :: Int)
inConvert x = Just (Plus, read x :: Int)
-- Read word instance : check the MAYBE unwrapping.
stringConvert :: String -> [Elt]
stringConvert xs = if (Nothing `elem` word) 
                   then error $ show word 
                   else map fromJust $ word
  where word = map inConvert $ words xs

inverse :: Elt -> Elt
inverse (Plus, x) = (Minus, x)
inverse (Minus, x) = (Plus, x)

-- rewriteRules
rewriteRules :: [Elt] -> [Elt]
rewriteRules [] = []
rewriteRules [x] = [x]
rewriteRules (e1@(i1,x1):e2@(i2,x2):xs)
  | i1 == Plus  && i2 == Plus  && (x1 > x2) = [e2, (i1, x1 + 1)] ++ rewriteRules xs
  | i1 == Minus && i2 == Plus  && (x1 > x2) = [e2, (i1, x1 + 1)] ++ rewriteRules xs
  | i1 == Minus && i2 == Plus  && (x1 < x2) = [(i2, x2 + 1), e1] ++ rewriteRules xs
  | i1 == Minus && i2 == Minus && (x1 < x2) = [(i2, x2 + 1), e1] ++ rewriteRules xs
  | e1 == inverse e2 = rewriteRules xs
  | otherwise        = e1 : rewriteRules (e2:xs)

-- O(n^2) naive seminormal rewrite
naiveSeminorm :: [Elt] -> [Elt]
naiveSeminorm xs = if rewriteRules xs == xs 
                   then xs 
                   else naiveSeminorm $ rewriteRules xs

-- naive norm calculation step
normalStep :: [Elt] -> [Elt]
normalStep word
  | matching == [] = word
  | otherwise      = reduceNorm (last matching) positive negative
  where positive = [ snd x | x <- word, fst x == Plus ]  -- ascending
        negative = [ snd x | x <- word, fst x == Minus ] -- descending
        matching = filter (adjacent positive negative) $ intersect positive negative
adjacent :: [Int] -> [Int] -> Int -> Bool
adjacent xs ys n = if (elem (n+1) xs) || (elem (n+1) ys) 
                   then False 
                   else True

reduceNorm :: Int -> [Int] -> [Int] -> [Elt]
reduceNorm n positive negative = map (\x -> (Plus, x)) newPos 
                                 ++ map (\x -> (Minus, x)) newNeg 
  where newPos = delete n $ [x | x <- positive, x <= n] 
                            ++ [x - 1 | x <- positive, x > n]
        newNeg = delete n $ [x - 1 | x <- negative, x > n] 
                            ++ [x | x <- negative, x <= n]

-- iter-naively calculate norm of an element
naiveNorm :: [Elt] -> [Elt]
naiveNorm xs = if normalStep xs == xs 
                  then xs 
                  else naiveNorm $ normalStep xs

-- recursively calculate seminorm of an element
seminorm :: [Elt] -> [Elt]
seminorm []  = []
seminorm [x] = [x]
seminorm xs  = merge (seminorm firstHalf) (seminorm secndHalf)
    where splitted  = splitAt (((length xs) + 1) `div` 2) xs
          firstHalf = fst splitted
          secndHalf = snd splitted 

-- split SEMINORMAL element to positive and negative product
-- WARNING  . SEMINORMAL ONLY
divE :: [Elt] -> ([Elt], [Elt])
divE xs = (xp, xn)
  where xp = [ x | x <- xs, fst x == Plus  ]
        xn = [ x | x <- xs, fst x == Minus ]

-- important note: init is unsafe. can blow up if empty input..
  -- changed to ipop
ipop :: [x] -> [x]
ipop [] = []
ipop xs = init xs
-- negative-positive merge (n log n)
{-
brain time. what is the e_i? they are the offset parameter application
to the negative and positive words.

e1 incrememnts upwards if the final negative symbol + e1 is > the first
positive symbol + e2.

this represents a shifting of the symbol as the lowest index is just not
affected by shifts, only the higher index.

the final case is where we complete the product and the e_i is mapped across
as a parametric adjustment. 
-}
mergeNP :: [Elt] -> [Elt] -> Int -> Int -> [Elt]
mergeNP [] pw _ e2 = map (\(s,v) -> (s,v+e2)) pw -- was not clarified in paper
mergeNP nw [] e1 _ = map (\(s,v) -> (s,v+e1)) nw
mergeNP nw pw e1 e2
  | (snd j1) + e1 == (snd i1) + e2 = mergeNP (ipop nw) (tail pw) e1 e2
  | (snd j1) + e1 <  (snd i1) + e2 = (mergeNP (ipop nw) pw e1 (e2+1)) 
                                          <> [(Minus, (snd j1) + e1)]
  | (snd j1) + e1 >  (snd i1) + e2 = [(Plus, (snd i1) + e2)] <>
                                          (mergeNP nw (tail pw) (e1+1) e2)
  where j1 = last nw
        i1 = head pw
mergeNP _ _ _ _ = error "Input problematic??"

-- double positive merge (to right side)
-- only increment e1 when we move something from the right word to the front
mergePP :: [Elt] -> [Elt] -> Int -> Int -> [Elt]
mergePP [] pw _ e2 = map (\(s,v) -> (s,v+e2)) pw -- but e2 is always zero. (can remove e2 completely in theory)
mergePP nw [] e1 _ = map (\(s,v) -> (s,v+e1)) nw
mergePP nw pw e1 e2
  | (snd j1) + e1 <= (snd i1) + e2 = [(Plus, (snd j1) + e1)] <>
                                          (mergePP (tail nw) pw e1 e2)
  | (snd j1) + e1 > (snd i1) + e2  = [(Plus, (snd i1) + e2)] <>
                                          (mergePP nw (tail pw) (e1+1) e2)
  where j1 = head nw
        i1 = head pw
mergePP _ _ _ _ = error "Input problematic??"

-- double negative merge, to left side, without indexing
-- only increment e1 when we move something from the left word to the back
-- consider using reverse $ mergePP
mergeNN :: [Elt] -> [Elt] -> Int -> Int -> [Elt]
mergeNN [] pw _ e2 = map (\(s,v) -> (s,v+e2)) pw 
mergeNN nw [] e1 _ = map (\(s,v) -> (s,v+e1)) nw -- but e1 is always zero. (can remove e1 completely in theory)
mergeNN nw pw e1 e2
  | (snd j1) + e1 < (snd i1) + e2  = (mergeNN (ipop nw) pw e1 (e2+1)) 
                                      <> [(Minus, (snd j1) + e1)]
  | (snd j1) + e1 >= (snd i1) + e2 = (mergeNN nw (ipop pw) e1 e2) 
                                      <> [(Minus, (snd i1) + e2)]
  where j1 = last nw
        i1 = last pw
mergeNN _ _ _ _ = error "mergeNN error"

-- merging two seminormal form elements
merge :: [Elt] -> [Elt] -> [Elt]
merge xs ys = w2 ++ w3
  where (xp, xn) = divE xs
        (yp, yn) = divE ys
        (wp, wn) = divE (mergeNP xn yp 0 0)
        w2 = mergePP xp wp 0 0
        w3 = mergeNN wn yn 0 0

-- input is strictly seminormal.
normalise :: [Elt] -> [Elt]
normalise x = k1 ++ k2
  where (u1, u2) = divE x
        xs = (NormState u1 u2 0 0 [] [] [] [])
        (NormState k1 k2 d1 d2 s1 s2 w1 w2) = rebuild $ deleteB xs


type Stack = [Int]
data NormState = NormState [Elt] [Elt] Int Int Stack Stack [Elt] [Elt] deriving Show
-- k u1 u2 d1 d2 s1 s2 w1 w2
-- k is the p in F_p generalised group.
-- u_i are the positive and negative words in order.
-- d_i are the parametric index adjustment variables
-- s_i are the stacks that we will adjust the final products by.
-- w_i are the partial final output words (return w_1 w_2

deletecon1 :: NormState -> Bool
deletecon1 (NormState u1 u2 d1 d2 s1 s2 w1 w2)
  = (u1 /= []) && (u2 == [] || i_s > j_t)
  where i_s = snd $ last u1
        j_t = snd $ head u2

deletecon2 :: NormState -> Bool
deletecon2 (NormState u1 u2 d1 d2 s1 s2 w1 w2)
  = (u2 /= []) && (u1 == [] || i_s < j_t)
  where i_s = snd $ last u1
        j_t = snd $ head u2

-- "adjacent"
deletecon3 :: NormState -> Bool
deletecon3 (NormState u1 u2 d1 d2 s1 s2 w1 w2)
  = (u1 /= []) && (u2 /= []) && (i_s == j_t)
        -- the last elt of u1 and first elt of u2 is the same!
      && (((w1 /= []) && (s1 /= []) && not ((i_s <= a_e1) && (a_e1 <= i_s + 1))) || (w1 == []) || (s1 == []))
        -- if a-e1 is defined, it is not in [i_s..i_s+p]. if it's not defined, that's ok. (implies)
      && (((w2 /= []) && (s2 /= []) && not ((j_t <= b_e2) && (b_e2 <= j_t + 1))) || (w2 == []) || (s2 == []))
        -- if b-e2 is defined, it is not in [j_t..j_t+p]. if it's not defined, that's ok. (implies)
  where i_s = snd $ last u1
        j_t = snd $ head u2
        a  = snd $ head w1
        b  = snd $ last w2
        e1 = head s1
        e2 = head s2
        a_e1 = a - e1
        b_e2 = b - e2

-- "otherwise"
deletecon4 :: NormState -> Bool
deletecon4 (NormState u1 u2 d1 d2 s1 s2 w1 w2)
  = (u1 /= []) && (u2 /= []) && (i_s == j_t)
        -- the last elt of u1 and first elt of u2 is the same!
      && ((w1 /= []) && (s1 /= []) && (i_s <= a_e1) && (a_e1 <= i_s + 1)) 
        -- if a-e1 is defined, it is in [i_s..i_s+p]
      && ((w2 /= []) && (s2 /= []) && (j_t <= b_e2) && (b_e2 <= j_t + 1)) 
        -- if b-e2 is defined, it is in [j_t..j_t+p]
  where i_s = snd $ last u1
        j_t = snd $ head u2
        a  = snd $ head w1
        b  = snd $ last w2
        e1 = head s1
        e2 = head s2
        a_e1 = a - e1
        b_e2 = b - e2

deleteB :: NormState -> NormState
-- if either is empty, we are done.
deleteB n@(NormState [] u2 d1 d2 s1 s2 w1 w2) = n
deleteB n@(NormState u1 [] d1 d2 s1 s2 w1 w2) = n
deleteB n@(NormState u1 u2 d1 d2 s1 s2 w1 w2)
  | deletecon1 n
      = deleteB (NormState (ipop u1) u2 d1 d2 (0:s1) s2 ((Plus, i_s):w1) w2)
  | deletecon2 n
      = deleteB (NormState u1 (til u2) d1 d2 s1 (0:s2) w1 (w2 ++ [(Minus, j_t)]))
  | deletecon3 n
      = deleteB (NormState (ipop u1) (til u2) d1 d2 (s1n) (s2n) w1 w2)
  | otherwise -- deletecon4 n
      = deleteB (NormState (ipop u1) (til u2) d1 d2 (0:s1) (0:s2) ((Plus, i_s):w1) (w2 ++ [(Minus, j_t)]))
  where i_s = snd $ last u1
        j_t = snd $ head u2
        (s1n, s2n) = (incHead s1, incHead s2)

-- increment head (by p = 1) if nonempty.
incHead :: [Int] -> [Int]
incHead [] = []
incHead (x:xs) = (x+1):xs

rebuild :: NormState -> NormState
rebuild (NormState u1 u2 d1 d2 (c:s1s) s2 (i1:w1s) w2)
  = rebuild (NormState (u1 ++ [u]) u2 (nd) d2 (s1s) s2 (w1s) w2)
  where nd = d1 + c
        u  = (Plus, (snd i1) - nd)
rebuild (NormState u1 u2 d1 d2 s1 (c:s2s) [] w2@(_:_))
  = rebuild (NormState u1 (u:u2) d1 (nd) s1 s2s [] (init w2))
  where j1 = last w2
        nd = d2 + c
        u  = (Minus, (snd j1) - nd) 
rebuild n@(NormState u1 u2 d1 d2 s1 s2 [] []) = n
rebuild n = error $ show n

til :: [a] -> [a]
til [] = []
til xs = tail xs

-- conditional
unadjacent s (NormState u1 u2 d1 d2 s1 s2 w1 w2)
  = (a == Nothing || e1 == Nothing 
      || ((snd $ fromJust a) - (fromJust e1)) == s 
      || ((snd $ fromJust a) - (fromJust e1)) == (s+1) )
    ||
    (b == Nothing || e2 == Nothing 
      || (snd (fromJust b) - fromJust e2) == s 
      || (snd (fromJust b) - fromJust e2) == (s+1) )
  where a = mhead w1
        b = mlast w2
        e1 = mhead s1
        e2 = mhead s2

mhead :: [a] -> Maybe a
mhead [] = Nothing
mhead xs = Just (head xs)

mlast :: [a] -> Maybe a
mlast [] = Nothing
mlast xs = Just (last xs)

main :: IO ()
main = do
  print "Input elemental word"
  strInput <- getLine
  print "elemental word:"
  let elemWord = stringConvert $ strInput
  print elemWord
  let parsedInputSeminorm = seminorm $ elemWord
  print "seminormal form:"
  print parsedInputSeminorm
  let parsedInputNorm = normalise parsedInputSeminorm
  print "reduced normal form:"
  print parsedInputNorm
  