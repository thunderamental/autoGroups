{-
We can define elements of the F_n groups as integer lists.

-}

-- elimination of 'bad pairs'; this comes from the conjugate relation of F_2.
-- extend to n > 1.

import Data.Maybe
import Data.List

data Ind = Plus | Minus deriving (Eq, Show)
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

naiveNorm :: [Elt] -> [Elt]
naiveNorm xs = if normalStep xs == xs 
                  then xs 
                  else naiveNorm $ normalStep xs

seminorm :: [Elt] -> [Elt]
seminorm []  = []
seminorm [x] = [x]
seminorm xs  = merge (seminorm firstHalf) (seminorm secndHalf)
    where splitted  = splitAt (((length xs) + 1) `div` 2) xs
          firstHalf = fst splitted
          secndHalf = snd splitted 

merge :: [Elt] -> [Elt] -> [Elt]
-- x and y are elements in seminormal form.
merge x y = x <> y

-- important note: init is unsafe. can blow up if empty input..

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
  | (snd $ j1) + e1 == (snd $ i1) + e2 = mergeNP (init nw) (tail pw) e1 e2
  | (snd $ j1) + e1 < (snd $ i1) + e2  = (mergeNP (init nw) pw e1 (e2+1)) 
                                          <> [(Minus, (snd j1) + e1)]
  | (snd $ j1) + e1 > (snd $ i1) + e2  = [(Plus, (snd i1) + e2)] <>
                                          (mergeNP nw (tail pw) (e1+1) e2)
  where j1 = last nw
        i1 = head pw
mergeNP _ _ _ _ = error "Input problematic??"

-- double positive merge (to right side)
-- only increment e1 when we move something from the right word to the front
mergePP :: [Elt] -> [Elt] -> Int -> Int -> [Elt]
mergePP [] pw _ e2 = map (\(s,v) -> (s,v+e2)) pw -- but e2 is always zero.
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
mergeNN nw [] e1 _ = map (\(s,v) -> (s,v+e1)) nw -- but e1 is always zero.
mergeNN nw pw e1 e2
  | (snd j1) + e1 < (snd i1) + e2  = (mergeNN (init nw) pw e1 (e2+1)) <> [(Minus, (snd j1) + e1)]
  | (snd j1) + e1 >= (snd i1) + e2 = (mergeNN nw (init pw) e1 e2) <> [(Minus, (snd i1) + e2)]
  where j1 = last nw
        i1 = last pw
mergeNN _ _ _ _ = error "Input problematic??"

main :: IO ()
main = do
  print "Input elemental word"
  strInput <- getLine
  print "elemental word:"
  let elemWord = stringConvert $ strInput
  print elemWord
  let parsedInputSeminorm = naiveSeminorm $ elemWord
  let parsedInputNorm = naiveNorm parsedInputSeminorm
  print "reduced normal form:"
  print parsedInputNorm
  