import Data.Map as Map
import Data.Maybe

data Formula = Forall Char Formula
             | Exists Char Formula
             | And Formula Formula
             | Or Formula Formula
             | Not Formula
             | Impl Formula Formula
             | Pred Char [Char]
             
(~~) :: Formula -> Formula
(~~) = Not
(\/) :: Formula -> Formula -> Formula
(\/) = Or
(/\) :: Formula -> Formula -> Formula
(/\) = And
(==>) :: Formula -> Formula -> Formula
(==>) = Impl
(#) :: Char -> [Char] -> Formula
(#) = Pred

interp :: [x] -> Map Char ([x] -> Bool) -> Map Char x -> Formula -> Bool
interp dom preds val (Forall x f) = all (\v -> interp dom preds (Map.insert x v val) f) dom
interp dom preds val (Exists x f) = any (\v -> interp dom preds (Map.insert x v val) f) dom
interp dom preds val (And f g) = interp dom preds val f && interp dom preds val g
interp dom preds val (Or f g) = interp dom preds val f || interp dom preds val g
interp dom preds val (Not f) = not $ interp dom preds val f
interp dom preds val (Impl f g) = not (interp dom preds val f) || interp dom preds val g
interp dom preds val (Pred pred vars) = (preds ! pred) ((val !) <$> vars)

formulaP :: Formula
formulaP = Forall 'x' ('Q' # ['x'])
           \/ (Exists 'x' ((Forall 'y' ('R' # ['y', 'x']) \/ ('Q' # ['x']))
                          ==> Exists 'z' (Forall 'y' ('P' # ['z', 'y']))))

test :: Bool
test = 
    not (interp domain (Map.fromList [('P', p), ('Q', q), ('R', r)]) Map.empty formulaP)
  where
    domain = [0,1]
    p [z, y] = z > y
    q [x]    = x < 0
    r [y,x]  = y >= x