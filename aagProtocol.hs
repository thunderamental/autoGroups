import Grigorchuk

-- instance of AAG Protocol.

-- generating sets
genA :: [String]
genA = ["ad", "ca", "bad"]
genB :: [String]
genB = ["c", "caca", "ab", "ac"]

-- secret-sequences
ssA :: [String]
ssA = ["ad", invert "ca", "bad"] 
ssB :: [String]
ssB = ["c", invert "caca", "ab", invert "ac"]


-- secrets now encoded elementally 
sA :: String
sA = reduce $ concat ssA -- "adadad" 
sB :: String
sB = reduce $ concat ssB -- "cacada"

-- transmission 'tables' : the 'manual' way
tA :: [String]
tA = map (conjugate sA) genB
tB :: [String]
tB = map (conjugate sB) genA

-- adjusted to secret string. in reality, we have to do this 'manually'.
-- see that for x,y ; (x^{-1}yx)^{-1} == (x^{-1}y^{-1}x)
-- that is, for x`y = x^{-1}yx the conjugator-operation, (x`y)^{-1} = (x`y^{-1})
-- Conjugate of inverse is inverse of conjugate
tA2 :: [String]
tA2 = map (conjugate sA) ssB -- B's secret config
tB2 :: [String]
tB2 = map (conjugate sB) ssA -- A's secret config

-- shared secret 'precomputation' for completeness
kA :: String
kA = reduce $ concat tB2 -- this is b^{-1}ab. 
kB :: String
kB = reduce $ concat tA2 -- this is a^{-1}ba.


keyA :: String
keyA = reduce $ (invert sA) ++ kA
keyB :: String
keyB = reduce $ (invert kB) ++ sB


trueKey :: Bool
trueKey = keyA == keyB