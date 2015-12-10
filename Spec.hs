import System.Random
import Test.QuickCheck
import Data.List ((\\))

import Filterbank
import Daubechies


randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)


-- Decimation properties
prop_decimate xs = evens xs == xs \\ odds xs

-- Dilution properties
prop_dilute (NonEmpty xs) = decimate (dilute xs) == xs  


-- Conv properties
prop_conv_comm (NonEmpty xs) (NonEmpty hs)  = 
    filter (/=0) (conv xs hs) == filter (/=0) (conv hs xs)

prop_conv_assoc (NonEmpty xs) (NonEmpty ys) (NonEmpty zs)   =
    filter (/= 0) (conv (conv xs ys) zs) == filter (/= 0) (conv xs (conv ys zs))

signalAdd xs ys =
    if length xs > length ys
       then zipWith (+) xs (ys ++ replicate (length xs - length ys) 0)
       else zipWith (+) ys (xs ++ replicate (length ys - length xs) 0)

prop_conv_dist (NonEmpty xs) (NonEmpty ys) (NonEmpty zs)    =
    signalAdd (conv xs ys) (conv xs zs) == conv xs (signalAdd ys zs)
    
prop_conv_id (NonEmpty xs)  = conv xs [1] == xs

prop_conv_mult n (NonEmpty xs) (NonEmpty ys)  =
    map (*n) (conv xs ys)   == conv (map (*n) xs) ys
