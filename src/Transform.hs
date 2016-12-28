module Transform
  ( hdwt, fdwt
  , ihdwt, ifdwt
  ) where

import           Data.List       (foldl', tails)
import           Data.Scientific as Scientific


-- | Convert wavelet filter coefficients
toLDec, toLCons :: [Scientific] -> [Scientific]
toLDec  = id
toLCons = reverse

toHDec, toHCons :: [Scientific] -> [Scientific]
toHDec  = reverse . toHCons
toHCons = zipWith (*) (cycle [-1, 1])


-- | Dilution and decimation by 2
decimate, dilute :: [Scientific] -> [Scientific]
dilute    = init . foldr (\ x acc -> x : 0 : acc) []
decimate  = evens

evens []     = []
evens (x:xs) = x : odds xs

odds []     = []
odds (_:xs) = evens xs


-- | List convolution
conv :: [Scientific] -> [Scientific] -> [Scientific]
conv xs hs  =
    let pad = replicate (length hs - 1) 0
        ts  = pad ++ xs
     in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)


-- | compose f n times
iter :: (a -> a) -> Int -> a -> a
iter f n = foldr (.) id (replicate n f)

thresh :: Scientific -> Bool
thresh = (>= 1e-7)

getFst :: (a, b, c) -> a
getFst (x, _, _)    = x


hswt, fswt :: [Scientific] -> [Scientific] -> Int -> [[Scientific]]
hswt xs hs n    = getFst (iter step n ([xs], lo, hi))
    where
        lo  = toLDec hs
        hi  = toHDec hs

        step (z:zs, a, b) =
            let c   = z `conv` a
                d   = z `conv` b
                a'  = dilute a
                b'  = dilute b
             in (c:d:zs, a', b')

fswt xs hs n    = getFst (iter step n ([xs], lo, hi))
    where
        lo  = toLDec hs
        hi  = toHDec hs

        step (lst, a, b) =
            let res = foldr op [] lst
                op x acc = x `conv` a : x `conv` b : acc
                a'  = dilute a
                b'  = dilute b
             in (res, a', b')


ihswt, ifswt :: [[Scientific]] -> [Scientific] -> [Scientific]
ihswt xss ys    =
    filter thresh $ concat $ getFst $ iter step m (xss, as, bs)
    where
        m   = length xss - 1    -- number of stage
        as  = iter dilute (m - 1) (toLCons ys)
        bs  = iter dilute (m - 1) (toHCons ys)

        step (x:y:lst, hs, gs)  =
            let hs' = decimate hs
                gs' = decimate gs
                n   = length hs
                x'  = zipWith (+) (conv x hs) (conv y gs)
                x'' = drop (n - 1) $ map (/2) x'
             in (x'':lst, hs', gs')


ifswt xss ys    =
    filter thresh $ concat $ getFst $ iter step m (xss, as, bs)
    where
        n   = length xss
        m   = round $ logBase 2 (fromIntegral n)
        as  = iter dilute (m - 1) (toLCons ys)
        bs  = iter dilute (m - 1) (toHCons ys)

        step (lst, hs, gs)  =
            let hs' = decimate hs
                gs' = decimate gs
                ev  = map (conv hs) (evens lst)
                od  = map (conv gs) (odds lst)
                res = map (map (/2)) $ zipWith (zipWith (+)) ev od
             in (res, hs', gs')


hdwt, fdwt :: [Scientific] -> [Scientific] -> Int -> [[Scientific]]
hdwt xs hs n    = iter step n [xs]
    where
        step (y:ys) =
            let lo  = decimate (y `conv` toLDec hs)
                hi  = decimate (y `conv` toHDec hs)
             in lo:hi:ys


fdwt xs hs n    = iter step n [xs]
    where
        step lst    =
            let op z acc    =
                    decimate (z `conv` toLDec hs):
                    decimate (z `conv` toHDec hs):acc
             in foldr op [] lst


ihdwt, ifdwt :: [[Scientific]] -> [Scientific] -> [Scientific]
ihdwt xss hs    = filter thresh $ concat $ iter step m xss
    where
        m   = length xss - 1
        n   = length hs - 1

        step (x:y:ys)   =
            let lo  = dilute x `conv` toLCons hs
                hi  = dilute y `conv` toHCons hs
                res = drop n (zipWith (+) lo hi)
             in res:ys


ifdwt xss hs    = filter thresh $ concat $ iter step m xss
    where
        m   = round $ logBase 2 (fromIntegral $ length xss)
        as  = toLCons hs
        bs  = toHCons hs

        step lst    =
            let ev  = map (conv as . dilute) (evens lst)
                od  = map (conv bs . dilute) (odds lst)
             in zipWith (zipWith (+)) ev od
