module Nats (
    Natural (..),
    natToInt,
    diffNat,
) where

import Prelude
import Data.Ratio

data Natural = Succ Natural | Zero
    deriving (Show, Read)

natToInt :: Natural -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

diffNat :: Natural -> Natural -> Natural
diffNat n1 n2 = if n1 > n2 then n1 - n2 else n2 - n1

instance Eq Natural where
    (==) Zero Zero = True
    (==) (Succ n) (Succ n') = n == n'
    (==) _ _ = False

instance Ord Natural where
    (<=) Zero Zero = True
    (<=) Zero _ = True
    (<=) _ Zero = False
    (<=) (Succ n) (Succ n') = n <= n'

instance Num Natural where
    (+) n Zero = n
    (+) Zero n = n
    (+) n (Succ Zero) = Succ n
    (+) (Succ Zero) n = Succ n
    (+) n (Succ n') = (Succ n) + n'

    (*) n Zero = Zero
    (*) Zero n = Zero
    (*) (Succ Zero) n = n
    (*) n (Succ Zero) = n
    (*) (Succ n) n' = n' + (n * n')

    (-) n Zero = n
    (-) Zero n = Zero
    (-) (Succ n) (Succ n') = n - n'

    abs n = n

    signum Zero = Zero
    signum _ = Succ Zero

    fromInteger 0 = Zero
    fromInteger x = Succ (fromInteger (x-1))

instance Real Natural where
    toRational n = (fromIntegral . natToInt $ n) % 1

-- TODO make an instance of this that doesn't cheat
instance Integral Natural where
    toInteger = fromIntegral . natToInt
    quotRem l r = (fromInteger l', fromInteger r')
        where (l', r') = quotRem (toInteger l) (toInteger r)

instance Enum Natural where
    fromEnum = natToInt
    toEnum = fromInteger . fromIntegral
