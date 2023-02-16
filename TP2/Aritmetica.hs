module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits


--(1)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt _ _ = (0, (0, 0))

--(2)
criba :: Integer -> Set Integer
criba 0 = []
criba n | esPrimo (n-1) == True = (esPrimo(n-1)):(criba (n-1))
        | otherwise = criba(n-1)

esPrimo :: Integer -> Bool 
esPrimo 1 = False
esPrimo n = n == menorDivisor n

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisordesde n 2

menorDivisordesde :: Integer -> Integer -> Integer
menorDivisordesde n k | n `mod` k == 0 = k
                      | otherwise = menorDivisordesde n (k+1)

--(3)
coprimoCon:: Integer -> Integer
coprimoCon _ = 0


--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo _ _ = 0



-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
