{-recibe un número natural n y devuelve True si el número n es suma de dos
cubos y False en caso contrario-}

esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos  n | n <= 0 = undefined
                    | otherwise = esSumaDeCubosAux n 1 

esSumaDeCubosAux :: Integer -> Integer -> Bool
esSumaDeCubosAux n s | s^3 >= n = False
                     | esUnCubo (n-(s^3)) = True                     
                     | otherwise = esSumaDeCubosAux n (s+1)

esUnCubo :: Integer -> Bool
esUnCubo x = round (fromIntegral x ** (1/3)) ^ 3 == x

{-Dado un natural n que se escribe como suma de dos cubos devuelve un par
(a, b) de números naturales tales que n = a^3 + b^3-}

descomposicionCubos :: Integer -> (Integer, Integer)
descomposicionCubos n = descomposicionCubosAux n 1 

descomposicionCubosAux :: Integer -> Integer -> (Integer, Integer)
descomposicionCubosAux n s | (s^3) >= n = undefined  {- devuelve undefined en caso de que no existan a^3 + b^3 = n -}
                           | esUnCubo (n-(s^3)) = (s, (round (fromIntegral (n-(s^3))**(1/3))))
                           | otherwise = descomposicionCubosAux n (s+1)

{-Recibe un número natural n y devuelve la cantidad de formas en la que n se
escribe como suma de dos cubos-}

cantidadDeFormas :: Integer -> Integer 
cantidadDeFormas n = contadorCubosAux n 1  

contadorCubosAux :: Integer -> Integer -> Integer
contadorCubosAux n s | (s^3) >= (n `div` 2) = 0
                     | esUnCubo (n-(s^3)) = 1 + contadorCubosAux n (s+1) 
                     | otherwise = contadorCubosAux n (s+1)  

{-Recibe un número natural n y devuelve el menor número “especial” mayor o
igual que n-}

especialDesde :: Integer -> Integer
especialDesde n | esEspecial n = n
                | otherwise = especialDesde (n+1)

esEspecial :: Integer -> Bool
esEspecial n = cantidadDeFormas n >= 2

{-Recibe un número natural n y devuelve el n-ésimo número “especial”-}

especialNumero :: Integer -> Integer 
especialNumero n | n <= 0 = undefined
                 | n == 1 = 1729
                 | otherwise = especialDesde (1 + especialNumero (n-1) )

{-Recibe un número natural n y devuelve True si el número n es “muy especial”
y False en caso contrario-}

esMuyEspecial :: Integer -> Bool 
esMuyEspecial n | not (esEspecial n) = False 
                | otherwise = revisarEspecialesAnteriores n 1 


revisarEspecialesAnteriores :: Integer -> Integer -> Bool
revisarEspecialesAnteriores n c | c > n = True               
                                | esUnCubo cociente && cociente > 1 = False 
                                | otherwise = revisarEspecialesAnteriores n (especialDesde (c+1))                                                       
                                  where cociente = div n c





