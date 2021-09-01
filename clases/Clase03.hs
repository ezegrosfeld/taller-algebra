-- RECURSION

{- Fibonacci function using recursion -}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

{- parteEntera calcula la parte entera de un numero real -}
parteEnteraNoRecursiva :: Float -> Integer
parteEnteraNoRecursiva = truncate

{- parteEnteraRecursiva :: Float -> Integer
parteEnteraRecursiva x
  |
 -}
{- Funcion que determina si un numero es multiplo de 3 recursivamente sin usar ni div ni mod -}
esMultiploDeTresRecursivo :: Integer -> Bool
esMultiploDeTresRecursivo n
  | n == 0 = True
  | n < 0 = False
  | otherwise = esMultiploDeTresRecursivo (n - 3)

{- Funcion que dado un n, suma todos los primeros n numeros impares -}
sumaImpares :: Integer -> Integer
sumaImpares n
  | n == 1 = 1
  | n <= 0 = 0
  | otherwise = 2 * n - 1 + sumaImpares (n - 1)

{- medioFact calcula el medio factorial (n!!) de n -}
medioFact :: Integer -> Integer
medioFact n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n * medioFact (n - 2)

{- sumaDigitos suma todos los digitos de un numero -}
sumaDigitos :: Integer -> Integer
sumaDigitos n
  | n < 10 = n
  | otherwise = mod n 10 + sumaDigitos (div n 10)

{- todosIguales determina si todos los digitos de un numero son iguales -}
todosIguales :: Integer -> Bool
todosIguales n
  | n < 10 = True
  | otherwise = mod n 10 == mod (div n 10) 10 && todosIguales (div n 10)

{- --------------------- -}
{-  EJERCICIOS DEL ZOOM  -}
{- --------------------- -}

{--
Escribir una función que dados dos naturales n y m, devuelva
el resultado de elevar n a la m (sin usar ^ y usando recursión).
--}

power :: Integer -> Integer -> Integer
power n m
  | m == 0 = 1
  | otherwise = n * power n (m - 1)

{--
Escribir una función log2 :: Integer -> Integer que devuelva el
logaritmo (entero) en base 2 del argumento, es decir, el piso
("floor", o parte entera por abajo) del log_2 de un natural n.

log2 8 = 3

log2 9 = 3
--}

log2 :: Integer -> Integer
log2 n
  | n == 1 = 0
  | otherwise = 1 + log2 (div n 2)

{--
Escriba una función dígitosDecrecientes :: Integer -> Bool tal que
para cada entero positivo n la expresión dígitosDecrecientes n sea
True o False dependiendo de si los dígitos de n decrecen de derecha
a izquierda.
--}

digitosDecrecientes :: Integer -> Bool
digitosDecrecientes n
  | n < 10 = True
  | otherwise = mod n 10 > mod (div n 10) 10 && digitosDecrecientes (div n 10)

{--
Escriba una función digitosOrdenados :: Integer -> Bool tal que para
cada entero positivo n la expresión digitosOrdenados n sea True o
False dependiendo de si los dígitos de n o bien crecen o bien decrecen
de derecha a izquierda.
--}
digitosCrecientes :: Integer -> Bool
digitosCrecientes n
  | n < 10 = True
  | otherwise = mod n 10 < mod (div n 10) 10 && digitosCrecientes (div n 10)

digitosOrdenados :: Integer -> Bool
digitosOrdenados n = digitosDecrecientes n || digitosCrecientes n

{--
El triangulo de Pascal es la siguiente tabla triangular
cuyas primeras filas son las siguientes:

       k
     | 0   1   2   3   4   5   6   7   8   9  10
n 0  | 1
  1  | 1   1
  2  | 1   2   1
  3  | 1   3   3   1
  4  | 1   4   6   4   1
  5  | 1   5   10  10  5   1
  6  | 1   6   15  20  15  6   1
  7  | 1   7   21  35  35  21  7   1
  8  | 1   8   28  56  70  56  28  8   1
  9  | 1   9   36  84  126 126 84  36  9  1
  10 | 1   10  45  120 210 252 210 120 45 10 1

que se arma de la siguiente manera:

• la columna con n = 0 está completa con 1,
• la diagonal en la que n = k está completa con 1,
• todas todas las otras entradas son la suma de la que está
arriba y la que está justo arriba a la izquierda: por ejemplo
el 126 que está en la fila 9 y la columna 4 es la suma del 70
que está inmediatamente arriba y el 56 que está a la izquierda
de este.

Escriba una función pascal :: Integer -> Integer -> Integer tal
que la expresión pascal n k sea el entero que está en la tabla
en la fila n-ésima y la columa k-ésima.
--}

pascal :: Integer -> Integer -> Integer
pascal n k
  | n == k = 1
  | k == 0 = 1
  | otherwise = pascal (n - 1) (k - 1) + pascal (n - 1) k