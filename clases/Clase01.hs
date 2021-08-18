{- Funcion que suma 2 numeros -}
sumar :: Int -> Int -> Int
sumar x y = x + y

{- Funcion que resta 2 numeros -}
restar :: Int -> Int -> Int
restar x y = x - y

{- Funcion que multiplica 2 numeros -}
multiplicar :: Int -> Int -> Int
multiplicar x y = x * y

{- Funcion que duplica un numero -}
duplicar :: Int -> Int
duplicar x = x * 2

{- Funcion que obtiene la norma vectorial -}
norma :: Float -> Float -> Float
norma x1 x2 = sqrt (x1 ^ 2 + x2 ^ 2)

{- Funcion constante -}
constante :: Int -> Int
constante x = x

{- Funcion que retorna el signo -}
signo :: Int -> Int
signo x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

{- Funcion que retorna el número más grande -}
mayor :: Int -> Int -> Int
mayor x y
  | x > y = x
  | x < y = y
  | otherwise = x

{- Funcion que verifica si el numero es par -}
par :: Int -> Bool
par = even

{- Funcion que verifica si el numero no es par -}
impar :: Int -> Bool
impar = odd

{- Funcion que devuelve, dados b y c, la cantidad de soluciones para x^2 + bx + c -}
soluciones :: Int -> Int -> Int
soluciones b c
  | b ^ 2 - 4 * c < 0 = 2
  | b ^ 2 - 4 * c == 0 = 1
  | otherwise = 0

{- Funcion que devuelve el valor absoluto de un numero -}
absolute :: Int -> Int
absolute x
  | x < 0 = - x
  | otherwise = x

{- Funcion que devuelve el maximo entre el valor absoluto de dos numeros enteros -}
maxAbsolute :: Int -> Int -> Int
maxAbsolute x y
  | absolute x > absolute y = absolute x
  | absolute x < absolute y = absolute y
  | otherwise = absolute x

{- Funcion que devuelve el maximo entre 3 numeros enteros -}
max3 :: Int -> Int -> Int -> Int
max3 x y z
  | x > y && x > z = x
  | y > x && y > z = y
  | z > x && z > y = z
  | otherwise = x

{- Funcion que revisa si un número es 0 -}
esCero :: Int -> Bool
esCero x = x == 0

{- Funcion que revisa si alguno de 2 numeros es 0 -}
algunoEsCero :: Int -> Int -> Bool
algunoEsCero x y
  | x == 0 = True
  | y == 0 = True
  | otherwise = False

{- Funcion que verifica que ambos numeros son 0 -}
ambosCero :: Int -> Int -> Bool
ambosCero x y = x == 0 && y == 0

{- Funcion que verifica si un numero es multiplo de otro -}
multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0

{- Funcion que obtiene el digito de las unidades -}
digitoUnidades :: Int -> Int
digitoUnidades x
  | x > 10 = mod x (div x 10 * 10)
  | x > 0 = x
  | x < -10 = mod (- x) (div (- x) 10 * 10)
  | x < 0 = - x
  | otherwise = 0

{- Funcion que obtiene el digito de las decenas -}
digitoDecenas :: Int -> Int
digitoDecenas x
  | x > 100 = div (mod x (div x 100 * 100)) 10
  | x > 10 = div x 10
  | x < -100 = div (mod (- x) (div (- x) 100 * 100)) 10
  | x < -10 = - div (- x) 10
  | otherwise = 0