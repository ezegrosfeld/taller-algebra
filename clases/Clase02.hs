{- Funciones sin tipo asignado -}
identidad :: t -> t
identidad x = x

primero :: tx -> ty -> tx
primero x y = x

segundo :: tx -> ty -> ty
segundo x y = y

constante5 :: tx -> ty -> tz -> Int
constante5 x y z = 5

mismoTipo :: t -> t -> Bool
mismoTipo x y = True

{- CLASES DE TIPOS: se describen como restricciones sobre variables de tipos -}
{- Uso: (Clase v) => v -}
{-
    -> Ord:        Si se hace una comparación (<,>, <=, >=,) se usa **Ord** (Poseen orden)
    -> Eq:         Si se hace una igualdad (==, !=) se usa **Eq**
    -> Num:        Si se usan números se usa **Num** (Poseen números)
    -> Fractional: Si se hace una división se usa **Fractional**
    -> Floating:    Si se usan doubles/float o se hacen exponenciales o raices se usa **Floating*
-}

triple :: (Num t) => t -> t
triple x = x * 3

maximo :: (Ord t) => t -> t -> t
maximo x y = if x > y then x else y

distintos :: (Eq t) => t -> t -> Bool
distintos x y = x /= y

-- Al usar dos clases sobre una misma variable, se especifica que debe cumplir ambas (ser numero y ordenado)
cantidadDeSoluciones :: (Num t, Ord t) => t -> t -> Int
cantidadDeSoluciones b c
  | d > 0 = 2
  | d == 0 = 1
  | otherwise = 0
  where
    d = b ^ 2 - 4 * c

{- TUPLAS -}
{-
    Funciones de acceso a tuplas:
    -> fst :  Devuelve el primer elemento de la tupla
    -> snd :  Devuelve el segundo elemento de la tupla
 -}

{- Suma vectorial -}
suma :: (Float, Float) -> (Float, Float) -> (Float, Float)
suma v w = (fst v + fst w, snd v + snd w)

-- Es lo mismo que:
--suma (vx, vy) (wx, wy) = (vx + wx, vy + wy)

{- Pattern matching sobre tuplas y constructores -}

-- esOrigen verifica que el vector sea el orgien de coordenadas
esOrigen :: (Float, Float) -> Bool
esOrigen (0, 0) = True
esOrigen (_, _) = False

-- angulo45 verifica que el vector tenga un angulo de 45 grados con el eje X
angulo45 :: (Float, Float) -> Bool
angulo45 (x, y) = x == y

{- Parametros vs Tuplas -}

-- Norma vectorial
-- Con parametros:
normaVectorialP :: Float -> Float -> Float
normaVectorialP x y = sqrt (x ^ 2 + y ^ 2)

-- Con tuplas:
normaVectorialT :: (Float, Float) -> Float
normaVectorialT (x, y) = sqrt (x ^ 2 + y ^ 2)

-- Suma de las normales de 2 vectores
-- Usando la norma vectorial con parametros
sumaNormasP :: (Float, Float) -> (Float, Float) -> Float
sumaNormasP v w = normaVectorialP (fst s) (snd s) -- uncurry normaVectorialP s (es lo mismo)
  where
    s = suma v w

-- Usando la norma vectorial con tuplas
sumaNormasT :: (Float, Float) -> (Float, Float) -> Float
sumaNormasT v w = normaVectorialT (suma v w) --  CONVIENE USAR TUPLAS EN ESTE CASO

{- --------------------- -}
{-  EJERCICIOS DEL ZOOM  -}
{- --------------------- -}

{-  1.
    estanRelacionados es una funcion que dados dos numeros reales, decide si estan
    relacionados considerando la relacion de equivalencia en R cuyas clases de equivalencia son:
    (−∞, 3], (3, 7] y (7, ∞)
-}
estanRelacionados :: (Num t, Ord t) => t -> t -> Bool
estanRelacionados x y
  | x <= 3 && y <= 3 = True
  | x > 7 && y > 7 = True
  | (x > 3 && x <= 7) && (y > 3 && y <= 7) = True
  | otherwise = False

{-  2.
    prodInt: calcula el producto interno entre dos vectores de R2.
 -}
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt v w = (fst v * fst w) + (snd v * snd w)

{-  3.
    todoMenor: dados dos vectores de R2, decide si es cierto que cada coordenada del primer
    vector es menor a la coordenada correspondiente del segundo vector.
-}
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor v w = fst v < fst w && snd v < snd w

{-  4.
    distanciaPuntos: calcula la distancia entre dos puntos de R2.
 -}
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos v w = normaVectorialT (fst v - fst w, snd v - snd w)

{-  5.
    sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
 -}
sumaTerna :: Int -> Int -> Int -> Int
sumaTerna x y z = x + y + z

{-  6.
    posicPrimerPar: dada una terna de enteros, devuelve la posicion del primer numero par si
    es que hay alguno, y devuelve 4 si son todos impares.

 -}
par :: Integral a => a -> Bool
par x = x `mod` 2 == 0

posicPrimerPar :: Int -> Int -> Int -> Int
posicPrimerPar x y z
  | par x = 1
  | par y = 2
  | par z = 3
  | otherwise = 4

{-  7.
    crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por
    separado (debe funcionar para elementos de cualquier tipo).
-}
crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

{-  8.
    invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como par´ametro
    (debe funcionar para elementos de cualquier tipo).
-}
invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)

{- EJERCICIOS CLASES ZOOM -}

{-  1.
    Escribir una funcion que dados un numero real a y un vector x 2 R2, devuelva el producto por
    escalar a · x.
-}
productoEscalar :: Float -> (Float, Float) -> (Float, Float)
productoEscalar a x = (a * (fst x), a * (snd x))

{-  2.
    Escribir una funcion que dados 2 vectores en R3, devuelva el producto vectorial (o producto
    cruz) entre ellos.
-}
productoVectorial :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
productoVectorial (x1, y1, z1) (x2, y2, z2) = ((y1 * z2) - (z1 * y2), (- x1 * z2) + (x2 * z1), (x1 * y2) - (y1 * x2))

{-  3.
    Ana y Beto juegan en equipo a embocar bolitas en un aro, por cada embocada suman 1 punto,
    ganan si suman entre los dos al menos 20 puntos.
    Escribir una funcion que dadas las cantidades de bolitas embocadas por cada jugador, devuelva
    la cantidad total de bolitas embocadas y verdadero en caso de que hayan ganado, y falso en caso
    contrario.
-}
bolitas :: Int -> Int -> (Int, Bool)
bolitas x y = (s, s >= 20)
  where
    s = x + y