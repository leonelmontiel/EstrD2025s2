-- 1.
  -- 1) Dada una lista de enteros devuelve la suma de todos sus elementos
sumatoria :: [Int]->Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns
-- Precondición: no tiene

  -- 2) Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud::[a]->Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs
-- Precondición: no tiene

  -- 3) Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int]->[Int]
sucesores [] = []
sucesores (n:ns) = n+1 : sucesores ns
-- Precondición: no tiene

  -- 4) Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool]->Bool
conjuncion [] = True
conjuncion (x:xs) = yTambien x (conjuncion xs)
-- Precondicion: no tiene

yTambien::Bool->Bool->Bool
yTambien True x = x
yTambien _ _ = False

  -- 5) Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool]->Bool
disyuncion [] = False
disyuncion (x:xs) = oBien x (disyuncion xs)
-- Precondicion: no tiene

oBien::Bool->Bool->Bool
oBien False x = x
oBien _ _ = True

  -- 6) Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]]->[a]
aplanar [] = []
aplanar (ls:lss) = ls ++ (aplanar lss)
-- Precondicion: no tiene

  -- 7) Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a-> [a]-> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys
-- Precondicion: no tiene

  -- 8)  Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a-> [a]-> Int
apariciones _ [] = 0
apariciones x (y:ys) = unoSi (x == y) + apariciones x ys
-- Precondicion: no tiene

unoSi::Bool->Int
unoSi x = if x then 1 else 0
-- Precondicion: no tiene

  -- 9) Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA::Int->[Int]->[Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = agregarSiEsMenor n x ++ losMenoresA n xs
-- Precondicion: no tiene

agregarSiEsMenor::Int->Int->[Int]
agregarSiEsMenor n n' = if n > n' then [n'] else []
-- Precondicion: no tiene

  -- 10) Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos
lasDeLongitudMayorA :: Int-> [[a]]-> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x:xs) = agregarSiLongitudEsMayorA n x ++ lasDeLongitudMayorA n xs
-- Precondicion: no tiene

agregarSiLongitudEsMayorA::Int->[a]->[[a]]
agregarSiLongitudEsMayorA n xs = if n < longitud xs then [xs] else []
-- Precondicion: no tiene