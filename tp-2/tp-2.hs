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

  -- 11) Dados una lista y un elemento, devuelve una lista con ese elemento agregado al nal de la lista.
agregarAlFinal::[a]->a->[a]
agregarAlFinal [] x = [x]
agregarAlFinal (y:ys) x = y : agregarAlFinal ys x
-- Precondicion: no tiene

  -- 12) Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. De nida en Haskell como (++).
agregar::[a]->[a]->[a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys
-- Precondicion: no tiene

  -- 13) Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa::[a]->[a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x
-- Precondicion: no tiene

  -- 14) Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente tienen la misma longitud.
zipMaximos::[Int]->[Int]->[Int]
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys

maxDelPar::(Int, Int)->Int
maxDelPar (n,n') = if n>n' then n else n'
-- del tp1

  -- 15) Dada una lista devuelve el mínimo
elMinimo::Ord a=>[a]->a
elMinimo [] = error "La lista no puede estar vacía"
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

-- 2. De na las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:

  -- 1) Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial::Int->Int
factorial 0 = 1
factorial n = if n>0
  then n * factorial (n-1)
  else error "el número no debe ser negativo"
-- Precondicion: n no debe ser negativo
-- El tipo Int en Haskell tiene un rango limitado (normalmente de -2³¹ a 2³¹-1 en sistemas de 32 bits). El factorial de 56 es un número enorme, por eso lo envuelve en 0.
