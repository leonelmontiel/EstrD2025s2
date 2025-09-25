
{- Calculo de costos
  Especificar el costo operacional de las siguientes funciones: -}
head' :: [a]->a
head' (x:xs) = x
-- Costo CONSTANTE O(1): Solo toma un elemento de la lista

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
{- Costo CONSTANTE O(1): siempre realiza nueve sumas, sin importar si x es 10 o 10.000.000.
El número de pasos para llegar al resultado es constante.-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
{- Costo LINEAL O(n): siendo n el número ingresado por parámetro, la cantidad total de llamadas y multiplicaciones 
es directamente proporcional al valor de n. 
Si n se duplica, el número de pasos para resolver el problema también se duplica -}

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
-- Costo LINEAL O(n): siendo n la cantidad de elementos de lista ingresada, se suma 1 por cada uno de ellos n.

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
{- Costo CUADRÁTICO O(n^2): siendo n la cantidad de elementos de la lista ingresada y m el valor máximo de la lista.
La función factoriales recorre toda la lista lo que lo hace de costo LINEAL pero hay que tener en cuenta 
el costo de factorial que es también LINEAL. Por ende O(n*m) es O(n^2) lo que lo hace de costo CUADRÁTICO.-}

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
{- Costo LINEAL O(n): siendo n el valor a encontrar y m la cantidad de elemento de la lista, n tiene un costo CONSTANTE
al compararse con el primer elemento de la lista iterada pero m es LINEAL porque en el peor de los casos recorre todos
los elementos. Por ende O(1*n) es O(n).-}

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
  if pertenece x xs
    then sinRepetidos xs
    else x : sinRepetidos xs
{- Costo CUADRÁTICO O(n^2): siendo n la cantidad de elementos de la lista xs que se pasa por parámetro a pertenece 
y m la cantidad de elementos de la lista completa, n tiene un costo LINEAL al iterarse por cada elemento y
m también tiene un costo LINEAL por la recursividad en cada elemento, al hacer O(n*n) da como resulta O(n^2)
lo cual implica un costo CUADRÁTICO.

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
{- Costo LINEAL O(n): siendo n la cantidad de elementos de la primera lista ingresada, 
se itera por cada elemento para agregarlos a la segunda lista. Por ende el costo es LINEAL O(n).-}

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
{- Costo CUADRÁTICO O(n^2): siendo n la longitud total de todos los caracteres en las cadenas de la lista de entrada.
Si la lista contiene m cadenas con costo O(n) y cada cadena tiene una longitud promedio de k con costo O(n),
la complejidad de tiempo es aproximadamente O(m*k). 
Fijarme como redactar mejor esto -}




















