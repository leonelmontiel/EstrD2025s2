import Set

{- Calculo de costos
  Especificar el costo operacional de las siguientes funciones: -}
head' :: [a]->a
head' (x:xs) = x
-- Costo CONSTANTE O(1): Solo toma el primer elemento de la lista sin importar la longitud

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
{- Costo CONSTANTE O(1): siempre realiza nueve sumas, sin importar si x es 10 o 10.000.000.
El número de pasos para llegar al resultado es constante.-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
{- Costo LINEAL O(n): siendo n el número ingresado por parámetro, la cantidad total de llamadas y multiplicaciones es directamente proporcional al valor de n. 
Si n se duplica, el número de pasos para resolver el problema también se duplica -}

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
-- Costo LINEAL O(n): siendo n la cantidad de elementos de lista ingresada, se suma 1 por cada uno de ellos.

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
{- Costo CUADRÁTICO O(n^2): siendo n la cantidad de elementos de la lista ingresada.
La función factoriales recorre toda la lista lo que lo hace de costo LINEAL pero hay que tener en cuenta el costo de factorial que es también LINEAL. Por ende O(n*n) es O(n^2) lo que lo hace de costo CUADRÁTICO.-}

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
{- Costo LINEAL O(n): siendo n el valor a encontrar y m la cantidad de elemento de la lista, n tiene un costo CONSTANTE al compararse con el primer elemento de la lista iterada pero m es LINEAL porque en el peor de los casos recorre todos los elementos. Por ende O(1*n) es O(n).-}

{- lo comento porque sino rompe porque más abajo me toca implementar una función con el mismo nombre
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
  if pertenece x xs
    then sinRepetidos xs
    else x : sinRepetidos xs
{- Costo CUADRÁTICO O(n^2): siendo n la cantidad de elementos de la lista que se pasa por parámetro, pertenece tiene un costo LINEAL al iterarse por cada elemento y sinRepetidos también tiene un costo LINEAL por la recursividad en cada elemento en cualquier caso del if, al hacer O(n*n) da como resulta O(n^2)
lo cual implica un costo CUADRÁTICO. -} -}

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
Un String es una lista de Char, y estas listas de Char están dentro de otra lista. El append tiene un costo LINEAL y concatenar también ya que itera por cada elemento de la lista. Por ende es O(n^2). -}

takeN::Int->[a]->[a]
takeN 0 xs = xs
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
{- El 'cons' es O(1) pero se hace según n veces, en el peor de los casos se recorre toda la lista xs, por ende takeN sería LINEAL O(n) -}

dropN :: Int-> [a]-> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
{- Según n veces se va descartando el último elemento de la lista xs, así que en el peor de los casos n es la longitud de la lista, lo que haría recorrer todos los elementos. Por eso esta función es LINEAL O(n). -}

partir :: Int-> [a]-> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
{- Tanto 'takeN' como 'dropN' son lineales, como son dos operaciones separadas: O(n) + O(n) da como resulta que 'partir' sea también LINEAL O(n). -}

minimo :: Ord a => [a]-> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
{- 'min x' tiene un costo CONSTANTE y 'minimo', al recorrer toda la lista para devolver el número más chico, es LINEAL, por ende O(1) + O(n) = O(n). Generando que tenga un costo LINEAL. -}

sacar :: Eq a => a-> [a]-> [a]
sacar n [] = []
sacar n (x:xs) =
  if n == x
  then xs
  else x : sacar n xs
{- Siendo N la cantidad de elementos de la lista xs, en el peor de los casos se evalúa cada uno sus elementos para comparar con n. El '==' es O(1) al igual que el 'cons', pero 'sacar' es O(N) por lo dicho anteriormente, por eso O(1) + O(1) + O(N) = O(N). Por eso esta función es LINEAL. -}

ordenar :: Ord a => [a]-> [a]
ordenar [] = []
ordenar xs =
  let m = minimo xs
  in m : ordenar (sacar m xs)
{- Siendo n la longitud de xs, 'minimo' tiene un costo lineal, 'sacar' también y lo mismo con 'ordenar' porque se aplicar para todos los elementos de la lista. Además de O(1) por el 'cons'. Pero al repetir las primeras dos funciones lineales en cada paso de la recursión, hace que 'ordenar' sea CUADRÁTICA O(n^2) -}

-----
-- FUNCIONES AUXILIARES
crearSet :: Set a
crearSet = emptyS

addElemsSet :: Eq a => [a] -> Set a -> Set a
addElemsSet [] s = s
addElemsSet (x:xs) s = addS x (addElemsSet xs s)

s1 = crearSet
s2 = addElemsSet [1,5,4] s1

-- 2) 2. Como usuario del tipo abstracto Set implementar las siguientes funciones:
losQuePertenecen :: Eq a => [a]-> Set a-> [a]
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s =
  if belongs x s
  then x : losQuePertenecen xs s
  else losQuePertenecen xs s                               
{- siendo n la cantidad de elementos de xs y m la cantidad de elementos de la lista en el Set:
    * belongs --> O(m) porque recorre la lista interna del Set.
    * cons    --> O(1)
  La función recorre los n elementos de xs, y en cada paso ejecuta belongs O(m) y eventualmente un cons O(1). Por lo tanto, el costo total en el peor caso es O(n·m). -}

sinRepetidos :: Eq a => [a]-> [a]
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos xs = setToList (setSinRepetidos xs emptyS)
{- * setToList       --> O(1)
   * setSinRepetidos --> O(n*m)
   siendo n la cantidad de elementos de xs y m el tamaño del conjunto; O(1) + O(n*m) da como resultado que 'sinRepetidos' es O(n*m) -}

setSinRepetidos :: Eq a => [a] -> Set a -> Set a
setSinRepetidos [] s = s
setSinRepetidos (x:xs) s = setSinRepetidos xs (addS x s)
{- * addS    --> O(m)
  siendo n la cantidad de elementos de la lista y m el tamaño del conjunto; por cada elemento se evalua en el Set si se debe agregar o no según la existencia, por eso esta funcion tiene un costo O(n*m) -}









