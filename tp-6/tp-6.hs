{-  Ejercicio 2
Implementar la función heapSort :: Ord a => [a]-> [a], que dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar. ¿Cuál es su costo?
OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente práctica -}

import PriorityQueue
import Map

heapSort::Ord a => [a] -> [a]
heapSort [] = []
heapSort xs =
    let pq = crearPQ xs
    in obtenerListaOrdenada pq
{- siendo n la cantidad de elementos de xs:
  * crearPQ --> O(n^2)
  * obtenerListaOrdenada --> O(n)
  entonces heapSort es O(n^2). -}

crearPQ::Ord a => [a] -> PriorityQueue a
crearPQ [] = emptyPQ
crearPQ (x:xs) = insertPQ x (crearPQ xs)
{- siendo n la cantidad de elementos de la lista:
  * insertPQ --> O(n)
  * emptyPQ --> O(1)
  * crearPQ --> como se hace un llamado a insertPQ por cada elemento de la lista, teniendo este un costo LINEAL y, a su vez, hacer recursión, entonces crearPQ es CUADRÁTICO. -}

obtenerListaOrdenada::Ord a => PriorityQueue a -> [a]
obtenerListaOrdenada pq =
    if not (isEmptyPQ pq)
        then findMinPQ pq : obtenerListaOrdenada (deleteMinPQ pq)
        else []
{- siendo n los elementos de la PQ:
  * isEmptyPQ --> O(1)
  * findMinPQ --> O(1)
  * deleteMinPQ --> O(1)
  * (:) --> O(1)
  * obtenerListaOrdenada --> se hace la recursion por cada n elementos hasta tranformar la PQ en una lista, por eso esta función es LINEAL.  -}

  ------------------
--Implementar como usuario del tipo abstracto Map las siguientes funciones:
valuesM :: Eq k => Map k v-> [Maybe v]
-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM map = let ks = keys map
  in getValues ks map
{- siendo n la cantidad de elementos del map: 
  * keys --> O(n) 
  * getValues --> O(n^2) 
  en esta función prima el costo CUADRÁTICO. -}

getValues::Eq k => [k] -> Map k v -> [Maybe v]
getValues [] _ = []
getValues (k:ks) map = (lookupM k map) : (getValues ks map)
{- siendo n la cantidad de elementos de la lista y m la del map:
  * (:) --> O(1)
  * lookupM --> O(m)
  * getValues --> se hace la recursión obligadamente para obtener con lookupM todos los values de las n keys, por eso esta función es CUADRÁTICA. -}

todasAsociadas :: Eq k => [k]-> Map k v-> Bool
-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] _ = False
todasAsociadas ks map =
  let kss = keys map
  in existenEn ks kss
{- siendo n la cantidad de elementos de la lista y m los del map:
  * keys --> O(m)
  * existenEn --> O(m^2)
  en esta función prima el costo CUADRÁTICO. -}

existenEn::Eq a => [a] -> [a] -> Bool
existenEn [] _ = True
existenEn (k:ks) kss =
  if elem k kss
    then existenEn ks kss
    else False
{- siendo n la cantidad de elementos de la primera lista y m los de la segunda:
  * elem --> O(m)
  * existeEn --> en el peor de los casos se hace la recursión n veces sobre los m, haciendo siempre el elem, por eso esta funcióne es CUADRÁTICA. -}

--listToMap :: Eq k => [(k, v)]-> Map k v
-- Propósito: convierte una lista de pares clave valor en un map.
--mapToList :: Eq k => Map k v-> [(k, v)]
-- Propósito: convierte un map en una lista de pares clave valor.
--agruparEq :: Eq k => [(k, v)]-> Map k [v]
-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
--incrementar :: Eq k => [k]-> Map k Int-> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
--mergeMaps:: Eq k => Map k v-> Map k v-> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada por la del primero.
-- Indicar los ordenes de complejidad en peor caso de cada función implementada, justi cando las respuestas.

m0 = emptyM
m1 = assocM 1 "leo" m0
m2 = assocM 2 "elias" m1
m3 = assocM 3 "leo" m2
m4 = assocM 1 "montiel" m3