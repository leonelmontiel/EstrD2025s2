{- 1. Priority Queue (cola de prioridad)
Ejercicio 1
La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.  Implementarla usando listas, e indicando el costo de cada operación. -}

module PriorityQueue
(PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where
data PriorityQueue a = PQ [a] deriving Show
{- INV. REP:
  * los elementos deben estar enlistados con prioridad ascendente
  * el primer elemento debe ser el más chico de todos -}

emptyPQ :: PriorityQueue a
-- Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []
-- Costo O(1)

isEmptyPQ :: PriorityQueue a-> Bool
-- Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ []) = True
isEmptyPQ _ = False
-- Costo O(1)

insertPQ :: Ord a => a-> PriorityQueue a-> PriorityQueue a
-- Propósito: inserta un elemento en la priority queue.
insertPQ x (PQ ys) = PQ (insertarMin x ys)
-- Como insertarMin es O(n) entonces esta función también es LINEAL.

insertarMin :: Ord a => a -> [a] -> [a]
insertarMin x [] = [x]
insertarMin x (y:ys) = if x < y then x:(y:ys) else y : insertarMin x ys
{- siendo n la cantidad de elementos de la lista:
  * (<) --> O(1)
  * (:) --> O(1)
  * insertarMin --> se hace la recursión cada n elementos que no cumplan ser mayor que el elemento x a agregar, por eso es de orden LINEAL. -}

findMinPQ :: Ord a => PriorityQueue a-> a
{-Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
Precondición: parcial en caso de priority queue vacía.-}
findMinPQ (PQ []) = error "la Priority Queue no puede estar vacía"
findMinPQ (PQ (x:_)) = x
-- el costo es O(1) gracias al invariante de representación

deleteMinPQ :: Ord a => PriorityQueue a-> PriorityQueue a
{-Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
Precondición: parcial en caso de priority queue vacía-}
deleteMinPQ (PQ []) = error "la Priority Queue no puede estar vacía"
deleteMinPQ (PQ (_:xs)) = PQ xs
-- el costo es O(1) gracias al invariante de representación

pq0 = PQ []
pq1 = insertPQ 3 pq0
pq2 = insertPQ 5 pq1
pq3 = insertPQ 0 pq2
pq4 = insertPQ 3 pq3
pq5 = insertPQ 8 pq4