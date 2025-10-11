{-Queue (cola)
1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el final de la lista y desencolarse por delante.
Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el primero en salir (como la cola de un banco). Su interfaz es la siguiente:
-}
module Queue
  (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where
data Queue a = Q [a] deriving Show
{- INV. REP:
    * debe ser un FIFO: el primer elemento que entra, es el primero que sale,
    * los elementos deben encolarse por el final de la lista y desencolarse por delante.

VÁLIDO:
enqueue 3 (Q [1, 5])  = (Q [1, 5, 3])
dequeue (Q [1, 5, 3]) = (Q [5, 3])
INVÁLIDO:
enqueue 3 (Q [1, 5])  = (Q [3, 1, 5]) o (Q [1, 3, 5])
dequeue (Q [1, 5, 3]) = (Q [1, 5]) o (Q [1, 3])
-}

emptyQ :: Queue a
-- Crea una cola vacía.
emptyQ = Q []
{- como solo retorna el constructor de la cola, esto es un costo CONSTANTE -}

isEmptyQ :: Queue a-> Bool
-- Dada una cola indica si la cola está vacía.
isEmptyQ (Q []) = True
isEmptyQ _ = False
{- solo abre el dato para conocer los elementos de la cola, por eso es CONSTANTE -}

--enqueue :: a-> Queue a-> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
--enqueue x (Q xs) = Q (xs++[x])
{- siendo n la cantidad de elementos de xs
   * cons --> O(1)
   * (++) --> O(n)
  predomina el costo mayor que es el LINEAL -}

--firstQ :: Queue a-> a
-- Dada una cola devuelve el primer elemento de la cola.
-- Precondicion: debe existir al menos un elemento en la cola.
--firstQ (Q []) = error "la cola no tiene elementos"
--firstQ (Q (x:xs)) = x
{- se abre la lista para conocer y retornar el primer elemento, por eso, esta función es CONSTANTE O(1) -}

--dequeue :: Queue a-> Queue a
-- Dada una cola la devuelve sin su primer elemento.
-- Precondicion: debe existir al menos un elemento en la cola.
--dequeue (Q []) = error "la cola no tiene elementos"
--dequeue (Q (x:xs)) = Q xs
{- solo abre la queue para conocer si tiene elementos o no, y si se da el caso retorna la cola sin su primer elemento, por eso esta función es CONSTANTE. -}

{-  2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare la eficiencia entre ambas implementaciones.-}
{- INV. REP:
    * debe ser un FIFO: el primer elemento que entra, es el primero que sale,
    * los elementos deben encolarse por el frente de la lista y desencolarse por detrás.

VÁLIDO:
enqueue 3 (Q [5, 1])  = (Q [3, 5, 1])
dequeue (Q [3, 5, 1]) = (Q [3, 5])
INVÁLIDO:
enqueue 3 (Q [5, 1])  = (Q [5, 1, 3]) o (Q [5, 3, 1])
dequeue (Q [3, 5, 1]) = (Q [5, 1]) o (Q [3, 1])
-}
enqueue :: a-> Queue a-> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (x:xs)
{- sea n la cantidad de elementos en xs
   * cons --> O(1)
   al insertar solo una vez el elemento a la lista, la función es CONSTANTE. -}

firstQ :: Queue a-> a
-- Dada una cola devuelve el primer elemento de la cola.
-- Precondicion: debe existir al menos un elemento en la cola.
firstQ (Q []) = error "la cola no tiene elementos"
firstQ (Q (x:xs)) = if null xs then x else firstQ (Q xs)

dequeue :: Queue a-> Queue a
-- Dada una cola la devuelve sin su primer elemento.
-- Precondicion: debe existir al menos un elemento en la cola.
dequeue (Q []) = error "la cola está vacía"
dequeue (Q xs) = Q (sinUltimo xs)
{- sea n la cantidad de elementos de xs
   * sinUltimo --> O(n)
   como predomina O(n), esta función también es LINEAL. -}

sinUltimo :: [a] -> [a]
-- Dada una lista la devuelve sin su primer elemento.
-- Precondicion: debe existir al menos un elemento en la lista.
sinUltimo [] = error "la lista está vacía"
sinUltimo (x:xs) = if null xs then xs else x : sinUltimo xs
{- sea n la cantidad de elementos de xs
   * null --> O(1)
   * cons --> O(1)
   * sinUltimo --> O(n)
   al realizar recursión por todo el resto de la lista cada vez, esta función es LINEAL -}