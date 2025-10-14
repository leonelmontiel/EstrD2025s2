{- Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el primero en salir (como la cola de un banco).

Implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá que todas las operaciones sean constantes (aunque alguna/s de forma amortizada).
La estructura funciona de la siguiente manera. Llamemos a una de las listas fs (front stack) y
a la otra bs (back stack). Quitaremos elementos a través de fs y agregaremos a través de bs, pero todas las operaciones deben garantizar el siguiente invariante de representación: Si fs se encuentra vacía, entonces la cola se encuentra vacía -}
module Queue2
  (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where
data Queue a = Q [a] [a] deriving Show
--                fs  bs
{- INV. REP:
   * Si fs se encuentra vacía, entonces la cola se encuentra vacía -}

emptyQ :: Queue a
-- Crea una cola vacía.
emptyQ = Q [] []
{- solo crea una Queue con listas vacías, por eso es CONSTANTE. -}

isEmptyQ :: Queue a-> Bool
-- Dada una cola indica si la cola está vacía.
isEmptyQ (Q [] _) = True
isEmptyQ _ = False
{- solo chequea que fs sea vacío, teniendo en cuenta el Invariante, por eso esta función es CONSTANTE -}

enqueue :: a-> Queue a-> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q [] []) = Q [x] []
enqueue x (Q fs bs) = Q fs (x:bs)

firstQ :: Queue a-> a
-- Dada una cola devuelve el primer elemento de la cola.
firstQ (Q [] _) = error "la cola debe tener elementos"
firstQ (Q (f:_) _) = f

dequeue :: Queue a-> Queue a
-- Dada una cola la devuelve sin su primer elemento.
dequeue (Q [] []) = error "cola vacía"
dequeue (Q (_:fs) bs) = balancearQueue fs bs

-- AUXILIAR
balancearQueue :: [a] -> [a] -> Queue a 
balancearQueue [] bs = Q (reverse bs) []
balancearQueue fs bs = Q fs bs