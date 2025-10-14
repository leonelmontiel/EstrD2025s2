{-  Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de platos).  -}
module Stack2
  (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where
data Stack a = S [a] deriving Show

{- INV. REP:
   - debe seguir la regla LIFO (last in, first out) 
   - los últimos elementos agregados a la estructura deben ser los primeros en salir
   
   VÁLIDO:
   * push 5 (S [1, 2]) = (S [5, 1, 2])
   * pop (S [5, 1, 2]) = (S [1, 2])
   INVÁLIDO:
   * push 5 (S [1, 2]) = (S [1, 2, 5] 3) o (S [1, 5, 2])
   * pop (S [5, 1, 2]) = (S [5, 1] 2) o (S [5, 2])
    -}

emptyS :: Stack a
-- Crea una pila vacía.
emptyS = S []
{- como solo construye un Stack con lista vacía, el costo es CONSTANTE. -}

isEmptyS :: Stack a-> Bool
-- Dada una pila indica si está vacía.
isEmptyS (S []) = True
isEmptyS _ = False
{- como solo hace PM sobre la lista, el costo es CONSTANTE. -}

push :: a-> Stack a-> Stack a
-- Dados un elemento y una pila, agrega el elemento a la pila.
push x (S xs) = S (x:xs)
{- solo hace un 'cons' a una lista sin importar la cantidad de elementos, por eso es CONSTANTE. -}

top :: Stack a-> a
-- Dada un pila devuelve el elemento del tope de la pila.
-- Precondición: la pila no puede estar vacía
top (S []) = error "la pila no puede estar vacía"
top (S (x:_)) = x
{- como solo retorna el primer elemento de la pila, el costo es CONSTANTE. -}

pop :: Stack a-> Stack a
-- Dada una pila devuelve la pila sin el primer elemento.
pop (S []) = error "la pila no puede estar vacía"
pop (S (_:xs)) = S xs
{- como solo retorna un Stack con la cola deel stack original, el costo es CONSTANTE. -}

lenS :: Stack a-> Int
-- Dada la cantidad de elementos en la pila.
lenS (S xs) = length xs
{- siendo n la cantidad de elementos de xs
  * length --> O(n)
  cuenta cada uno de los elementos para retornar la longitud de la pila, por eso el costo es LINEAL. -}