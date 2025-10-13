{-  Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de platos).  -}
module Stack
  (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where
data Stack a = S [a] Int deriving Show
--                xs   n
{- INV. REP:
   - debe seguir la regla LIFO (last in, first out) 
   - los últimos elementos agregados a la estructura deben ser los primeros en salir
   - n es la cantidad de elementos de xs
   - si xs es vacía, entonces n es 0 y viceversa
   
   VÁLIDO:
   * push 5 (S [1, 2] 2) = (S [5, 1, 2] 3)
   * pop (S [5, 1, 2] 3) = (S [1, 2] 2)
   * (S [5, 1, 2] 3)
   * (S [] 0)
   INVÁLIDO:
   * push 5 (S [1, 2] 2) = (S [1, 2, 5] 3) o (S [1, 5, 2] 3)
   * pop (S [5, 1, 2] 2) = (S [5, 1] 2) o (S [5, 2] 2)
   * isEmptyS (S [5, 1, 2] 3)
   * (S [5, 1, 2] 0)
   * (S [] 5)
    -}

emptyS :: Stack a
-- Crea una pila vacía.
emptyS = S [] 0
{- Solo construye un Stack vacía, por eso el costo es CONSTANTE. -}

isEmptyS :: Stack a-> Bool
-- Dada una pila indica si está vacía.
isEmptyS (S _ 0) = True
isEmptyS _ = False
{- Como solo chequea si la lista del Stack está vacía, su costo es CONSTANTE. -}

push :: a-> Stack a-> Stack a
-- Dados un elemento y una pila, agrega el elemento a la pila.
push x (S xs n) = S (x:xs) (n+1)
{- * cons --> O(1)
  como solo agrega ese elemento a la lista sin importar la cantidad de elementos, su costo es CONSTANTE. -}

top :: Stack a-> a
-- Dada un pila devuelve el elemento del tope de la pila.
-- Precondición: la pila debe contener elementos
top (S [] _) = error "la pila no tiene elementos"
top (S (x:_) _) = x 
{- como solo retorna el primer elemento de la pila, su costo es CONSTANTE. -}

pop :: Stack a-> Stack a
-- Dada una pila devuelve la pila sin el primer elemento.
-- Precondición: la pila debe contener elementos
pop (S [] _) = error "la pila no tiene elementos"
pop (S (_:xs) n) = S xs (n-1)
{- como solo retorna la cola de la lista, sin su primer elemento, su costo es CONSTANTE. -}

lenS :: Stack a-> Int
-- Dada la cantidad de elementos en la pila.
-- Costo: constante. Para que esto se de, se debe agregar un campo Int al data Stack que lleve el conteo de la cantidad de elementos de la lista. Porque sin eso, el costo de 'lenS' tendría que ser Lineal al recorrer cada uno de los elementos para contarlos.
lenS (S _ n) = n