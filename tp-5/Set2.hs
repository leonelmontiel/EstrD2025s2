module Set2
 (Set, emptyS, addS, belongs, sizeS,  removeS, removeElem, unionS, setToList)
where

data Set2 = S [a] deriving Show
{- ONSERVACIÓN:
    * xs puede tener elementos repetidos -}

emptyS :: Set a
-- Crea un conjunto vacío.
emptyS = S []
{- Costo CONSTANTE O(1) -}

addS :: Eq a => a-> Set a-> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs) = S (x:xs)
{- Solo se hace un Cons a la lista contenida en el Set, por ende es de costo O(1) -}

belongs :: Eq a => a-> Set a-> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs) = elem x xs
{- 'elem' en el peor de los casos recorre toda la lista xs para encontrar el elemento o no. Por ese motivo esta función tiene un costo LINEAL O(n) -}

sizeS :: Eq a => Set a-> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs) = length (sinRepetidos xs)
{- siendo n la cantidad de elementos en la lista interna del conjunto:
    * sinRepetidos --> O(n²). Es la operación dominante. Para asegurar que cada elemento es único, debe comparar cada elemento con los demás.
    * length --> O(n). Recorre la lista resultante (ya sin repetidos) una sola vez.
  Como el costo cuadrático de 'sinRepetidos' es el término dominante, la eficiencia total de la función es O(n²). -}

removeS :: Eq a => a-> Set a-> Set a
-- Borra un elemento del conjunto.
removeS x (S xs) = if elem x xs then S (removeElem x xs) else S xs
{- siendo n la cantidad de elementos en la lista interna del conjunto:
    * elem --> O(n). En el peor caso, debe recorrer toda la lista para encontrar el elemento.
    * removeElem --> O(n). Si se encuentra el elemento, se debe recorrer la lista nuevamente para construir la nueva versión sin él.
  Como las operaciones se ejecutan una después de la otra, sus costos se suman O(n) + O(n). La eficiencia total sigue siendo lineal, resultando en O(n). -}

removeElem :: Eq a => a -> [a] -> [a]
removeElem _ [] = []
removeElem x (y:ys) = if x == y then ys else y : removeElem x ys
{- tanto el '==' como el 'cons' son Constantes, pero, en el peor de los casos, se recorre toda la lista utilizando la recursión para encontrar el elemento a eliminar, entonces esta función resulta LINEAL (n) -}

unionS :: Eq a => Set a-> Set a-> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs) (S ys) = S (sinRepetidos (xs++ys))
{- siendo n el número total de elementos en la lista concatenada, N la longitud de xs y M la de ys, (n = N + M):
    * (++) --> O(n). Recorre las listas una vez.
    * sinRepetidos --> O(n²). Es la operación dominante.
Como el costo cuadrático de 'sinRepetidos' crece mucho más rápido que los costos lineales, determina la eficiencia total de la función, resultando en O(n²). -}

setToList :: Eq a => Set a-> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs) = sinRepetidos xs
{- * sinRepetidos --> O(n²). Es la operación dominante.
Como el costo cuadrático de 'sinRepetidos' crece, determina la eficiencia total de la función, resultando en O(n²). -}