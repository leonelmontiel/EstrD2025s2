module Set
 (Set, emptyS, addS)--, belongs, sizeS, removeS, unionS, setToList)
where
    
data Set a = S [a] Int deriving Show
{- INVARIANTES DE REPRESENTACIÓN: en T xs n
     * xs no tiene repetidos
     * si xs es vacía, n es 0
     * si xs NO es vacía, n es la longitud de xs -}

emptyS :: Set a
-- Crea un conjunto vacío.
emptyS = S [] 0
{- Costo CONSTANTE O(1) -}

addS :: Eq a => a-> Set a-> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs n) = if elem x xs then S xs n else S (x : xs) (n+1)
{- Tanto el cons como la suma son de costo CONSTANTE, por ende addS es O(1) -}

belongs :: Eq a => a-> Set a-> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs _) = elem x xs
{- 'elem' en el peor de los casos recorre toda la lista xs para encontrar el elemento o no. Por ese motivo esta función tiene un costo LINEAL O(n) -}

sizeS :: Eq a => Set a-> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S _ n) = n
{- Como solo retorna el Int que indica la cantidad de elementos de la lista, el costo de esta fución es CONSTANTE O(1) -}

removeS :: Eq a => a-> Set a-> Set a
-- Borra un elemento del conjunto.
removeS x (S xs n) = if elem x xs then S (removeElem x xs) (n-1) else S xs n
{- la resta es O(1), pero 'elem' y 'emoveElem' son Lineale, por ende esta función tiene un costo O(n)  -}

removeElem :: Eq a => a -> [a] -> [a]
removeElem _ [] = []
removeElem x (y:ys) = if x == y then ys else y : removeElem x ys
{- tanto el '==' como el 'cons' son Constantes, pero, en el peor de los casos, se recorre toda la lista utilizando la recursión para encontrar el elemento a eliminar, entonces esta función resulta LINEAL (n) -}

unionS :: Eq a => Set a-> Set a-> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS (S xs _) (S ys _) = let mixed = sinRepetidos (xs++ys)
    in S mixed (length mixed)
{- 'sinRepetidos'  -}

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs then xs else x : sinRepetidos xs

--setToList :: Eq a => Set a-> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.