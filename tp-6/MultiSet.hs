{-
3. MultiSet (multiconjunto)
Ejercicio 6
UnMultiSet (multiconjunto) es un tipo abstracto de datos similar a un Set (conjunto). A diferencia del último, cada elemento posee una cantidad de apariciones, que llamaremos ocurrencias del elemento en el multiset. Su interfaz es la siguiente:
-}
module MultiSet
(MultiSet, emptyMS, addMS)--, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where
data MultiSet a = MS [(a, Int)] deriving Show
                    -- a --> elemento | Int --> apariciones

emptyMS :: MultiSet a
--Propósito: denota un multiconjunto vacío.
emptyMS = MS []
-- O(1)

addMS :: Ord a => a-> MultiSet a-> MultiSet a
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS x (MS eas) = MS (agregarMS x eas)
-- siendo n la cantidad de elementos del MS y como agregarMS es O(n) entonces esta función también es LINEAL.    

agregarMS::Ord a => a -> [(a,Int)] -> [(a,Int)]
agregarMS x [] = [(x,1)]
agregarMS x ((e,a) : eas) =
    if x == e 
        then ((e,(a+1)) : eas)
        else (e,a) : agregarMS x eas
{- siendo n la cantidad de pares en la lista:
  * (==) --> O(1)
  * (:) --> O(1)
  * agregarMS --> en el peor caso, se recorre toda la lista hasta encontrar o no el elemento a agregar o incrementar su aparición, por eso es LINEAL. -}

ocurrencesMS :: Ord a => a-> MultiSet a-> Int
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS x (MS eas) = aparicionesDe x eas
-- siendo n la cantidad de elementos del MS y como aparicionesDe es O(n) entonces esta función también es LINEAL.   

aparicionesDe::Ord a => a -> [(a,Int)] -> Int
aparicionesDe _ [] = 0
aparicionesDe x ((e,a) : eas) =
    if x == e
        then a 
        else aparicionesDe x eas
{- siendo n la cantidad de pares de la lista:
  * (==) --> O(1)
  * aparicionesDe --> en el peor caso, se recorre n veces hasta encontrar o no el elemento para retornar sus apariciones, por eso es LINEAL. -}

unionMS :: Ord a => MultiSet a-> MultiSet a-> MultiSet a -- (opcional)
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS (MS []) ms = ms
unionMS (MS ((e,a) : eas)) ms =
    if a > 0
        then addMS e (unionMS (MS ((e,(a-1)) : eas)) ms)
        else unionMS (MS eas) ms

--intersectionMS :: Ord a => MultiSet a-> MultiSet a-> MultiSet a (opcional)
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
--multiSetToList :: MultiSet a-> [(a, Int)]
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias

ms0 = emptyMS
ms1 = addMS "leo" ms0
ms2 = addMS "elias" ms1
ms3 = addMS "leo" ms2