{-
Implementar el tipo abstracto MultiSet2 utilizando como representación un Map. Indicar los ordenes de complejidad en peor caso de cada función de la interfaz, justificando las respuestas.
-}

module MultiSet2
(MultiSet2, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import Map

data MultiSet2 a = MS (Map a Int) deriving Show
                      --k: elemento --v: apariciones

emptyMS :: MultiSet2 a
--Propósito: denota un multiconjunto vacío.
emptyMS = MS (emptyM)
-- O(1)

addMS :: Ord a => a-> MultiSet2 a-> MultiSet2 a
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS e (MS m) = MS (insertarMS e m)
-- solo se llama a insertarMS que es LINEAL.

insertarMS :: Ord a => a -> Map a Int -> Map a Int 
insertarMS e m =
    case lookupM e m of
        Nothing -> assocM e 1 m
        Just v -> assocM e (v+1) m
{- siendo n los elementos del map:
  * lookupM --> O(n)
  * assocM --> O(n)
  * (+) --> O(1)
  se busca el elemento como key en el map, si la encuentra o no de todas formas retorna un valor y se hace un assocM de esa key nuevamente en el map. Por eso al hacer O(n) + O(n) + O(1) da como resultado que esta función también es LINEAL. -}

ocurrencesMS :: Ord a => a-> MultiSet2 a-> Int
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS e (MS m) = 
    case lookupM e m of
        Nothing -> 0
        Just v -> v
{- siendo n los elementos del map:
  * lookupM --> O(n)
  luego de la búsqueda de la key en el map, retorna las apariciones, por eso es LINEAL. -}

unionMS :: Ord a => MultiSet2 a-> MultiSet2 a-> MultiSet2 a -- (opcional)
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS (MS m1) (MS m2) = MS (insertarKeys m1 m2)

insertarKeys::Ord a => Map a Int -> Map a Int -> Map a Int
insertarKeys m1 m2 = let ks1 = keys m1
                         vs1 = valuesM ks1 m1
    in unirElems ks1 vs1 m2

unirElems:: Ord a => [a] -> [Int] -> Map a Int -> Map a Int
-- Precondición: ambas listas deben tener la misma longitud
unirElems [] _ m = m
unirElems (e:es) (v:vs) m =
  case lookupM e m of
    Nothing -> unirElems es vs (assocM e v m)
    Just v' -> unirElems es vs (assocM e (v+v') m)

valuesM:: Ord a => [a] -> Map a Int -> [Int]
valuesM [] m = []
valuesM (e:es) m =
  case lookupM e m of
    Nothing -> valuesM es m
    Just v -> v : (valuesM es m)


intersectionMS :: Ord a => MultiSet2 a-> MultiSet2 a-> MultiSet2 a -- (opcional)
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
intersectionMS (MS m1) (MS m2) = MS (intersectarMS m1 m2)

intersectarMS::Ord a => Map a Int -> Map a Int -> Map a Int 
intersectarMS m1 m2 = let ks1 = keys m1 
                          vs1 = valuesM ks1 m1 
  in intersectarCon ks1 vs1 m2

intersectarCon::Ord a => [a] -> [Int] -> Map a Int -> Map a Int 
intersectarCon [] _ m = emptyM
intersectarCon (e:es) (v:vs) m =
  case lookupM e m of
    Nothing -> intersectarCon es vs m
    Just v' -> assocM e (min v v') (intersectarCon es vs m)

multiSetToList :: Ord a => MultiSet2 a-> [(a, Int)]
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
multiSetToList (MS m) = let ks = keys m
  in setToList ks (valuesM ks m)

setToList:: Ord a => [a] -> [Int] -> [(a,Int)]
setToList [] _ = []
setToList (e:es) (v:vs) = (e,v) : (setToList es vs)

ms0 = emptyMS
ms1 = addMS "leo" ms0
ms2 = addMS "montiel" ms1
ms3 = addMS "leo" ms2