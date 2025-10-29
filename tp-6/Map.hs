{- Ejercicio 3
La interfaz del tipo abstracto Map es la siguiente:-}
module Map

(Map, emptyM, assocM, lookupM, deleteM, keys, assocM')
where
data Map k v = M [(k, v)] deriving Show
{- INV. REP:
  * no existen claves repetidas
  
  VÁLIDO:
  M [(1, "leo"), (2, "elias"), (3, "montiel")]

  INVáLIDO:
  M [(1, "leo"), (2, "elias"), (1, "montiel")] -}

emptyM::Map k v
-- Propósito: devuelve un map vacío
emptyM = M []
-- costo O(1)

assocM :: Eq k => k-> v-> Map k v-> Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM k v (M xs) = M (insertarM k v xs)
-- siendo insertarM O(n) entonces esta función también es LINEAL.

insertarM::Eq k => k -> v -> [(k, v)] -> [(k, v)]
insertarM k v [] = [(k, v)]
insertarM k v ((x, y) : xs) =
    if k == x 
        then (k, v) : xs
        else (x, y) : insertarM k v xs
{- siendo n la cantidad de elementos de la lista de pares:
  * (==) --> O(1)
  * (:) --> O(1)
  * insertarM --> en el peor de los casos se hace la recursión hasta el último elemento para agregar el par, por eso es LINEAL. -}

lookupM :: Eq k => k-> Map k v-> Maybe v
-- Propósito: encuentra un valor dado una clave.
lookupM k (M xs) = buscarM k xs
-- siendo buscarM O(n), esta función también es LINEAL.

buscarM::Eq k => k -> [(k,v)] -> Maybe v 
buscarM _ [] = Nothing
buscarM k ((x,y) : xs) =
    if k == x
        then Just y
        else buscarM k xs
{- siendo n la cantidad de elementos pares en la lista:
  * (==) --> O(1)
  * Just --> O(1)
  * buscarM --> en el peor de los casos hace recursión hasta comparar con la última key de la lista, para saber si existe la buscada, por eso es LINEAL. -}

deleteM :: Eq k => k-> Map k v-> Map k v
-- Propósito: borra una asociación dada una clave.
deleteM k (M xs) = M (borrarM k xs)
-- siendo borrarM O(n), entonces esta función también es LINEAL.

borrarM::Eq k => k -> [(k,v)] -> [(k,v)]
borrarM _ [] = []
borrarM k ((x,y) : xs) = 
    if k == x
        then xs
        else (x,y) : borrarM k xs
{- siendo n la cantidad de pares de la lista:
  * (==) --> O(1)
  * (:) --> O(1)
  * borrarM --> en el peor de los casos compara hasta la última key para intentar borrarla, por eso es LINEAL. -}

keys :: Map k v-> [k]
-- Propósito: devuelve las claves del map.
keys (M xs) = llavesM xs
-- siendo llavesM O(n), entonces esta función también es LINEAL.

llavesM::[(k,v)] -> [k]
llavesM [] = []
llavesM ((k,_) : kvs) = k : llavesM kvs
{- siendo n la cantidad de pares de la lista:
  * (:) --> O(1)
  * llavesM --> obligadamente tiene que recorrer cada par para extraer la key, por eso es LINEAL. -}

m0 = emptyM
m1 = assocM 1 "leo" m0
m2 = assocM 2 "elias" m1
m3 = assocM 3 "leo" m2
m4 = assocM 1 "montiel" m3

----------------------
-- Map como una lista de pares-clave valor con claves repetidas
assocM' :: Eq k => k-> v-> Map k v-> Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM' k v (M xs) = M ((k,v):xs)

m1' = assocM' 1 "leo" m0
m2' = assocM' 2 "elias" m1
m3' = assocM' 3 "leo" m2
m4' = assocM' 1 "montiel" m3