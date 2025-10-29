-- Map como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está asociada al valor en la misma posición, pero de la otra lista.
module Map2
(Map, emptyM, assocM, lookupM, deleteM, keys)
where
data Map k v = M [k] [v] deriving Show
{- INV. REP:
  * ambas listas deben tener la misma longitud
  * cada clave de la primera lista debe corresponder a cada valor de la segunda
  * las claves no deben estar repetidas en la primera lista 
  
  VÁLIDO:
  M [] []
  M [1,2] ["leo","elias"]

  INVÁLIDO:
  M [1,2] ["leo"]
  M [1,2,1] ["leo", "elias", "montiel"]
  -}

emptyM :: Map k v 
-- Propósito: devuelve un map vacío
emptyM = M [] []
-- O(1)

assocM :: Eq k => k -> v -> Map k v -> Map k v 
-- Propósito: agrega una asociación clave-valor al map.
assocM k v (M ks vs) = insertarM k v ks vs
-- siendo insertarM O(n) entonces esta función también.

insertarM :: Eq k => k -> v -> [k] -> [v] -> Map k v 
insertarM k v [] [] = M [k] [v]
insertarM k v (x:ks) (y:vs) =
    if k == x
        then M (k:ks) (v:vs)
        else agregarKV x y (insertarM k v ks vs)
{- siendo n las claves de la lista 1 y m los valores de la lista 2:
  * (==) --> O(1)
  * (:) --> O(1)
  * agregarKV --> O(1)
  * insertarM --> en el peor de los casos se hace la recursión por toda la lista de llaves y valores hasta que se agrega la clave y el valor, por eso es LINEAL. -}

agregarKV::Eq k => k -> v -> Map k v -> Map k v 
agregarKV k v (M ks vs) = M (k:ks) (v:vs)
-- solo se hacen dos cons, por eso es O(1)

lookupM :: Eq k => k-> Map k v-> Maybe v
-- Propósito: encuentra un valor dado una clave.
lookupM k (M ks vs) = buscarM k ks vs
-- siendo buscarM O(n) entonces esta función también.

buscarM::Eq k => k -> [k] -> [v] -> Maybe v 
buscarM _ [] [] = Nothing
buscarM k (x:ks) (y:vs) = 
    if k == x 
        then Just y
        else buscarM k ks vs
{- siendo n las claves de la lista 1 y m los valores de la lista 2:
  * (==) --> O(1)
  * Just -> O(1)
  * buscarM --> en el peor caso se hace la recursión por toda la lista de llaves y valores hasta encontrar la key buscada para arrojar su valor asociado, por eso es LINEAL. -}

deleteM :: Eq k => k-> Map k v-> Map k v
-- Propósito: borra una asociación dada una clave.
-- Precondición: la clave debe existir en el map.
deleteM x (M ks vs) = borrarM x ks vs
-- siendo borrarM O(n) entonces esta función también.

borrarM::Eq k => k -> [k] -> [v] -> Map k v 
-- Precondición: la clave debe existir en la lista 1.
borrarM _ [] [] = error "la clave buscada no existe en las asociaciones."
borrarM x (k:ks) (v:vs) =
  if x == k 
    then M ks vs 
    else agregarKV k v (borrarM x ks vs)
{- siendo n las claves de la lista 1 y m los valores de la lista 2:
  * (==) --> O(1)
  * agregarKV --> O(1)
  * borrarM --> en el peor de los casos se hace la recursión por toda la lista de llaves y valores hasta que se borra la clave y el valor en las listas, por eso es LINEAL. -}

keys :: Map k v-> [k]
-- Propósito: devuelve las claves del map.
keys (M ks _) = ks
-- O(1)
 
m0 = emptyM
m1 = assocM 1 "leo" m0
m2 = assocM 2 "elias" m1
m3 = assocM 1 "montiel" m2