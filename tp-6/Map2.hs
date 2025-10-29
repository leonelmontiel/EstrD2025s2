-- Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está asociada al valor en la misma posición, pero de la otra lista.
module Map2
(Map, emptyM, assocM, lookupM)--, deleteM, keys)
where
data Map k v = M [k] [v] deriving Show
{- INV. REP:
  * ambas listas deben tener la misma longitud
  * cada llave de la primera lista debe corresponder a cada clave de la segunda
  * las claves no deben estar repetidas en la primera lista -}

emptyM :: Map k v 
emptyM = M [] []

assocM :: Eq k => k -> v -> Map k v -> Map k v 
assocM k v (M ks vs) = insertarM k v ks vs

insertarM :: Eq k => k -> v -> [k] -> [v] -> Map k v 
insertarM k v [] [] = agregarKV k v emptyM
insertarM k v (x:ks) (y:vs) =
    if k == x
        then M (k:ks) (v:vs)
        else agregarKV x y (insertarM k v ks vs)

agregarKV::Eq k => k -> v -> Map k v -> Map k v 
agregarKV k v (M ks vs) = M (k:ks) (v:vs)

lookupM :: Eq k => k-> Map k v-> Maybe v
lookupM k (M ks vs) = buscarM k ks vs

buscarM::Eq k => k -> [k] -> [v] -> Maybe v 
buscarM _ [] [] = Nothing
buscarM k (x:ks) (y:vs) = 
    if k == x 
        then Just y
        else buscarM k ks vs

m0 = emptyM
m1 = assocM 1 "leo" m0
m2 = assocM 2 "elias" m1
m3 = assocM 1 "montiel" m2