-- f :: [a] -> b
-- f []     = … 
-- f (x:xs) = … x … f xs …

tomarHasta :: Int -> [a] -> [a]
-- PRECOND: i >= 0
--
-- DECIDO usar recursión en la estructura del Int. O sea,
--  tomarHasta 0 ys = … ys …
--  tomarHasta n ys = … n … tomarHasta (n-1) ¿ys? …
-- Luego, trato de resolver el caso recursivo. 
-- Para eso me doy cuenta que NECESITO OTRA recursión en la estructura la [a]
--  tomarHasta 0 ys     = … 
--  tomarHasta n []     = … n … 
--  tomarHasta n (x:xs) = … n … tomarHasta (n-1) xs …
-- Ahora puedo resolver el caso recursivo. 
-- ME PREGUNTO:
--    ¿Qué tipo tiene el resultado de la recursión?
--    ¿Qué cosas hay en ella?
--    ¿Qué le falta a eso para ser el resultado que yo preciso?
--    ¿Cómo le agrego eso que falta?
--  tomarHasta 0 ys     = … 
--  tomarHasta n []     = … n … 
--  tomarHasta n (x:xs) = x : tomarHasta (n-1) xs
-- Por último RESUELVO los casos base
--
tomarHasta 0 _      = []
tomarHasta _ []     = [] 
tomarHasta n (x:xs) = x : tomarHasta (n-1) xs

tomarDesde :: Int -> [a] -> [a]
-- PRECOND: i >= 0
tomarDesde 0 ys     = ys
tomarDesde n []     = []
tomarDesde n (_:xs) = tomarDesde (n-1) xs

tomarEntre :: Int -> Int -> [a] -> [a]
-- PRECOND: i <= j
tomarEntre i j xs = tomarHasta (j-i+1) (tomarDesde i xs)

tomarEntre' :: Int -> Int -> [a] -> [a]
tomarEntre' i j xs = tomarDesde i (tomarHasta (j+1) xs)
                   -- tomarHasta toma HASTA n elementos, o sea, hasta la posición (j-1)
                   -- para tomar hasta la posición j, tengo que tomar (j+1)

apariciones :: Eq a => [ a ] -> [ (a,Int) ]
apariciones []     = []
apariciones (x:xs) = agregar x (apariciones xs)

agregar :: Eq a => a -> [ (a,Int) ] -> [ (a,Int) ]
agregar x []          = (x,1) : []
agregar x ((y,n):yns) = if x==y
                         then (y,n+1) : yns
                         else (y,n)   : agregar x yns

type Index = Int 

indexar :: [a] -> [(Index,a)]
indexar []     = []
indexar (x:xs) = (0,x) : aumentar (indexar xs)

aumentar :: [(Index,a)] -> [(Index,a)]
-- aumenta todos los índices en uno
aumentar []          = []
aumentar ((i,x):ixs) = (i+1,x) : aumentar ixs

indexar' xs = indexarDesde 0 xs

indexarDesde :: Int -> [a] -> [(Int,a)]
indexarDesde k []     = []
indexarDesde k (x:xs) = (k,x) : indexarDesde (k+1) xs


ordenar :: Ord a => [a] -> [a]
ordenar []     = []
ordenar (x:xs) = insert x (ordenar xs)

insert :: Ord a => a -> [a] -> [a]
-- PRECOND: la lista dada está ordenada
insert x []     = x : []
insert x (y:ys) = if x <= y
                   then x : y : ys
                   else y : insert x ys

prefijosPropios :: [a] -> [[a]]
prefijosPropios []     = []
prefijosPropios (x:xs) = [x] : consACada x (prefijosPropios xs)

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss

-- =========================================
-- =========================================
data Persona = P String Int  String 
              -- Nombre Edad DNI

edad (P _ e _) = e

hayMayorDeEdad :: [ Persona ] -> Bool
hayMayorDeEdad []     = False
hayMayorDeEdad (p:ps) = esMayorDeEdad p || hayMayorDeEdad ps

esMayorDeEdad :: Persona -> Bool
esMayorDeEdad p = edad p >= 18

hayMayorDeEdadEn' :: Int -> [Persona] -> Bool
-- Por recursión sobre Int, y en CADA caso, recursión sobre la lista
hayMayorDeEdadEn' 0 []     = False
hayMayorDeEdadEn' 0 (p:_)  = esMayorDeEdad p
hayMayorDeEdadEn' n []     = False
hayMayorDeEdadEn' n (_:ps) = hayMayorDeEdadEn' (n-1) ps

hayMayorDeEdadEn :: Int -> [Persona] -> Bool
-- Por recursión en la lista, en el caso recursivo, recursión sobre el número
hayMayorDeEdadEn _ []     = False
hayMayorDeEdadEn 0 (p:_)  = esMayorDeEdad p
hayMayorDeEdadEn n (_:ps) = hayMayorDeEdadEn (n-1) ps

cantidadHastaMayorDeEdad :: [Persona] -> Int
-- PRECOND: hay al menos un mayor de edad
cantidadHastaMayorDeEdad []     = error "Debe haber alguien mayor de edad"
cantidadHastaMayorDeEdad (p:ps) = 
    if esMayorDeEdad p then 0
                       else 1 + cantidadHastaMayorDeEdad ps

sumaDeEdadesEntre :: Int -> Int -> [Persona] -> Int
sumaDeEdadesEntre i j ps = sumarEdades (tomarEntre i j ps)

sumarEdades []     = 0
sumarEdades (p:ps) = edad p + sumarEdades ps

-- =========================================
-- =========================================
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

-- f :: Tree a -> b
-- f EmptyT          = …
-- f (NodeT x t1 t2) = … x … f t1 … f t2 … 

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = [] 
levelN 0 (NodeT x _  _ ) = x : []
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []       yss      = yss
juntarNiveles xss      []       = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

listPerLevelMenosFeliz :: Tree a -> [[a]]
listPerLevelMenosFeliz t = listPerLevelHasta (altura t) t

listPerLevelHasta :: Int -> Tree a -> [[a]]
listPerLevelHasta 0 t = [ levelN 0 t ]
listPerLevelHasta n t = listPerLevelHasta (n-1) t ++ [ levelN n t ]

altura EmptyT          = 0
altura (NodeT _ t1 t2) = 1 + max (altura t1) (altura t2)

todosLosCaminos2 :: Tree a -> [[a]]
todosLosCaminos2 EmptyT          = []
todosLosCaminos2 (NodeT x t1 t2) = [x] :
                                   consACada x (todosLosCaminos2 t1)
                                ++ consACada x (todosLosCaminos2 t2)

data Opcion   = Izq | Der
              deriving Show
type Posicion = [ Opcion ]

elementoEn :: Posicion -> Tree a -> a
-- PRECOND: la posición es válida
elementoEn _      EmptyT          = error "No existe esa posicion"
elementoEn []     (NodeT x _  _)  = x
elementoEn (d:ds) (NodeT _ t1 t2) = 
    if (esIzq d) then elementoEn ds t1
                 else elementoEn ds t2

esIzq Izq = True
esIzq _   = False

aca :: Posicion
aca = []

posicionesDe :: Eq a => a -> Tree a -> [ Posicion ]
posicionesDe x EmptyT          = []
posicionesDe x (NodeT y t1 t2) =  singularSi aca (x == y)
                               ++ consACada Izq (posicionesDe x t1)
                               ++ consACada Der (posicionesDe x t2)

singularSi x True  = [x]
singularSi _ False = []

{-
                                 1  
                 2                               3  
         4               5               6               7     
     8       9      10      11      12      13      14      15
  16  17  18  19  20  21  22  23  24  25  26

x               = 1
listPerLevel t1 =        [ [2], [4,5],     [8,9,10,11], [16,17,18,19,20,21,22,23] ]
listPerLevel t2 =        [ [3], [6,7],     [12,13,14,15] ]
listPerLevel t  = [ [1], [2,3], [4,5,6,7], [8,9,10,11,12,13,14,15],  ... ]


todosLosCaminos t1 = [ [2]
                     , [2,4]
                     , [2,4,8]
                     , [2,4,8,16]
                     , [2,4,8,17]
                     ...
                     ]
todosLosCaminos t = [ [1]
                    , ...
                    , [1,2,4,8,16]
                    , [1,2,4,8,17]
                    , [1,2,4,9,18]
                    , [1,2,4,9,19]
                    ...
                    , [1,3,6,12]
                    , [1,3,6,13]
                    ]
-}

ej :: Tree Int
ej = NodeT 1 
      (NodeT 2 
        (NodeT 4
          (NodeT  8 (NodeT 16 EmptyT EmptyT)
                    (NodeT 17 EmptyT EmptyT))
          (NodeT  9 (NodeT 18 EmptyT EmptyT)
                    (NodeT 19 EmptyT EmptyT)))
        (NodeT 5
          (NodeT 10 (NodeT 20 EmptyT EmptyT)
                    (NodeT 21 EmptyT EmptyT))
          (NodeT 11 (NodeT 22 EmptyT EmptyT)
                    (NodeT 23 EmptyT EmptyT))))
      (NodeT 3 
        (NodeT 6
          (NodeT 12 (NodeT 24 EmptyT EmptyT)
                    (NodeT 25 EmptyT EmptyT))
          (NodeT 13 (NodeT 26 EmptyT EmptyT)
                    (NodeT 27 EmptyT EmptyT)))
        (NodeT 7
          (NodeT 14 (NodeT 28 EmptyT EmptyT)
                    (NodeT 29 EmptyT EmptyT))
          (NodeT 15 (NodeT 30 EmptyT EmptyT)
                    (NodeT 31 EmptyT EmptyT))))

-- ===================================================
-- ===================================================
data Movida = Adelante | Izquierda | Derecha
type Lugar  = [Movida]
data Objeto = Armadura | Escudo 
            | Maza     | Oro
     deriving Eq            
type Cofre = [Objeto]            
data Dungeon = Armario 
             | Pasillo    Cofre Dungeon
             | Habitacion Cofre Dungeon Dungeon

hayOroEnLugar :: Lugar -> Dungeon -> Bool    
-- PRECOND: el lugar es válido dentro del dungeon
hayOroEnLugar = undefined

-- ===================================================
-- ===================================================
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril     = Comida | Oxigeno | Torpedo | Combustible
data Sector     = S SectorId [Componente] [Tripulante]
type SectorId   = String
type Tripulante = String


data Nave = N (Tree Sector)

sectores :: Nave -> [ SectorId ]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [ SectorId ]
sectoresT EmptyT          = []
sectoresT (NodeT s t1 t2) = idSector s : sectoresT t1 
                                      ++ sectoresT t2

idSector (S id _ _) = id

-- FEO, porque mezcla niveles de abstracción
sectoresFEO :: Nave -> [ SectorId ]
sectoresFEO (N EmptyT)          = []
sectoresFEO (N (NodeT s t1 t2)) = idSector s : sectoresFEO (N t1) ++ sectoresFEO (N t2)



poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = propulsionT t

propulsionT :: Tree Sector -> Int
propulsionT EmptyT          = 0
propulsionT (NodeT s t1 t2) = propulsionS s + propulsionT t1 + propulsionT t2

propulsionS :: Sector -> Int
propulsionS (S _ cs _) = propulsionCs cs

propulsionCs :: [Componente] -> Int
propulsionCs []     = 0
propulsionCs (c:cs) = propulsionC c + propulsionCs cs

propulsionC :: Componente -> Int
propulsionC (Motor n) = n
propulsionC _         = 0


-- ===================================================
-- ===================================================
type Presa      = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre     = String -- nombre de lobo
data Lobo   = Cazador Nombre [Presa] Lobo Lobo Lobo
            | Explorador Nombre [Territorio] Lobo Lobo
            | Cria Nombre
data Manada = M Lobo

buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M lobo) = cantidadDeAlimentoL lobo

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cazador _ presas l1 l2 l3) = alimentoEn presas
                                                + cantidadDeAlimentoL l1
                                                + cantidadDeAlimentoL l2
                                                + cantidadDeAlimentoL l3
cantidadDeAlimentoL (Explorador _ _ l1 l2)      = cantidadDeAlimentoL l1
                                                + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cria _)                    = 0

alimentoEn :: [Presa] -> Int
alimentoEn ps = length ps

-- ------------------------------------------------
cantidadDeCrias :: Manada -> Int
cantidadDeCrias = undefined

-- ===========================================
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaL lobo

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador nom presas l1 l2 l3) = elegirEntre (nom, alimentoEn presas)
                                                    (elegirEntre (elAlfaL l1)
                                                                 (elegirEntre (elAlfaL l2)
                                                                              (elAlfaL l3)))
elAlfaL (Explorador nom _ l1 l2)      = elegirEntre (elAlfaL l1)
                                                    (elegirEntre (elAlfaL l2)
                                                                 (nom, 0))
elAlfaL (Cria nom)                    = (nom, 0)


elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (nom1, c1) (nom2, c2) = if (c1>=c2) then (nom1, c1)
                                                else (nom2, c2)

elAlfaL2 :: Lobo -> (Nombre, Int)
elAlfaL2 (Cazador nom presas l1 l2 l3) = elegir [ (nom, alimentoEn presas)
                                                , elAlfaL l1
                                                , elAlfaL l2
                                                , elAlfaL l3
                                                ]
elAlfaL2 (Explorador nom _ l1 l2)      = elegir [ elAlfaL l1
                                                , elAlfaL l2
                                                , (nom, 0)
                                                ]
elAlfaL2 (Cria nom)                    = (nom, 0)


elegir :: [ (Nombre, Int) ] -> (Nombre, Int)
-- PRECOND: la lista no es vacía
elegir (nc : [])  = nc
elegir (nc : ncs) = elegirEntre nc (elegir ncs)