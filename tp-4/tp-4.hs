{-1. Pizzas
 Tenemos los siguientes tipos de datos:-}
data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

pizza0 = Prepizza
pizza1 = Capa Salsa (pizza0)
pizza2 = Capa Queso (pizza1)
pizza3 = Capa Jamon (pizza2)
pizza4 = Capa (Aceitunas 8) (pizza3)
pizza5 = Capa Salsa (Capa Queso Prepizza)

ing0 = []
ing1 = [Salsa]
ing2 = [Queso]++ing1
ing3 = [Jamon]++ing2
ing4 = [Aceitunas 8]++ing3

pizzas0 = []
allPizzas = [pizza0, pizza1, pizza2, pizza3, pizza4]
--Definir las siguientes funciones:

  -- 1) Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas::Pizza->Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p
-- Precondicion: ninguna

  -- 2) Dada una lista de ingredientes construye una pizza
armarPizza::[Ingrediente]->Pizza
armarPizza [] = Prepizza
armarPizza (ing:ings) = Capa ing (armarPizza ings)
-- Precondicion: ninguna

  -- 3) Le saca los ingredientes que sean jamón a la pizza
sacarJamon::Pizza->Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing p) = if (esJamon ing)
                          then sacarJamon p
                          else Capa ing (sacarJamon p)
-- Precondicion: ninguna

esJamon::Ingrediente->Bool
esJamon Jamon = True
esJamon _ = False
-- Precondicion: ninguna

  -- 4) Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) =
    case ing of
        Salsa -> tieneSoloSalsaYQuesoConFlags p True False
        Queso -> tieneSoloSalsaYQuesoConFlags p False True
        _     -> False

tieneSoloSalsaYQuesoConFlags :: Pizza -> Bool -> Bool -> Bool
tieneSoloSalsaYQuesoConFlags Prepizza haySalsa hayQueso = haySalsa && hayQueso
tieneSoloSalsaYQuesoConFlags (Capa ing p) haySalsa hayQueso =
    case ing of
        Salsa -> tieneSoloSalsaYQuesoConFlags p True hayQueso
        Queso -> tieneSoloSalsaYQuesoConFlags p haySalsa True
        _     -> False


  -- 5) Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas::Pizza->Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) = Capa (duplicarAceitunasSiHay ing) p
-- Precondicion: ninguna

duplicarAceitunasSiHay::Ingrediente->Ingrediente
duplicarAceitunasSiHay (Aceitunas n) = Aceitunas (n*2)
duplicarAceitunasSiHay ing = ing
-- Precondicion: ninguna

  -- 6) Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza::[Pizza]->[(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p):cantCapasPorPizza ps
-- Precondicion: ninguna

{-
 2. Mapa de tesoros (con bifurcaciones)
 Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
 cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
 -}
data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa

c0 = Cofre []
c1 = Cofre [Chatarra]
c2 = Cofre [Tesoro]
c3 = Cofre [Tesoro, Chatarra, Tesoro]

mapa0 = Fin c0
mapa1 = Fin c1
mapa2 = Fin c2
mapa3 = Fin c3

mapa4 = Bifurcacion c0 mapa0 mapa0
mapa5 = Bifurcacion c0 mapa1 mapa1
mapa6 = Bifurcacion c2 mapa0 mapa1
mapa7 = Bifurcacion c1 mapa2 mapa3
mapa8 = Bifurcacion c3 mapa7 mapa6
mapa9 = Bifurcacion c3 mapa1 mapa8
mapa10 = Bifurcacion c0 mapa1 mapa9
--  Definir las siguientes operaciones:

  -- 1) Indica si hay un tesoro en alguna parte del mapa.
hayTesoro::Mapa->Bool
hayTesoro (Fin c) = hayTesoroDentro c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroDentro c || hayTesoro m1 || hayTesoro m2
--Precondicion: ninguna

hayTesoroDentro::Cofre->Bool
hayTesoroDentro (Cofre obs) = hayAlMenosUnTesoro obs
--Precondicion: ninguna

hayAlMenosUnTesoro::[Objeto]->Bool
hayAlMenosUnTesoro [] = False
hayAlMenosUnTesoro (ob:obs) = esTesoro ob || hayAlMenosUnTesoro obs
--Precondicion: ninguna

esTesoro::Objeto->Bool
esTesoro Tesoro = True
esTesoro _ = False
--Precondicion: ninguna

  -- 2) Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una lista vacía de direcciones.
hayTesoroEn::[Dir]->Mapa->Bool
hayTesoroEn dirs m = hayTesoroSegunDirs dirs m
-- Precondicion: ninguna

cofreDe::Mapa->Cofre
cofreDe (Fin c) = c
cofreDe (Bifurcacion c _ _) = c
-- Precondicion: ninguna

mapaSegunDir::Dir->Mapa->Mapa
mapaSegunDir _ (Fin _) = error "el mapa deve ser con bifurcación"
mapaSegunDir Izq (Bifurcacion _ m1 _) = m1
mapaSegunDir Der (Bifurcacion _ _ m2) = m2
-- Precodicion: el mapa debe ser con Bifurcacion

hayTesoroSegunDirs::[Dir]->Mapa->Bool
hayTesoroSegunDirs [] m = hayTesoroDentro (cofreDe m)
hayTesoroSegunDirs (d) (Fin _) = error "Las direcciones exceden el rango del mapa"
hayTesoroSegunDirs (d:ds) m = hayTesoroEn ds (mapaSegunDir d m)
-- Precondicion: cuando se evalúe un mapa Fin Cofre, no deben existir direcciones en la lista dada

  -- 3)  Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro::Mapa->[Dir]
caminoAlTesoro (Fin c) = if hayTesoroDentro c 
                         then []
                         else error "No se encontró un tesoro, debería existir uno en el camino"
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroDentro c 
                                       then []
                                       else agregarDirAlTesoro m1 m2
agregarDirAlTesoro::Mapa->Mapa->[Dir]
agregarDirAlTesoro m1 m2 = if hayTesoro m1
                        then Izq : caminoAlTesoro m1
                        else Der : caminoAlTesoro m2

  -- 4)  Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga::Mapa->[Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) =
       let izq = Izq:caminoDeLaRamaMasLarga m1
           der = Der:caminoDeLaRamaMasLarga m2
       in if length izq >= length der
          then izq
          else der

  -- 5) Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel::Mapa->[[Objeto]]
tesorosPorNivel (Fin c) = [tesorosDe c]
tesorosPorNivel (Bifurcacion c m1 m2) = tesorosDe c : (tesorosPorNivel m1 ++ tesorosPorNivel m2)

tesorosDe::Cofre->[Objeto]
tesorosDe (Cofre []) = []
tesorosDe (Cofre (ob:obs)) = agregarSi ob (esTesoro ob) ++ tesorosDe (Cofre obs)

agregarSi::Objeto->Bool->[Objeto]
agregarSi x True = [x]
agregarSi _ _ = []

  -- 6) Devuelve todos lo caminos en el mapa.
todosLosCaminos::Mapa->[[Dir]]
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ m1 m2) =
  agregarDirACada Izq (todosLosCaminos m1) ++ agregarDirACada Der (todosLosCaminos m2)

agregarDirACada::Dir->[[Dir]]->[[Dir]]
agregarDirACada d [] = [[d]]
agregarDirACada d (ds:dss) = (d:ds) : agregarDirACada d dss

{- 3. Nave Espacial
 modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial, dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación es la siguiente:-}
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show

data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

cs1 = [LanzaTorpedos, Motor 7]

s0 = S "0" [] []
s1 = S "1" [LanzaTorpedos, Almacen []] ["Leo"]
s2 = S "2" [Motor 5, Almacen [Torpedo, Comida]] ["Elias", "Montiel"]
s3 = S "3" [Motor 18, Almacen [Comida, Oxigeno], Motor 2, LanzaTorpedos] ["Leo"]

st0 = EmptyT
st1 = NodeT s0 st0 st0
st2 = NodeT s1 st0 st1
st3 = NodeT s2 st1 st2
st4 = NodeT s3 st2 st3

nave0 = N st0
nave1 = N st1
nave2 = N st2
nave3 = N st3
nave4 = N st4
-- Implementar las siguientes funciones utilizando recursión estructural:

  -- 1) Devuelve todos los sectores de la nave.
sectores::Nave->[SectorId]
sectores (N sector) = idsDe sector

idsDe::Tree Sector->[SectorId]
idsDe EmptyT = []
idsDe (NodeT s s1 s2) = idDe s : (idsDe s1 ++ idsDe s2)

idDe::Sector->SectorId
idDe (S id _ _) = id

  -- 3)  Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota: el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion::Nave->Int
poderDePropulsion (N sector) = cantDePoderDeMotores sector

cantDePoderDeMotores::Tree Sector->Int
cantDePoderDeMotores EmptyT = 0
cantDePoderDeMotores (NodeT s s1 s2) = cantDePoderDe s + cantDePoderDeMotores s1 + cantDePoderDeMotores s2

cantDePoderDe::Sector->Int
cantDePoderDe (S _ [] _) = 0
cantDePoderDe (S id (c:cs) ts) = poderDe c + cantDePoderDe (S id cs ts)

poderDe::Componente->Int
poderDe (Motor poder) = poder
poderDe _ = 0

  -- 3) Devuelve todos los barriles de la nave.
barriles::Nave->[Barril]
barriles (N sector) = todosLosBarrilesDel sector

todosLosBarrilesDel::Tree Sector->[Barril]
todosLosBarrilesDel EmptyT = []
todosLosBarrilesDel (NodeT s s1 s2) = barrilesDel s ++ todosLosBarrilesDel s1 ++ todosLosBarrilesDel s2

barrilesDel::Sector->[Barril]
barrilesDel (S _ cs _) = soloLosBarriles cs

soloLosBarriles::[Componente]->[Barril]
soloLosBarriles [] = []
soloLosBarriles (c:cs) = barrilesDe c ++ soloLosBarriles cs

barrilesDe::Componente->[Barril]
barrilesDe (Almacen bs) = bs
barrilesDe _ = []

  -- 4) Añade una lista de componentes a un sector de la nave.Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N sector) = N (agregarEnSector cs id sector)

agregarEnSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarEnSector _ _ EmptyT = EmptyT
agregarEnSector cs id (NodeT s s1 s2) =
  if tieneMismoId id s
  then NodeT (agregarComponentes cs s) s1 s2
  else NodeT s (agregarEnSector cs id s1) (agregarEnSector cs id s2)

tieneMismoId::SectorId->Sector->Bool
tieneMismoId id (S sectorId cs ts) = id == sectorId

agregarComponentes::[Componente]->Sector->Sector
agregarComponentes [] sector = sector
agregarComponentes cs (S id comps ts) = S id (cs++comps) ts

  -- 5)  Incorpora un tripulante a una lista de sectores de la nave. Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA::Tripulante->[SectorId]->Nave->Nave
asignarTripulanteA t ids (N sector) = N (agregarTripulanteEnSectores t ids sector)

agregarTripulanteEnSectores::Tripulante->[SectorId]->Tree Sector->Tree Sector
agregarTripulanteEnSectores _ _ EmptyT = EmptyT
agregarTripulanteEnSectores t ids (NodeT s s1 s2) = 
  NodeT (agregarTripulante t s ids) (agregarTripulanteEnSectores t ids s1) (agregarTripulanteEnSectores t ids s2)

agregarTripulante::Tripulante->Sector->[SectorId]->Sector
agregarTripulante t (S id cs ts) ids =
  if elem id ids
  then S id cs (t:ts)
  else S id cs ts

  -- 6) Devuelve los sectores en donde aparece un tripulante dado
sectoresAsignados::Tripulante->Nave->[SectorId]
sectoresAsignados t (N sector) = sinSectorIdRepetidos (sectoresDelTripulante t sector)

sectoresDelTripulante::Tripulante->Tree Sector->[SectorId]
sectoresDelTripulante _ EmptyT = []
sectoresDelTripulante t (NodeT s s1 s2) = 
  let idsAsignados = (sectoresDelTripulante t s1 ++ sectoresDelTripulante t s2)
  in if elem t (tripulantesDe s)
    then  idDe s : idsAsignados
    else idsAsignados

tripulantesDe::Sector->[Tripulante]
tripulantesDe (S _ _ ts) = ts

sinSectorIdRepetidos::[SectorId]->[SectorId]
sinSectorIdRepetidos [] = []
sinSectorIdRepetidos (x:xs) =
  if elem x xs
  then sinSectorIdRepetidos xs
  else x : sinSectorIdRepetidos xs

  -- 7) Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes::Nave->[Tripulante]
tripulantes (N sectores) = sinTripulantesRepetidos (tripulantesDeTodosLos sectores)

tripulantesDeTodosLos::Tree Sector->[Tripulante]
tripulantesDeTodosLos EmptyT = []
tripulantesDeTodosLos (NodeT s s1 s2) = tripulantesDe s ++ tripulantesDeTodosLos s1 ++ tripulantesDeTodosLos s2

sinTripulantesRepetidos::[Tripulante]->[Tripulante]
sinTripulantesRepetidos [] = []
sinTripulantesRepetidos (t:ts) =
  if elem t ts
  then sinTripulantesRepetidos ts
  else t : sinTripulantesRepetidos ts

{- 4. Manada de lobos
Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
Los diferentes casos de lobos que forman la jerarquía son los siguientes:
- Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
- Los exploradores poseen nombre, una lista de nombres de territorio explorados (nombres de bosques, ríos, etc.), y poseen 2 lobos a cargo.
- Las crías poseen sólo un nombre y no poseen lobos a cargo.
La estructura es la siguiente: -}
type Presa = String-- nombre de presa
type Territorio = String-- nombre de territorio
type Nombre = String-- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre deriving Show
data Manada = M Lobo deriving Show
{-
  1) Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura que corresponda en cada caso:
-}
presas0 = ["Conejo", "Raton", "Venado"]
presas1 = presas0++["Chinchilla", "Pájaro"]
territorios0 = ["Pueblo"]
territorios1 = ["Bosque", "Rio de La Plata", "Riachuelo"]

cria0 = Cria "Tizi"
cria1 = Cria "Tahiel"
cria2 = Cria "Montiel"
cria3 = Cria "Daniela"

exp0 = Explorador "Elias" territorios0 cria0 cria1
exp1 = Explorador "Ari" territorios1 cria1 cria2
exp2 = Explorador "Atilio" territorios1 exp1 exp0

manada0 = M (Cazador "Guille" [] exp0 cria1 cria0)
manada1 = M (Cazador "Leo" presas0 exp0 exp1 cria3)
manada2 = M (Cazador "Messi" presas1 cria1 cria2 cria3)
manada3 = M (Cazador "Pipi" presas0 exp1 exp0 exp2)

  -- 2)  dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza::Manada->Bool
buenaCaza (M lobo) = cantAlimentoMayorQueCantCrias lobo

cantAlimentoMayorQueCantCrias::Lobo->Bool
cantAlimentoMayorQueCantCrias (Cria _) = False
cantAlimentoMayorQueCantCrias (Explorador _ _ l1 l2) = cantAlimentoMayorQueCantCrias l1 || cantAlimentoMayorQueCantCrias l2
cantAlimentoMayorQueCantCrias (Cazador _ ps l1 l2 l3) = length ps > (cantCrias l1 + cantCrias l2 + cantCrias l3)

cantCrias::Lobo->Int
cantCrias (Cria _) = 1
cantCrias (Explorador _ _ l1 l2) = cantCrias l1 + cantCrias l2
cantCrias (Cazador _ _ l1 l2 l3) = cantCrias l1 + cantCrias l2 + cantCrias l3

  -- 3) dada una manada, devuelve el nombre del lobo con más presas cazadas, junto con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de cero presas.
elAlfa::Manada->(Nombre, Int)
elAlfa (M lobos) = elLoboAlfa lobos

elLoboAlfa::Lobo->(Nombre, Int)
elLoboAlfa (Cria nom) = (nom, 0)
elLoboAlfa (Explorador nom _ l1 l2) =
  loboConMasPresas [(nom, 0), elLoboAlfa l1, elLoboAlfa l2]
elLoboAlfa (Cazador nom ps l1 l2 l3) =
  loboConMasPresas [(nom, length ps), elLoboAlfa l1, elLoboAlfa l2, elLoboAlfa l3]

loboConMasPresas::[(Nombre, Int)]->(Nombre, Int)
loboConMasPresas [tl] = tl
loboConMasPresas (tl:tls) =
  if cantPresasDe tl > cantPresasDe (loboConMasPresas tls)
  then tl
  else loboConMasPresas tls

cantPresasDe::(Nombre, Int)->Int
cantPresasDe (n,ps) = ps

  -- 4) dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losQueExploraron::Territorio->Manada->[Nombre]
losQueExploraron t (M lobos) = sinNombresRepetidos (nombresDeLosQueExploraron t lobos)

nombresDeLosQueExploraron::Territorio->Lobo->[Nombre]
nombresDeLosQueExploraron t (Explorador nom ts l1 l2) = agregarNombreSiExploro t ts nom ++ nombresDeLosQueExploraron t l1 ++ nombresDeLosQueExploraron t l2
nombresDeLosQueExploraron t (Cazador _ _ l1 l2 l3) = nombresDeLosQueExploraron t l1 ++ nombresDeLosQueExploraron t l2 ++ nombresDeLosQueExploraron t l3
nombresDeLosQueExploraron _ (Cria _) = []

agregarNombreSiExploro::Territorio->[Territorio]->Nombre->[Nombre]
agregarNombreSiExploro t ts nom =
  if elem t ts
  then [nom]
  else []

sinNombresRepetidos::[Nombre]->[Nombre]
sinNombresRepetidos [] = []
sinNombresRepetidos (x:xs) = if elem x xs
  then xs
  else x : sinNombresRepetidos xs

  -- 5) dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: Manada-> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobos) = agruparTerritorios (paresExploradores lobos)

paresExploradores::Lobo->[(Territorio, Nombre)]
paresExploradores (Cria _) = []
paresExploradores (Cazador _ _ l1 l2 l3) = paresExploradores l1 ++ paresExploradores l2 ++ paresExploradores l3
paresExploradores (Explorador nom ts l1 l2) = agregarNombrePorTerritorio nom ts ++ paresExploradores l1 ++ paresExploradores l2

agregarNombrePorTerritorio::Nombre->[Territorio]->[(Territorio, Nombre)]
agregarNombrePorTerritorio _ [] = []
agregarNombrePorTerritorio nom (t:ts) = (t, nom) : agregarNombrePorTerritorio nom ts

agruparTerritorios::[(Territorio, Nombre)]->[(Territorio, [Nombre])]
agruparTerritorios [] = []
agruparTerritorios ((t,nom):tns) = insertarTuplaTN t nom (agruparTerritorios tns)

insertarNombreSiNoEsta :: Nombre -> [Nombre] -> Territorio -> [(Territorio,[Nombre])] -> [(Territorio,[Nombre])]
insertarNombreSiNoEsta nom noms t tns =
  if elem nom noms
     then (t, noms) : tns
     else (t, nom:noms) : tns

-- Inserta (Territorio,Nombre) en la lista
insertarTuplaTN :: Territorio -> Nombre -> [(Territorio,[Nombre])] -> [(Territorio,[Nombre])]
insertarTuplaTN t nom [] = [(t, [nom])]
insertarTuplaTN t nom ((t', noms):tnss) =
  if t == t'
    then insertarNombreSiNoEsta nom noms t' tnss
    else (t', noms) : insertarTuplaTN t nom tnss