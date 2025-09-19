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