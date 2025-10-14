data Ingrediente = Salsa | Queso | Aceitunas Int 
                 | Anchoas | Anana | Roquefort 
                 | Jamon | Cerezas | AzucarNegra
                 | Tomate | Ajo | Longaniza
     deriving Show
data Pizza = Prepizza 
           | Capa Ingrediente Pizza
     deriving Show

pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8) 
              (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa Cerezas 
              (Capa AzucarNegra (Capa Anana 
                                      (Capa Queso (Capa Jamon Prepizza))))
pizza5 = Capa Queso (Capa Queso (Capa Queso (Capa Queso Prepizza)))
      --            \--------------------------------------------/
      -- Capa ing                      p

pizza3' = Capa (Aceitunas 8) pizza2

cantQueso :: Pizza -> Int
cantQueso Prepizza     = 0
cantQueso (Capa ing p) = unoSi (esQueso ing) + cantQueso p
{- Mhhhh... Y las subtareas
   if esQueso ing 
    then 1 + cantQueso p 
    else cantQueso p
-}

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0
-- unoSi b = if b then 1 else 0

esQueso :: Ingrediente -> Bool
esQueso Queso     = True
esQueso Roquefort = True
esQueso _         = False


cantAceitunas :: Pizza -> Int
--cantAceitunas Prepizza     = ...
--cantAceitunas (Capa ing p) = ... ing ... cantAceitunas p ...
--
--cantAceitunas Prepizza     = ...
--cantAceitunas (Capa ing p) = cantAceitunasIng ing + cantAceitunas p
--
cantAceitunas Prepizza     = 0
cantAceitunas (Capa ing p) = cantAceitunasIng ing + cantAceitunas p

cantAceitunasIng :: Ingrediente -> Int
cantAceitunasIng (Aceitunas n) = n
cantAceitunasIng _             = 0

agregados :: Pizza -> [Ingrediente]
--agregados Prepizza     = ...
--agregados (Capa ing p) = ... ing ... agregados p ...
--
--agregados Prepizza     = ...
--agregados (Capa ing p) = singularSi ing (esAgregado ing) ++ agregados p
--
agregados Prepizza     = []
agregados (Capa ing p) = singularSi ing (esAgregado ing) ++ agregados p

singularSi :: a -> Bool -> [a]
singularSi x True  = x:[]
singularSi x False = []

esAgregado :: Ingrediente -> Bool
esAgregado Queso = False
esAgregado Salsa = False
esAgregado _     = True

duplicarQueso :: Pizza -> Pizza
--duplicarQueso Prepizza     = ...
--duplicarQueso (Capa ing p) = ... ing ... duplicarQueso p ...
--
--duplicarQueso Prepizza     = ...
--duplicarQueso (Capa ing p) = if esQueso ing
--                              then Capa ing (Capa ing (duplicarQueso p))
--                              else Capa ing (duplicarQueso p)
--
duplicarQueso Prepizza     = Prepizza
duplicarQueso (Capa ing p) = if esQueso ing
                              then Capa ing (Capa ing (duplicarQueso p))
                              else Capa ing (duplicarQueso p)

--duplicarQueso Prepizza     = Prepizza
--duplicarQueso (Capa ing p) = subtarea ing (duplicarQueso p)

data Color = Azul | Negro | Rojo | Verde
data Celda = Bolita Color Celda | CeldaVacia

celda0 = CeldaVacia
celda1 = Bolita Rojo CeldaVacia
celda2 = Bolita Rojo (Bolita Verde CeldaVacia)
celda3 = Bolita Rojo (Bolita Rojo  CeldaVacia)

--nroBolitas :: Color -> Celda -> Int

data Pirata = JackSparrow | Barbanegra | DavyJones | WillTurner
data Cofre = MonedasDeOro | CorazonDe Pirata | Joyas | Nada
data Dir = Izq | Der
data MapaDelTesoro = Equis Int Cofre | Avanzar Int MapaDelTesoro | Girar Dir MapaDelTesoro

mapa0 = Equis 10 (CorazonDe DavyJones)
mapa1 = Avanzar 30 (Equis 10 (CorazonDe DavyJones))
mapa2 = Avanzar 30 (Girar Der (Avanzar 10 (Equis 5 MonedasDeOro)))

--f :: MapaDelTesoro -> a
--f (Equis   n c) = ... n ... c ...
--f (Avanzar n m) = ... n ... f m ...
--f (Girar   d m) = ... d ... f m ...

pasosAlTesoro :: MapaDelTesoro -> Int
--pasosAlTesoro (Equis   n c) = ... n ... c ...
--pasosAlTesoro (Avanzar n m) = ... n ... pasosAlTesoro m ...
--pasosAlTesoro (Girar   d m) = ... d ... pasosAlTesoro m ...
--
--pasosAlTesoro (Equis   _ _) = ...
--pasosAlTesoro (Avanzar n m) = n + pasosAlTesoro m
--pasosAlTesoro (Girar   _ m) = pasosAlTesoro m
--
pasosAlTesoro (Equis   _ _) = 0
pasosAlTesoro (Avanzar n m) = n + pasosAlTesoro m
pasosAlTesoro (Girar   _ m) = pasosAlTesoro m


-- http://bit.ly/GobstonesQuell


data Objeto = Armadura | Escudo | Maza     | Oro
     deriving Show
data Dungeon = Armario | Habitacion Objeto Dungeon Dungeon
     deriving Show

d1 = Habitacion Oro Armario Armario
d3 = Habitacion Maza 
        Armario
        (Habitacion Escudo 
            Armario
            (Habitacion Maza Armario Armario))

d5 = Habitacion Escudo
      (Habitacion Maza Armario
        (Habitacion Oro Armario Armario)
      )
      (Habitacion Armadura
        (Habitacion Oro Armario 
          (Habitacion Escudo Armario Armario))
        (Habitacion Maza Armario
          (Habitacion Escudo Armario
            (Habitacion Oro Armario Armario))
        )
      )

cantidadDeOro :: Dungeon -> Int
cantidadDeOro Armario                = 0
cantidadDeOro (Habitacion obj d1 d2) = 
   unoSi (esOro obj) + cantidadDeOro d1 + cantidadDeOro d2

esOro :: Objeto -> Bool
esOro Oro = True
esOro _   = False

profundidad :: Dungeon -> Int
profundidad Armario                = 0
profundidad (Habitacion obj d1 d2) = 1 + elMasGrande (profundidad d1) (profundidad d2)

elMasGrande n1 n2 = if n1>n2 then n1 else n2
 -- max

cambiarMazasPorOro :: Dungeon -> Dungeon
cambiarMazasPorOro Armario                = Armario
cambiarMazasPorOro (Habitacion obj d1 d2) = 
       Habitacion (cambiarUnaMazaPorOro obj) (cambiarMazasPorOro d1)
                                             (cambiarMazasPorOro d2)

cambiarUnaMazaPorOro :: Objeto -> Objeto
cambiarUnaMazaPorOro Maza = Oro
cambiarUnaMazaPorOro obj  = obj

-- cambiarMazasPorOro (Habitacion Escudo Armario (Habitacion Maza Armario Armario))
--                                \----/ \-----/ \--------------------------------/
--                                  obj    d1                 d2
--                     Habitacion Escudo Armario (Habitacion Oro  Armario Armario)

objetos :: Dungeon -> [Objeto]
objetos Armario                = []
objetos (Habitacion obj d1 d2) = obj : objetos d1 ++ objetos d2

objsDelCaminoMasLargo :: Dungeon -> [Objeto]
objsDelCaminoMasLargo Armario                = []
objsDelCaminoMasLargo (Habitacion obj d1 d2) =
   obj : elegirLaMasLarga (objsDelCaminoMasLargo d1) (objsDelCaminoMasLargo d2)

elegirLaMasLarga :: [a] -> [a] -> [a]
elegirLaMasLarga os1 os2 = if length os1 > length os2
                            then os1
                            else os2

data MiArbol = Base       Char
             | NodoDeUno  Int MiArbol
             | NodoDeDos  Int MiArbol MiArbol
             | NodoDeTres Int MiArbol MiArbol MiArbol

--f :: MiArbol -> b
--f (Base c)                = ... c ...
--f (NodoDeUno  n a1)       = ... n ... f a1 ...
--f (NodoDeDos  n a1 a2)    = ... n ... f a1 ... f a2 ...
--f (NodoDeTres n a1 a2 a3) = ... n ... f a1 ... f a2 ... f a3 ...
