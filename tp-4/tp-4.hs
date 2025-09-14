{-1. Pizzas
 Tenemos los siguientes tipos de datos:-}
data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

pizza0 = Prepizza
pizza1 = Capa Salsa (pizza0)
pizza2 = Capa Queso (pizza1)
pizza3 = Capa Jamon (pizza2)
pizza4 = Capa (Aceitunas 8) (pizza3)

ing0 = []
ing1 = [Salsa]
ing2 = [Queso]++ing1
ing3 = [Jamon]++ing2
ing4 = [Aceitunas 8]++ing3
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
tieneSoloSalsaYQueso::Pizza->Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa _ _) = False
tieneSoloSalsaYQueso (Capa ing p) = esSalsaOQueso ing && tieneSoloSalsaYQueso p

esSalsaOQueso::Ingrediente->Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False

-- Hacerlo comparando una lista con los ingredientes fijos, y otros con esos mismos ingredientes pero quitándolos si se encuetran en la primera lista