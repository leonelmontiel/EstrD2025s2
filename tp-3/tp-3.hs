{-1. Tipos recursivos simples
 1.1. Celdas con bolitas
 Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:-}
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show
{- En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría ser la siguiente:-}
bol1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
bol0 = CeldaVacia
--Implementar las siguientes funciones sobre celdas:

  -- 1)  Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya existe una operación sobre listas que ayude a resolver el problema.
nroBolitas::Color->Celda->Int 
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita co ce) = unoSi (esMismoColor c co) + nroBolitas c ce
-- Precondición: no tiene

esMismoColor::Color->Color->Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False
-- Precondición: no tiene

unoSi::Bool->Int
unoSi True = 1
unoSi _ = 0
-- Precondición: no tiene

  -- 2) Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner::Color->Celda->Celda
poner co celda = Bolita co celda
-- Precondición: no tiene

  -- 3)  Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar::Color->Celda->Celda
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita co ce) = if (esMismoColor c co)
                         then ce
                         else Bolita co (sacar c ce)
-- Precondición: no tiene

  -- 4)  Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN::Int->Color->Celda->Celda
ponerN n c celda = if (esMenorOIgualQueCero n)
                   then celda
                   else Bolita c (ponerN (n-1) c celda)
-- Precondicion: no tiene

esMenorOIgualQueCero::Int->Bool
esMenorOIgualQueCero n = n <= 0
-- Precondicion: no tiene

{- 1.2. Camino hacia el tesoro
 Tenemos los siguientes tipos de datos -}
data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

cam0 = Fin
cam1 = Nada Fin
cam2 = Cofre [] cam1
cam3 = Cofre [Cacharro] cam0
cam4 = Cofre [Tesoro] cam3
cam5 = Cofre [] cam4
cam6 = Nada cam5
cam7 = Cofre [Tesoro, Cacharro, Tesoro] cam6
-- Definir las siguientes funciones:

  -- 1)  Indica si hay un cofre con un tesoro en el camino.
hayTesoro::Camino->Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre obs c) = tieneTesoro obs || hayTesoro c
-- Precondicion: no tiene

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (Tesoro:_) = True
tieneTesoro (_:xs) = tieneTesoro xs
-- Precondición: no tiene

esTesoro::Objeto->Bool
esTesoro Tesoro = True
esTesoro _ = False
-- Precondición: no tiene

  -- 2) Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0. Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = error "No hay tesoro en el camino"
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre obs c) = if tieneTesoro obs
                                 then 0
                                 else 1 + pasosHastaTesoro c

  -- 3)  Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn::Int->Camino->Bool
hayTesoroEn n c = n == pasosHastaTesoro c
-- Precondicion: tiene que haber al menos un tesoro.

  -- 4)  Indica si hay al menos n tesoros en el camino
alMenosNTesoros::Int->Camino->Bool
alMenosNTesoros n c = if esMenorOIgualQueCero n 
                      then error "el número a evaluar deber ser mayor o igual que 0" 
                      else sumarTodosLosTesoros c >= n
-- Precondicion: el número a evaluar deber ser mayor o igual que 0

sumarTodosLosTesoros::Camino->Int
sumarTodosLosTesoros (Fin) = 0
sumarTodosLosTesoros (Nada c) = sumarTodosLosTesoros c
sumarTodosLosTesoros (Cofre obs c) = sumarTesoros obs + sumarTodosLosTesoros c
-- Precondicion: no tiene

sumarTesoros::[Objeto]->Int
sumarTesoros [] = 0
sumarTesoros (x:xs) = unoSi (esTesoro x) + sumarTesoros xs
-- Precondicion: no tiene

  -- 5) Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 0 c = sumarTodosLosTesoros c 
cantTesorosEntre n m c = contarTesorosEnRango (m - n + 1) (quitarPrimerosNPasos n c)
-- Precondicion: Debe existir al menos un camino siguiente

contarTesorosEnRango :: Int -> Camino -> Int
contarTesorosEnRango 0 _ = 0
contarTesorosEnRango _ Fin = 0
contarTesorosEnRango pasos (Nada c) = contarTesorosEnRango (pasos - 1) c
contarTesorosEnRango pasos (Cofre obs c) = sumarTesoros obs + contarTesorosEnRango (pasos - 1) c
-- Precondicion: no tiene

quitarPrimerosNPasos::Int->Camino->Camino
quitarPrimerosNPasos n c = if esMenorOIgualQueCero n
                   then c
                   else quitarPrimerosNPasos (n-1) (quitarPrimerPaso c)
-- Precondicion: Debe existir al menos un camino siguiente

quitarPrimerPaso::Camino->Camino
quitarPrimerPaso Fin = error "Debe existir al menos un camino siguiente"
quitarPrimerPaso (Nada c) = c
quitarPrimerPaso (Cofre _ c) = c
-- Precondicion: Debe existir al menos un camino siguiente

{-  2. Tipos arbóreos
 2.1. Árboles binarios
 Dada esta definición para árboles binarios -}
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

arb0 = EmptyT
arb1 = NodeT (1::Int) (EmptyT) (EmptyT)
arb2 = NodeT (5::Int) arb1 arb0
arb3 = NodeT (2::Int) arb2 arb1
-- defina las siguientes funciones utilizando recursión estructural según corresponda:

--  Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT::Tree Int->Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2