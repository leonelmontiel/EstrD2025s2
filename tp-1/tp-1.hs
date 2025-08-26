-- 1
  -- 1.
    -- a)
sucesor::Int->Int
sucesor n = n+1
-- Precondición: no tiene

    -- b)
sumar::Int->Int->Int
sumar a b = a+b
-- Precondición: no tiene
	
    -- c)
divisionYResto::Int->Int->(Int, Int)
divisionYResto a b = (div a b, mod a b)
-- Precondición: el divisor no puede ser 0
	
    -- d)
maxDelPar::(Int, Int)->Int
maxDelPar (a,b) = if (a>b)
					then a
					else b
-- Precondición: no tiene

  -- 2.
    -- 1) maxDelPar (divisionYResto (sucesor 50) (sumar 2 3))
    -- 2) maxDelPar (sumar 1 (sucesor 8) , maxDelPar (divisionYResto 4 2))
    -- 3) sucesor (maxDelPar (divisionYResto (sumar 10 8) 2))
    -- 4) sumar (maxDelPar (sucesor 7 , 1)) (maxDelPar (divisionYResto 8 4))
	
-- 3_
  -- 1.
data Dir = Norte | Este | Sur | Oeste deriving (Show)
    -- a)
opuesto::Dir->Dir
opuesto Este = Oeste
opuesto Oeste = Este
opuesto Sur = Norte
opuesto Norte = Sur
-- Precondición: no tiene

    -- b)
iguales::Dir->Dir->Bool
iguales Este Este = True
iguales Norte Norte = True
iguales Oeste Oeste = True
iguales Sur Sur = True
iguales _ _ = False
-- Precondición: no tiene

    -- c)
siguiente :: Dir -> Dir
siguiente d =
  case d of
    Norte -> Este
    Este  -> Sur
    Sur   -> Oeste
    Oeste -> error "Oeste no posee una dirección siguiente"
-- Precondición: la dirección Oeste no tiene un siguiente. Es una función Parcial ya que posee una precondición.

  -- 2.
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show 

    -- a) Devuelve un par donde la primera componente es el primer día de la semana, y la segunda componente es el último día de la semana. 
primeroYUltimoDia::(DiaDeSemana,DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo)
-- Precondición: no tiene

    -- b)  Dado un día de la semana indica si comienza con la letra M
empiezaConM::DiaDeSemana->Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False
-- Precondición: no tiene

    -- c) Dado dos días de semana, indica si el primero viene después que el segundo. Analizar la calidad de la solución respecto de la cantidad de casos analizados
vieneDespues::DiaDeSemana->DiaDeSemana->Bool
vieneDespues d1 d2 = cardinalDeDia d1 > cardinalDeDia d2
-- Precondición: no tiene

cardinalDeDia::DiaDeSemana->Int
cardinalDeDia Lunes = 1
cardinalDeDia Martes = 2
cardinalDeDia Miercoles = 3
cardinalDeDia Jueves = 4
cardinalDeDia Viernes = 5
cardinalDeDia Sabado = 6
cardinalDeDia Domingo = 7

    -- d)
estaEnElMedio::DiaDeSemana->Bool
estaEnElMedio Jueves = True
estaEnElMedio _ = False
-- Precondición: no tiene

  -- 3.
    -- a)  Dado un booleano, si es True devuelve False, y si es False devuelve True. En Haskell ya está de nida como not.
negar::Bool->Bool
negar True = False
negar False = True
-- Precondición: no tiene

    -- b) Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino devuelve True. Esta función NO debe realizar doble pattern matching. Nota: no viene implementada en Haskell.
implica::Bool->Bool->Bool
implica True False = False
implica _ _ = True
-- Precondición: no tiene

    -- c)  Dados dos booleanos si ambos son True devuelve True, sino devuelve False. Esta función NO debe realizar doble pattern matching. En Haskell ya está de nida como \&\&
yTambien::Bool->Bool->Bool
yTambien True True = True
yTambien _ _ = False
-- Precondición: no tiene

    -- d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False. Esta función NO debe realizar doble pattern matching. En Haskell ya está de nida como ||.
oBien::Bool->Bool->Bool
oBien False False = False
oBien _ _ = True
-- Precondición: no tiene

-- 4_
  -- 1.
data Persona = P String Int deriving Show
yo = P "Leonel" 29
ella = P "Ariana" 22
    -- a) Devuelve el nombre de una persona
nombre::Persona->String
nombre (P n _) = n
-- Precondición: no tiene

    -- b) Devuelve la edad de una persona
edad::Persona->Int
edad (P _ e) = e
-- Precondición: no tiene

    -- c) Aumenta en uno la edad de la persona.
crecer::Persona->Persona
crecer p = P (nombre p) (edad p +1)
-- Precondición: no tiene

    -- d) Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre::String->Persona->Persona
cambioDeNombre n p = if (n /= "")
                     then P n (edad p)
					 else error "El nombre a cambiar no puede ser vacio"
-- Precondición: El nombre a cambiar no puede ser vacío

    -- e) Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra::Persona->Persona->Bool
esMayorQueLaOtra p1 p2 = (edad p1) >= (edad p2)
-- Precondición: no tiene

    -- f) Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor::Persona->Persona->Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2)
                     then p1
					 else p2
-- Precondición: no tiene

  -- 2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un  porcentaje de energía; y Entrenador, como un nombre y dos Pokémon.
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pokemon TipoDePokemon Int deriving Show
                                 -- porcentaje de energía
pk1 = Pokemon Agua 92
pk2 = Pokemon Fuego 98
pk3 = Pokemon Planta 55

data Entrenador = Entrenador String Pokemon Pokemon deriving Show

e1 = Entrenador "Ash" pk1 pk2
e2 = Entrenador "Misty" pk1 pk3

    -- a) Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA::Pokemon->Pokemon->Bool
superaA pk1 pk2 = tipoDeUnoSuperaADos (tipoDe pk1) (tipoDe pk2)
-- Precondición: no tiene

tipoDe::Pokemon->TipoDePokemon
tipoDe (Pokemon t _) = t
-- Precondición: no tiene

tipoDeUnoSuperaADos::TipoDePokemon->TipoDePokemon->Bool
tipoDeUnoSuperaADos Agua Fuego = True
tipoDeUnoSuperaADos Fuego Planta = True
tipoDeUnoSuperaADos Planta Agua = True
tipoDeUnoSuperaADos _ _ = False
-- Precondición: no tiene

    -- b) Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe::TipoDePokemon->Entrenador->Int
cantidadDePokemonDe t e = unoSiEsIgual t (tipoDe (fstPokemon e)) + unoSiEsIgual t (tipoDe (sndPokemon e))
-- Precondición: el entrenador debe tener sí o sí dos pokemones a su cargo

fstPokemon::Entrenador->Pokemon
fstPokemon (Entrenador _ p _) = p
-- Precondición: el entrenador debe tener al menos un Pokemón a cargo

sndPokemon::Entrenador->Pokemon
sndPokemon (Entrenador _ _ p) = p
-- Precondición: el entrenador debe tener dos Pokemones a cargo

unoSiEsIgual::TipoDePokemon->TipoDePokemon->Int
unoSiEsIgual t1 t2 = if (esDeIgualTipo t1 t2)
                     then 1
					 else 0
-- Precondición: no tiene
					 
esDeIgualTipo::TipoDePokemon->TipoDePokemon->Bool
esDeIgualTipo Agua Agua = True
esDeIgualTipo Fuego Fuego = True
esDeIgualTipo Planta Planta = True
esDeIgualTipo _ _ = False
-- Precondición: no tiene

    -- c) Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador)-> [Pokemon]
juntarPokemon t = obtenerPokemonesDe (fst t) ++ obtenerPokemonesDe (snd t)
-- Precondición: no tiene

obtenerPokemonesDe::Entrenador->[Pokemon]
obtenerPokemonesDe e = [fstPokemon e, sndPokemon e]
-- Precondición: no tiene

-- 5_
  -- 1.
     -- a) Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo::a->a
loMismo a = a
-- Precondición: no tiene

      -- b) Dado un elemento de algún tipo devuelve el número 7.
siempreSiete::a->Int
siempreSiete a = 7
-- Precondición: no tiene

      -- c)Dadas una tupla, invierte sus componentes ¿Por qué existen dos variables de tipo diferentes? Porque los tipos posibles en los argumentos son paramétricos, siendo así polimórficos.
swap::(a,b)->(b,a)
swap (x,y) = (y,x)
-- Precondición: no tiene

-- 6_
   -- 1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no utilizar las funciones que ya vienen con Haskell)
   -- 2. Dada una lista de elementos, si es vacía devuelve True, sino devuelve False. Definida en Haskell como null.
estaVacia::[a]->Bool
estaVacia [] = True
estaVacia (_:_) = False
-- Precondición: no tiene

  -- 3. Dada una lista devuelve su primer elemento. Definida en Haskell como head. Nota: tener en cuenta que el constructor de listas es :
elPrimero::[a]->a 
elPrimero [] = error "La lista no puede estar vacía"
elPrimero (x:_xs) = x
-- Precondición: la lista no puede estar vacía

  -- 4. Dada una lista devuelve esa lista menos el primer elemento. Defnida en Haskell como tail. Nota: tener en cuenta que el constructor de listas es :
sinElPrimero::[a]->[a]
sinElPrimero [] = error "La lista no puede estar vacía"
sinElPrimero (_:xs) = xs
-- Precondición: la lista no puede estar vacía

  -- 5. Dada una lista devuelve un par, donde la primera componente es el primer elemento de la lista, y la segunda componente es esa lista pero sin el primero. Nota: tener en cuenta que el constructor de listas es :
splitHead::[a]->(a, [a])
splitHead (x:xs) = (x,xs)
-- Precondición: la lista debe tener al menos un elemento
