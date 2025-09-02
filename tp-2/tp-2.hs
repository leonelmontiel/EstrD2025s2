-- 1. Recursión sobre listas
  -- 1) Dada una lista de enteros devuelve la suma de todos sus elementos
sumatoria :: [Int]->Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns
-- Precondición: no tiene

  -- 2) Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud::[a]->Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs
-- Precondición: no tiene

  -- 3) Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int]->[Int]
sucesores [] = []
sucesores (n:ns) = n+1 : sucesores ns
-- Precondición: no tiene

  -- 4) Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool]->Bool
conjuncion [] = True
conjuncion (x:xs) = yTambien x (conjuncion xs)
-- Precondicion: no tiene

yTambien::Bool->Bool->Bool
yTambien True x = x
yTambien _ _ = False

  -- 5) Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool]->Bool
disyuncion [] = False
disyuncion (x:xs) = oBien x (disyuncion xs)
-- Precondicion: no tiene

oBien::Bool->Bool->Bool
oBien False x = x
oBien _ _ = True

  -- 6) Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]]->[a]
aplanar [] = []
aplanar (ls:lss) = ls ++ (aplanar lss)
-- Precondicion: no tiene

  -- 7) Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a-> [a]-> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys
-- Precondicion: no tiene

  -- 8)  Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a-> [a]-> Int
apariciones _ [] = 0
apariciones x (y:ys) = unoSi (x == y) + apariciones x ys
-- Precondicion: no tiene

unoSi::Bool->Int
unoSi x = if x then 1 else 0
-- Precondicion: no tiene

  -- 9) Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA::Int->[Int]->[Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = agregarSiEsMenor n x ++ losMenoresA n xs
-- Precondicion: no tiene

agregarSiEsMenor::Int->Int->[Int]
agregarSiEsMenor n n' = if n > n' then [n'] else []
-- Precondicion: no tiene

  -- 10) Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos
lasDeLongitudMayorA :: Int-> [[a]]-> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x:xs) = agregarSiLongitudEsMayorA n x ++ lasDeLongitudMayorA n xs
-- Precondicion: no tiene

agregarSiLongitudEsMayorA::Int->[a]->[[a]]
agregarSiLongitudEsMayorA n xs = if n < longitud xs then [xs] else []
-- Precondicion: no tiene

  -- 11) Dados una lista y un elemento, devuelve una lista con ese elemento agregado al nal de la lista.
agregarAlFinal::[a]->a->[a]
agregarAlFinal [] x = [x]
agregarAlFinal (y:ys) x = y : agregarAlFinal ys x
-- Precondicion: no tiene

  -- 12) Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. De nida en Haskell como (++).
agregar::[a]->[a]->[a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys
-- Precondicion: no tiene

  -- 13) Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa::[a]->[a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x
-- Precondicion: no tiene

  -- 14) Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente tienen la misma longitud.
zipMaximos::[Int]->[Int]->[Int]
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys

maxDelPar::(Int, Int)->Int
maxDelPar (n,n') = if n>n' then n else n'
-- del tp1

  -- 15) Dada una lista devuelve el mínimo
elMinimo::Ord a=>[a]->a
elMinimo [] = error "La lista no puede estar vacía"
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

-- 2. Recursión sobre números - Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:

  -- 1) Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial::Int->Int
factorial 0 = 1
factorial n = if n>0
  then n * factorial (n-1)
  else error "el número no debe ser negativo"
-- Precondicion: n no debe ser negativo
-- El tipo Int en Haskell tiene un rango limitado (normalmente de -2³¹ a 2³¹-1 en sistemas de 32 bits). El factorial de 56 es un número enorme, por eso lo envuelve en 0.

  -- 2) Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva::Int->[Int]
cuentaRegresiva n = if n<1
  then []
  else n : cuentaRegresiva (n-1)
-- Precondicion: no tiene

  -- 3) Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir::Int->a->[a]
repetir 0 _ = []
repetir n e = if n>0
  then e : repetir (n-1) e
  else error "el número de veces a repetir no debe ser negativo"
-- Precondicion: n no debe ser negativo

  -- 4) Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs. Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros n _ | n < 0 = error "La cantidad de elementos no debe ser negativa"
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n - 1) xs
-- Precondicion: n no debe ser negativo

  -- 5) Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n xs
  | n <= 0    = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (_:xs) = sinLosPrimeros (n - 1) xs
-- Precondicion: no tiene

-- 3.Registros
  -- 1_ Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:
data Persona = P String Int deriving Show
yo = P "Leonel" 29
ella = P "Ariana" 22
el = P "Jorge" 62
    -- a) Dados una edad y una lista de personas devuelve a las personas mayores a esa edad
mayoresA::Int->[Persona]->[Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if n>0
  then agregarPersonaSiCumpleEdad n x ++ mayoresA n xs
  else error "La edad a buscar debe ser mayor a 0"
-- Precondicion: no tiene

agregarPersonaSiCumpleEdad::Int->Persona->[Persona]
agregarPersonaSiCumpleEdad n p = if edad p > n then [p] else []
-- Precondicion: no tiene

-- del tp1  
edad::Persona->Int
edad (P _ e) = e
-- Precondición: no tiene

    -- b)  Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona
promedioEdad::[Persona]->Int
promedioEdad [] = error "La lista no puede estar vacía"
promedioEdad xs = div (sumaDeEdades xs) (longitud xs)

sumaDeEdades::[Persona]->Int
sumaDeEdades [] = 0
sumaDeEdades (x:xs) = edad x + (sumaDeEdades xs)
-- Precondicion: no tiene

    -- c) Dada una lista de personas devuelve la persona más vieja de la lista. 
elMasViejo :: [Persona] -> Persona
elMasViejo []     = error "La lista no puede estar vacía"
elMasViejo [x]    = x
elMasViejo (x:xs) = laQueEsMayor x (elMasViejo xs)
-- Precondición: la lista al menos posee una persona.

    -- del tp1 f) Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor::Persona->Persona->Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2) then p1 else p2
-- Precondición: no tiene

    -- del tp1 e) Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra::Persona->Persona->Bool
esMayorQueLaOtra p1 p2 = (edad p1) >= (edad p2)
-- Precondición: no tiene

  -- 2_ Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

pk1 = ConsPokemon Agua 92
pk2 = ConsPokemon Fuego 98
pk3 = ConsPokemon Planta 55

e1 = ConsEntrenador "Ash" [pk1]
e2 = ConsEntrenador "Misty" [pk1, pk2, pk3, pk1]
e3 = ConsEntrenador "Brook" []

    -- a) Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon::Entrenador->Int
cantPokemon (ConsEntrenador _ xs) = longitud xs
-- Precondicion: no tiene

    -- b)  Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe::TipoDePokemon->Entrenador->Int
cantPokemonDe _ (ConsEntrenador _ []) = 0
cantPokemonDe t (ConsEntrenador n (x:xs)) = unoSiEsIgual t (tipoDe x) + cantPokemonDe t (ConsEntrenador n xs)
-- Precondicion: no tiene

-- del tp1
unoSiEsIgual::TipoDePokemon->TipoDePokemon->Int
unoSiEsIgual t1 t2 = if (esDeIgualTipo t1 t2) then 1 else 0
-- Precondición: no tiene

-- del tp1					 
esDeIgualTipo::TipoDePokemon->TipoDePokemon->Bool
esDeIgualTipo Agua Agua = True
esDeIgualTipo Fuego Fuego = True
esDeIgualTipo Planta Planta = True
esDeIgualTipo _ _ = False
-- Precondición: no tiene

-- del tp1 pero modificado para que acepte el tipo ConsPokemon
tipoDe::Pokemon->TipoDePokemon
tipoDe (ConsPokemon t _) = t
-- Precondición: no tiene

    -- c) Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ []) (ConsEntrenador _ _) = 0
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = sumarTipoDeLeGanaATodosLosDe t e1 e2
-- Precondicion: no tiene

sumarTipoDeLeGanaATodosLosDe::TipoDePokemon->Entrenador->Entrenador->Int
sumarTipoDeLeGanaATodosLosDe t (ConsEntrenador n1 (p:ps)) (ConsEntrenador n2 enemigos) = if esDeIgualTipoYLeGanaATodos t p enemigos
  then 1 + cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador n1 ps) (ConsEntrenador n2 enemigos)
  else cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador n1 ps) (ConsEntrenador n2 enemigos)
-- Precondicion: no tiene

esDeIgualTipoYLeGanaATodos::TipoDePokemon->Pokemon->[Pokemon]->Bool
esDeIgualTipoYLeGanaATodos t p enemigos = esDeIgualTipo (tipoDe p) t && leGanaATodos p enemigos
-- Precondicion: no tiene

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos _ [] = True
leGanaATodos p (x:xs) = superaA p x && leGanaATodos p xs
-- Precondicion: no tiene

    -- del tp1  a) Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA::Pokemon->Pokemon->Bool
superaA pk1 pk2 = tipoDeUnoSuperaADos (tipoDe pk1) (tipoDe pk2)
-- Precondición: no tiene

-- del tp1 
tipoDeUnoSuperaADos::TipoDePokemon->TipoDePokemon->Bool
tipoDeUnoSuperaADos Agua Fuego = True
tipoDeUnoSuperaADos Fuego Planta = True
tipoDeUnoSuperaADos Planta Agua = True
tipoDeUnoSuperaADos _ _ = False
-- Precondición: no tiene

  {-
  3_ El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es una lista de personas con diferente rol. La definición es la siguiente
  -}

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

dev = Developer Junior (ConsProyecto "Link")
dev1 = Developer Senior (ConsProyecto "Capa Java")
man = Management Senior (ConsProyecto "Link")
emp = ConsEmpresa [dev, dev1, man, dev1]
emp1 = ConsEmpresa []
emp2 = ConsEmpresa [dev1, man]

    -- a) Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = proyectosSinRepetidos (proyectosDe roles)
-- Precondicion: no tiene

proyectosSinRepetidos :: [Proyecto] -> [Proyecto]
proyectosSinRepetidos [] = []
proyectosSinRepetidos (p:ps) = agregarSiNoExiste p (proyectosSinRepetidos ps)
-- Precondicion: no tiene

agregarSiNoExiste :: Proyecto -> [Proyecto] -> [Proyecto]
agregarSiNoExiste p ps =
  if perteneceProyecto p ps
    then ps
    else p : ps
-- Precondicion: no tiene

perteneceProyecto :: Proyecto -> [Proyecto] -> Bool
perteneceProyecto _ [] = False
perteneceProyecto p (x:xs) =
  if nombreDeProyectoDe p == nombreDeProyectoDe x
    then True
    else perteneceProyecto p xs
-- Precondicion: no tiene

proyectosDe::[Rol]->[Proyecto]
proyectosDe [] = []
proyectosDe (x:xs) = proyectoDe x : proyectosDe xs
-- Precondicion: no tiene

proyectoDe::Rol->Proyecto
proyectoDe (Developer _ x) = x
proyectoDe (Management _ x) = x
-- Precondicion: no tiene

nombreDeProyectoDe :: Proyecto -> String
nombreDeProyectoDe (ConsProyecto nombre) = nombre
-- Precondicion: no tiene

    -- c) Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losDevSenior::Empresa->[Proyecto]->Int
losDevSenior e ps = contarCoincidencias (losSeniorsDe e) ps
-- Precondicion: no tiene

contarCoincidencias :: [Rol] -> [Proyecto] -> Int
contarCoincidencias _ [] = 0
contarCoincidencias r (p:ps) = sumarUnoSiCumpleProyecto r p + contarCoincidencias r ps
-- Precondicion: no tiene

losSeniorsDe::Empresa->[Rol]
losSeniorsDe (ConsEmpresa []) = []
losSeniorsDe (ConsEmpresa (x:xs)) = agregarSiEsSenior x ++ losSeniorsDe (ConsEmpresa xs)
-- Precondicion: no tiene

agregarSiEsSenior::Rol->[Rol]
agregarSiEsSenior (Developer s p) = 
  if esSenior s 
    then [(Developer s p)] 
    else []
agregarSiEsSenior (Management _ _) = [] -- En la consigna pide Desarrolladores (Developer), no Management
-- Precondicion: no tiene

esSenior::Seniority->Bool
esSenior Senior = True
esSenior _ = False
-- Precondicion: no tiene

sumarUnoSiCumpleProyecto::[Rol]->Proyecto->Int
sumarUnoSiCumpleProyecto [] _ = 0
sumarUnoSiCumpleProyecto (x:xs) p = 
  if nombreDeProyectoDe p == nombreDeProyectoDe (proyectoDe x) 
    then 1 + sumarUnoSiCumpleProyecto xs p 
    else sumarUnoSiCumpleProyecto xs p
-- Precondicion: no tiene

    -- d)  Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn::[Proyecto]->Empresa->Int
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn (p:ps) (ConsEmpresa rs) = sumarUnoSiCumpleProyecto rs p + cantQueTrabajanEn ps (ConsEmpresa rs)
-- Precondicion: no tiene

    -- e) Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto::Empresa->[(Proyecto, Int)]
asignadosPorProyecto e = agruparSegunProyecto e
-- Precondicion: no tiene

agruparSegunProyecto::Empresa->[(Proyecto, Int)]
agruparSegunProyecto (ConsEmpresa xs) = agregarEmpleadoPorProyecto (proyectosSinRepetidos (proyectosDe xs)) xs
-- Precondicion: no tiene

agregarEmpleadoPorProyecto::[Proyecto]->[Rol]->[(Proyecto, Int)]
agregarEmpleadoPorProyecto [] _ = []
agregarEmpleadoPorProyecto (p:ps) [] = (p, 0) : agregarEmpleadoPorProyecto ps []
agregarEmpleadoPorProyecto (p:ps) rs = (p, sumarUnoSiCumpleProyecto rs p) : agregarEmpleadoPorProyecto ps rs
-- Precondicion: no tiene