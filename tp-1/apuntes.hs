multiplicar::Int->Int->Int
multiplicar a b = a*b

dividir::Int->Int->Int
dividir a b = if (b == 0)
then error "No se puede dividir por 0"
else div a b

sumar::Int->Int->Int
sumar a b = a+b

restar::Int->Int->Int
restar a b = a-b

max::Int->Int->Int
max a b = if (a>b)
then a
else b

min::Int->Int->Int
min a b = if (a<b)
then a
else b

{- ENUMERATIVOS: 
	❏ Se dan los constructores que definen los casos
	❏ Cada constructor determina un elemento
-}
data Dir = Norte | Este | Sur | Oeste deriving (Show)

direccionDefault::Dir
direccionDefault = Este

{-  REGISTROS:
	❏ Se da un único constructor con argumentos
	❏ El constructor tener nombre diferente al tipo
-}
data Persona = P String Int String deriving (Show)
	    -- Nombre Edad DNI

yo, el, ella :: Persona
yo = P "Leonel" 29 "39405936"
el = P "Jorge" 11 "49063111"
ella = P "Ariana" 22 "44021125"

{- PATTERN MATCHING:
	❏ Los constructores se pueden usar para acceder
	❏ Para eso se usan en los parámetros
		❏ Si el parámetro coincide, usa dicha ecuación
		❏ Si no, continúa verificando la siguiente ecuación
	❏ Solo se aplica a constructores de tipos algebraicos
	❏ El orden de las ecuaciones importa
-}
siguienteDir::Dir->Dir
siguienteDir Norte = Este
siguienteDir Este = Sur
siguienteDir Sur = Oeste
siguienteDir Oeste = Norte

-- También hay una alternativa indexada como expresión
siguienteDir'::Dir->Dir
siguienteDir' d = case d of
				Norte -> Este
				Este -> Sur
				Sur -> Oeste
				Oeste -> Norte

{- 	❏ También se puede usar para definir observadores
	❏ Observar el uso de variables como argumento de P
	❏ Si el parámetro no se usa, se puede usar un comodín ( _ )
-}
nombre::Persona->String
nombre (P n _ _) = n

edad::Persona->Int
edad (P _ e _) = e 

dni::Persona->String
dni (P _ _ d) = d

esMayorDeEdad::Persona->Bool
esMayorDeEdad (P _ e _) = e>=18

esEste::Dir->Bool
esEste Este = True
esEste _ = False

esConjuncionPM::Bool->Bool->Bool
esConjuncionPM True True = True
esConjuncionPM _ _ = False

esDisyuncionPM::Bool->Bool->Bool
esDisyuncionPM False False = False
esDisyuncionPM _ _ = True

{- SUMAS:
	❏ Los constructores pueden tener argumentos
	❏ Los tipos en posición de argumento van SOLAMENTE en las declaraciones data
	❏ Significa que, por ejemplo, el constructor Vasito seguido de un Gusto es un valor válido de tipo Helado
-}
data Gusto = Chocolate | Sambayon | DDL | Frutilla deriving (Show)
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving (Show)

miHeladoFavorito::Helado
miHeladoFavorito = Cucurucho Sambayon DDL

unVasito::Helado
unVasito = Vasito Sambayon

esGustoSerio::Gusto->Bool
esGustoSerio Sambayon = True
esGustoSerio DDL = True
esGustoSerio _ = False

-- En el pattern matching, se usan variables
esHeladoSerio::Helado->Bool
esHeladoSerio (Vasito g) = esGustoSerio g
esHeladoSerio (Cucurucho g1 g2) = esGustoSerio g1 && esGustoSerio g2
esHeladoSerio (Pote g1 g2 g3) = esGustoSerio g1 && esGustoSerio g2 && esGustoSerio g3

sinFrutilla::Helado->Helado
sinFrutilla (Vasito g) = Vasito (cambiarFrutilla g)
sinFrutilla (Cucurucho g1 g2) = Cucurucho (cambiarFrutilla g1) (cambiarFrutilla g2)
sinFrutilla (Pote g1 g2 g3) = Pote (cambiarFrutilla g1) (cambiarFrutilla g2) (cambiarFrutilla g3)

cambiarFrutilla::Gusto->Gusto
cambiarFrutilla Frutilla = Sambayon
cambiarFrutilla d = d

{- TUPLAS:
	❏ Son estructuras de datos predefinidas
	❏ Son similares a un registro, pero sin nombres de campo
	❏ Se escriben entre paréntesis y con comas (tanto los valores como los tipos)
-}

unPar::(Int, Bool)
unPar = (2, True)

unaTerna::(Int, String, Bool)
unaTerna = (3, "", True)

unParDePares::((Int, Bool), (String, Int))
unParDePares = ((2, True), ("Hola", 17))

unParImpar::((Int, Bool), (Int, String, Bool))
unParImpar = ((unPar), (unaTerna))

{- POLIMORFISMO PARAMÉTRICO:
 ❏ Permite definir funciones genéricas
	* Una sola definición opera sobre muchos tipos
	* No es necesario redefinir lo mismo una y otra vez
 ❏ Permite definir estructuras de datos genéricas
	* Estructuras como “contenedores de datos”
	* Independencia del contenedor respecto del tipo de dato
 ❏ Los paréntesis y la coma funcionan como un constructor
 ❏ Se puede hacer pattern matching sobre tuplas
 ❏ Si el elemento no está restringido, se utilizan variables de tipo (en diferentes usos, puede tomar diferentes tipos)
 ❏ La instaciación la hace el sistema de tipos
 ❏ Haskell se encarga de reemplazar esas variables
 ❏ Si a la función le importa la estructura, pero no los datos específcos, puede ser polimórfica
	* Es posible mientras no se opere con el parámetro
	* Si se usa alguna operación no polimórfica, no es posible
-}
fst::(a, b)->a
fst (x, y) = x

snd::(a, b)->b 
snd (x, y) = y

-- Función Monomórfica (importa el tipo del dato):
succ::Int->Int 
succ n = n+1

{- LISTAS:
	❏ La notación [1,2,3] es abreviatura de 1:2:3:[]
	❏ Las operaciones de (:) y [] son constructores
	❏ La operación de agregar (++) no es predefinida
	❏ Así, son equivalentes:
		[1,2,3]      1:2:3:[]          1:[2,3]   
			[1]++[2,3]     (1:[])++(2:[3])
	❏ Las listas son estructuras de datos predefinidas
		* (:) :: a -> [a] -> [a]
		* Se lee cons
		* Toma un elemento  de algún tipo y una lista del mismo tipo, y describe la lista que resulta de agregar el elemento adelante
	❏ [] :: [a]
		* Se lee lista vacía (o nil)
		* Es una lista de algún tipo a instanciar
-}

head::[a]->a 
head (x:_) = x

tail::[a]->[a]
tail (_:xs) = xs
