module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Estudiante = UnEstudiante {
    saludMental :: Number,
    conocimiento :: Number
} deriving Show

adquirirConocimiento :: Number -> Estudiante -> Estudiante
adquirirConocimiento cantidad estudiante = estudiante {conocimiento = conocimiento estudiante + cantidad} 

quemarseElBocho :: Number -> Estudiante -> Estudiante
quemarseElBocho cantidad estudiante = estudiante {saludMental = saludMental estudiante - cantidad} 

primerizo :: Estudiante
primerizo = UnEstudiante{
    saludMental = 150,
    conocimiento = 0
}

recursante :: Estudiante
recursante = UnEstudiante {
    saludMental = 75,
    conocimiento = 70
}

data Tema = UnTema {
    dificultad :: Number,
    aprendizaje :: Number 
} deriving Show

expresividad :: Tema
expresividad = UnTema { dificultad = 30, aprendizaje = 100 }

declaratividad :: Tema
declaratividad = UnTema { dificultad = 30, aprendizaje = 100 }

recursividad :: Tema
recursividad = UnTema { dificultad = 50, aprendizaje = 40 }

type Modificador = Number -> Number

estudiar :: Modificador -> Modificador -> Tema -> Estudiante -> Estudiante
estudiar modificadorConocimiento modificadorSaludMental tema = 
    aprenderTema modificadorConocimiento tema.estresarse modificadorSaludMental tema

aprenderTema :: Modificador -> Tema -> Estudiante -> Estudiante
aprenderTema modificadorConocimiento = adquirirConocimiento.modificadorConocimiento.aprendizaje

estresarse :: Modificador -> Tema -> Estudiante -> Estudiante
estresarse modificadorSaludMental = quemarseElBocho.modificadorSaludMental.dificultad

{-

:t (.) 
> (b -> c) -> (a -> b) -> (a -> c)

modificadorConocimiento.aprendizaje

modificadorConocimiento -> aprendizaje -> modificarConocimiento.aprendizaje

Modificador -> (Tema -> Number)

> (b -> c) -> (a -> b) -> (a -> c)
(Number -> Number) -> (Tema -> Number) -> (Tema -> Number)

unaFuncion = modificarConocimiento.aprendizaje :: (Tema -> Number)

adquirirConocimiento.unaFuncion

> (b -> c) -> (a -> b) -> (a -> c)

(Number -> Estudiante -> Estudiante) -> (Tema -> Number) -> ()

suma x y z = x + y + z

Number -> Number -> Number

Number -> (Number -> Number)

Number -> (Number -> (Number -> Number))

unTipo = Estudiante -> Estudiante

(b -> c) -> (a -> b) -> (a -> c)
(Number -> (unTipo)) -> (Tema -> Number) -> (Tema -> unTipo)

adquirirConocimiento.modificadorConocimiento.aprendizaje

Tema -> unTipo

Tema -> Estudiante -> Estudiante

(Number -> (Estudiante -> Estudiante)) -> (Tema -> Number) -> (Tema -> (Estudiante -> Estudiante))


-}

type MetodoDeEstudio = Tema -> Estudiante -> Estudiante

llevarlaAlDia :: MetodoDeEstudio
llevarlaAlDia = estudiar id id

sinDormir :: MetodoDeEstudio
sinDormir = estudiar dosTercios doble

dosTercios' :: Modificador
dosTercios' numero = 2 * (numero / 3)

dosTercios :: Modificador
dosTercios = (*2).(/3)

chatGPT :: MetodoDeEstudio
chatGPT = estudiar (/3) (/2)

type Sesion = [Tema]

--hacerSesionDeEstudio :: MetodoDeEstudio -> Estudiante -> Sesion -> Estudiante

--multiplicar (x,y) = x + y

--multiplicar tupla = fst tupla * snd tupla

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

--[1]
--x = 1
--xs = []

factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n-1)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' transformador (x:xs) = transformador x : map' transformador xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = True
any' condicion (x:xs) = condicion x || any' condicion xs

hacerSesionDeEstudio :: MetodoDeEstudio -> Estudiante -> Sesion -> Estudiante
hacerSesionDeEstudio _ estudiante [] = estudiante
hacerSesionDeEstudio metodo estudiante (tema:restoTemas) = hacerSesionDeEstudio metodo (metodo tema estudiante) restoTemas

hacerSesionDeEstudio' :: MetodoDeEstudio -> Estudiante -> Sesion -> Estudiante
hacerSesionDeEstudio' metodo estudiante temas = foldl (flip metodo) estudiante temas

maratonDeExpresividad :: Sesion
maratonDeExpresividad = repeat expresividad

multiplicar (x,y) = x * y

primero (x,y) = x

{-
multiplicar (1+2, 3+2)
> multiplicar (3, 5)
3 * 5
15

LAZY EVALUATION / EVALUACION DIFERIDA
(1+2) * (3+2)
3 * 5
15

primero (1+2, 3+2)
primero (3, 5)
3

1+2
3

condicion x || any' condicion xs
condicion expresividad || any' condicion [declaratividad]

?? > 20
aprendizaje expresividad > 20
100 > 20
True

take 5
-}