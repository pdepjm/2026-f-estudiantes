module Library where
import PdePreludat
import GHC.Num (Num)

doble :: Number -> Number
doble numero = numero + numero

data Estudiante = UnEstudiante { 
    saludMental :: Number, 
    conocimiento :: Number } deriving (Show)

adquirirConocimiento :: Number -> Estudiante -> Estudiante
adquirirConocimiento cantidad estudiante = estudiante { conocimiento = conocimiento estudiante + cantidad}

quemarseElBocho :: Number -> Estudiante -> Estudiante
quemarseElBocho cantidad estudiante = estudiante { saludMental = saludMental estudiante - cantidad}

primerizo :: Estudiante
primerizo = UnEstudiante { saludMental = 150, conocimiento = 0}

recursante :: Estudiante
recursante = UnEstudiante { saludMental = 75, conocimiento = 70}

data Tema = UnTema {
    aprendizaje :: Number,
    dificultad :: Number
}


expresividad :: Tema
expresividad = UnTema { aprendizaje = 100, dificultad = 30}

declaratividad :: Tema
declaratividad = UnTema { aprendizaje = 100, dificultad = 30}

recursividad :: Tema
recursividad = UnTema { aprendizaje = 50, dificultad = 40}

type Modificador = Number -> Number

estresarse :: Modificador -> (Tema -> (Estudiante -> Estudiante))
estresarse modificador = quemarseElBocho . modificador . dificultad

aprender ::  Modificador -> Tema -> Estudiante -> Estudiante
aprender modificador = adquirirConocimiento . modificador . aprendizaje

estudiar :: Modificador -> Modificador -> Tema -> Estudiante -> Estudiante
estudiar modConocimiento modEstres tema  = (aprender modConocimiento tema)  . (estresarse modEstres tema)

type MetodoEstudio = Tema -> Estudiante -> Estudiante

llevarlaAlDia :: MetodoEstudio
llevarlaAlDia = estudiar id id

sinDormir :: MetodoEstudio
sinDormir = estudiar ((*2) . (/3)) doble

chatGPT :: MetodoEstudio
chatGPT = estudiar (/3) (/2)

factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n-1) 

anyPropio :: (a -> Bool) -> [a] -> Bool
anyPropio _ [] = False
anyPropio f (x:xs) = f x || anyPropio f xs




or :: Bool -> Bool -> Bool
or x y
    | x == True = True
    | y == True = True
    | otherwise = False


--  Pattern Cons (cabeza:resto) y tambien (cabeza:segundo:resto)
-- Patron Lista vacia []
-- Patron lista tamanio fijo [x,y,z]

type Sesion = [Tema]

hacerSesion :: MetodoEstudio -> Estudiante -> Sesion -> Estudiante
hacerSesion _ estudiante [] = estudiante
hacerSesion metodo estudiante (tema:restoTemas) = hacerSesion metodo (metodo tema estudiante) restoTemas


--foldl' :: (a -> b -> a) -> a -> [b] -> a
--foldl' _ semilla [] = semilla
--foldl' funcionAcumuladora semilla (cabeza:cola) = foldl' funcionAcumuladora (funcionAcumuladora cabeza semilla) cola

-- hacerSesion = foldl



sesionPdeP :: Sesion
sesionPdeP = [expresividad, declaratividad, recursividad]


maratonExpresividad = repeat expresividad

maratonIntercalada = cycle [declaratividad, recursividad]


repasoEmergencia :: Estudiante -> MetodoEstudio -> Sesion -> Estudiante
repasoEmergencia estudiante metodo = hacerSesion metodo estudiante . take 5