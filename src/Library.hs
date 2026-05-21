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