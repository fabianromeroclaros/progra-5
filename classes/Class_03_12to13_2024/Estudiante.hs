module Class_03_12_2024.Estudiante where

import System.Random (randomRIO)

data Estudiante = Estudiante
  { 
    nombres :: String,
    apellidos :: String,
    salioElegido :: Bool,
    resolvioEjercicio :: Bool,
    existe :: Bool,
    conPermiso :: Bool
  }
  deriving (Show)

estudiantes :: [Estudiante]
estudiantes =
  [ Estudiante "Pedro" "Perez Velazquez" True False True False,
    Estudiante "Chapo1" "Herrera" True True True True,
    Estudiante "Chapo2" "Herrera" False False False True,
    Estudiante "Chapo3" "Herrera" False True True True
  ]

estudianteaElegibles :: [Estudiante]
estudianteaElegibles = filter (\x -> (not $ salioElegido x) && existe x) estudiantes

seleccionarEstudiante :: IO Estudiante
seleccionarEstudiante = do
  indice <- randomRIO (0, length estudianteaElegibles - 1)
  return $ estudianteaElegibles !! indice


main :: IO ()
main = do
  estudianteSeleccionado <- seleccionarEstudiante
  print $ nombres estudianteSeleccionado