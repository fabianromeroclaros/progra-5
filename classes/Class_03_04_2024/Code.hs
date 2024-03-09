module Class_03_04_2024.Code where

type DiaLiteral = String

type DiaNumeral = Int

type MesNumeral = Int

type MesLiteral = Int

type AñoNumeral = Int

type DiaDelMes = Int

queDiaEs :: DiaNumeral -> DiaLiteral
queDiaEs x = case x of
  1 -> "Domingo"
  2 -> "Lunes"
  3 -> "Martes"
  4 -> "Miércoles"
  5 -> "Jueves"
  6 -> "Viernes"
  7 -> "Sábados"
  _ -> "No sé qué día es"

diaDelMes :: MesNumeral -> AñoNumeral -> DiaDelMes
diaDelMes m a =
  let esBiciesto = añoEsBiciesto a
   in case m of
        1 -> 31
        2 -> if esBiciesto then 29 else 28
        3 -> 31
        4 -> 30
        5 -> 31
        6 -> 30
        7 -> 31
        8 -> 31
        9 -> 30
        10 -> 31
        11 -> 30
        12 -> 31
        _ -> -1
  where
    añoEsBiciesto :: AñoNumeral -> Bool
    añoEsBiciesto a =
      let esDivisiblePor4 = a `mod` 4 == 0
          esDivisiblePor100 = a `mod` 100 == 0
          esDivisiblePor400 = a `mod` 400 == 0
       in (esDivisiblePor4 && not esDivisiblePor100) || esDivisiblePor400

-- Cualquier función que reciba dos parámetros puede llamarse por a `mod` b
