module Main where

import Data.List (intercalate)

data Fil = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8                             deriving (Eq,Ord,Enum)
data Col = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8                             deriving (Eq,Ord,Enum)
data Color = Blanca | Negra                                                  deriving (Eq)
data Unidad = Rey| Mago | Bufo | Arco | Caballo | Pica | Espada | Hacha      deriving (Eq)
data Coordenada = Coord Fil Col                                              deriving (Eq)

type Ficha = (Unidad,Color)
type FichaTablero = (Unidad,Color,Coordenada)
type Tablero = [FichaTablero]

main :: IO ()
main = putStr (showBoard fichasEstado1)

-- Unidades del Juego - 15 fichas
unidades :: [Unidad]
unidades = [Hacha, Espada, Pica, Caballo, Arco, Bufo, Mago, Rey]

--Devuelve lista de unidades que puede ganarle la una unidad dada
gana :: Unidad -> [Unidad]
gana uni = case uni of
  Hacha -> [Arco, Bufo, Mago]
  Espada -> [Hacha, Bufo, Mago]
  Pica -> [Hacha, Espada, Caballo]
  Caballo -> [Hacha, Espada, Arco]
  Arco -> [Espada, Pica, Bufo]
  Bufo -> [Pica, Caballo, Mago]
  Mago -> [Pica, Caballo, Arco]
  _ -> unidades --rey

--Devuelve lista de unidades que puede perder la una unidad dadapierde :: Unidad -> [Unidad]
pierde :: Unidad -> [Unidad]
pierde uni = case uni of
  Hacha -> [Espada, Pica, Caballo]
  Espada -> [Pica, Caballo, Arco]
  Pica -> [Arco, Bufo, Mago]
  Caballo -> [Pica, Bufo, Mago]
  Arco -> [Hacha,Caballo, Mago]
  Bufo -> [Hacha,Espada, Arco]
  Mago -> [Hacha, Espada, Arco]
  _ -> unidades --rey

colorGana :: (Color, Unidad) -> (Color,[Unidad])
colorGana (Negra, uni) = (Blanca, gana uni)
colorGana (Blanca, uni) = (Negra, gana uni)

colorPierde :: (Color,Unidad) -> (Color,[Unidad])
colorPierde (Negra, uni) = (Blanca, pierde uni)
colorPierde (Blanca, uni) = (Negra, pierde uni)

unidadFT :: FichaTablero -> Unidad
unidadFT (unidad,_,_) = unidad

colorFT :: FichaTablero -> Color
colorFT (_,color,_) = color

coordenadaFT (_,_,coordenada) = coordenada
coordenadaFT :: FichaTablero -> Coordenada

--Caracter asociado a cada Ficha, Blancas son mayusculas, negras minusculas
fichaStr :: Ficha -> Char
fichaStr (Hacha, Blanca) = 'H'
fichaStr (Hacha, Negra) = 'h'
fichaStr (Espada,Blanca) = 'E'
fichaStr (Espada,Negra) = 'e'
fichaStr (Pica,Blanca) = 'P'
fichaStr (Pica,Negra) = 'p'
fichaStr (Caballo,Blanca) = 'C'
fichaStr (Caballo,Negra) = 'c'
fichaStr (Arco,Blanca) = 'A'
fichaStr (Arco,Negra) = 'a'
fichaStr (Bufo,Blanca) = 'B'
fichaStr (Bufo,Negra) = 'b'
fichaStr (Mago,Blanca) = 'M'
fichaStr (Mago,Negra) = 'm'
fichaStr (Rey,Blanca) = 'R'
fichaStr (Rey,Negra) = 'r'

fichaMaybeStr :: Maybe Ficha -> Char
fichaMaybeStr mFicha = case mFicha of
  Just ficha -> fichaStr ficha
  Nothing  -> ' '

--putStr (showBoard fichasEstado1)
showBoard :: [FichaTablero] -> String
showBoard fichas =
 emptyRow ++
 "|    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |\n" ++
 emptyRow ++
 row "1" (cadena F1 fichas) ++
 row "2" (cadena F2 fichas) ++
 row "3" (cadena F3 fichas) ++
 row "4" (cadena F4 fichas) ++
 row "5" (cadena F5 fichas) ++
 row "6" (cadena F6 fichas) ++
 row "7" (cadena F7 fichas) ++
 row "8" (cadena F8 fichas)
 where
  emptyRow = "+----+---+---+---+---+---+---+---+---+\n"
  row rowNumber fichas = "| " ++ rowNumber ++ "  | " ++ intercalate " | " fichas ++ " |\n" ++ emptyRow

-- Dado una fila y un tablero, devuelve los caracteres asociados a cada una de las fillas)
-- ejemplo, Fila1 -> Tablero que tiene 2 reyes en las columnas 3,5,devuelve: [" "," ","R"," ","r"," "," "," "]
-- < +----+---+---+---+---+---+---+---+---+
-- < |    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |  <- columnas
-- < +----+---+---+---+---+---+---+---+---+
-- < | 1  |   |  | R  |   | r  |   |   |   |
-- < +----+---+---+---+---+---+---+---+---+
cadena :: Fil -> Tablero -> [String]
cadena f fichas =  map (\col -> [fichaStrPocision fichas (Coord f col)]) [C1 .. C8]

-- Dado un tablero y una coordenada devuelve opcionalmente una ficha
fichaCoordenada :: Tablero -> Coordenada -> Maybe Ficha
fichaCoordenada tablero coord = if null listaFichas then Nothing else Just (head listaFichas)
  where listaFichas = map (\(u,c,_) -> (u,c)) (filter (\ficha -> coordenadaFT ficha == coord) tablero)

-- dado un tablero y una coordenada devuelve el carater asociado a la ficha a imprimir
fichaStrPocision :: Tablero -> Coordenada -> Char
fichaStrPocision tablero coord = fichaMaybeStr (fichaCoordenada tablero coord)

-- Lista de coordenadas validas al inicio del juego para las blancas
-- Filas 6..8, Columnas 1..8
-- posisionesValidasBlancas :: [Coordenada]


-- Lista de coordenadas validas al inicio del juego para las negras
-- Filas 6..8, Columnas 1..8
-- posisionesValidasNegras :: [Coordenada]


-- 15 unidades blancas iniciales, hacer un algoritmo de como poner las fichas
-- llamar de esta forma, putStr (showBoard fichasEstado1)
-- type Fichas = [(Unidad,Color,Coord)]
-- TODO hacer muchos tableros iniciales, pero solo de un color, para que jueguen
fichasEstado1 :: Tablero
fichasEstado1 = [
   (Hacha, Blanca, Coord F2 C1)
 , (Hacha, Negra, Coord F1 C2)
 , (Espada, Negra, Coord F3 C8)
 , (Espada, Blanca, Coord F4 C2)
 , (Rey, Negra, Coord F5 C7)
 , (Rey, Negra, Coord F6 C2)
 , (Bufo, Blanca, Coord F7 C4)
 , (Mago, Negra, Coord F8 C5)
 ]
