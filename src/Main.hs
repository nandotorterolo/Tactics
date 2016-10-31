module Main where

import Data.List (intercalate)
import Data.Maybe (isNothing)
import Control.Arrow ((&&&))
import System.Random

data Player = PlayerWhite | PlayerBlack                                      deriving (Eq, Show, Enum)
data Fil = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8                             deriving (Eq,Ord,Enum)
data Col = CA | CB | CC | CD | CE | CF | CG | CH                             deriving (Eq,Ord,Enum)
data Color = Blanca | Negra                                                  deriving (Eq)
data Unidad = Rey| Mago | Bufo | Arco | Caballo | Pica | Espada | Hacha      deriving (Eq)
data Coordenada = Coord Fil Col                                              deriving (Eq)

type GameState = (Tablero,Int,Player)
type Ficha = (Unidad,Color)
type FichaTablero = (Unidad,Color,Coordenada)
type Tablero = [FichaTablero]

main :: IO ()
main = showIOBoard (crearTableroCompleto todasLasUnidades []) >>= putStr

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

--Devuelve lista de unidades que puede perder la una unidad dada
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

colorGana :: Ficha -> (Color,[Unidad])
colorGana (unidad, Negra) = (Blanca, gana unidad)
colorGana (unidad, Blanca) = (Negra, gana unidad)

colorPierde :: Ficha -> (Color,[Unidad])
colorPierde (unidad, Negra) = (Blanca, pierde unidad)
colorPierde (unidad, Blanca) = (Negra, pierde unidad)

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

-- Imprime un tablero
-- putStr (showBoard fichasEstado1)
showBoard :: Tablero -> String
showBoard fichas =
 emptyRow ++
 "|    | A | B | C | D | E | F | G | H |\n" ++
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

showIOBoard :: IO Tablero -> IO String
showIOBoard fichas = 
  do 
    tablero <- fichas
    return (showBoard tablero)

-- Dado una fila y un tablero, devuelve los caracteres asociados a cada una de las fillas)
-- ejemplo, Fila1 -> Tablero que tiene 2 reyes en las columnas 3,5,devuelve: [" "," ","R"," ","r"," "," "," "]
-- < +----+---+---+---+---+---+---+---+---+
-- < |    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |  <- columnas
-- < +----+---+---+---+---+---+---+---+---+
-- < | 1  |   |  | R  |   | r  |   |   |   |
-- < +----+---+---+---+---+---+---+---+---+
cadena :: Fil -> Tablero -> [String]
cadena f fichas =  map (\col -> [fichaStrPosicion fichas (Coord f col)]) [CA .. CH]

-- dado un tablero y una coordenada devuelve el carater asociado a la ficha a imprimir
fichaStrPosicion :: Tablero -> Coordenada -> Char
fichaStrPosicion tablero coord = fichaMaybeStr (fichaCoordenada tablero coord)

-- Predicado para ocupacion de posicion.
estaLibre :: Tablero -> Coordenada -> Bool
estaLibre tablero coord = isNothing (fichaCoordenada tablero coord)

-- Dado un tablero y una coordenada devuelve opcionalmente una ficha
-- En la funcion where se utiliza Arrow &&&, la forma clasico seri la linea de abajo.
-- where listaFichas = map (\ ft -> (unidadFT ft, colorFT ft)) (filter (\ficha -> coordenadaFT ficha == coord) tablero)
-- https://en.wikibooks.org/wiki/Haskell/Understanding_arrows
fichaCoordenada :: Tablero -> Coordenada -> Maybe Ficha
fichaCoordenada tablero coord =
  if null listaFichas
    then Nothing
    else Just (head listaFichas) -- la lista deberia tener solo un elemento
  where listaFichas = map (unidadFT &&& colorFT) (filter (\ficha -> coordenadaFT ficha == coord) tablero)

-- Lista de coordenadas validas al inicio del juego para las negras
posicionesValidasNegrasLibres :: Tablero -> [Coordenada]
posicionesValidasNegrasLibres t = [c | c <- posicionesValidasNegras, estaLibre t c]

posicionesValidasNegras :: [Coordenada]
posicionesValidasNegras = [Coord f c | f <- [F1 .. F3], c<- [CA .. CH], not (elem (Coord f c) posicionesReyNegro)]

posicionesReyNegro :: [Coordenada]
posicionesReyNegro = [Coord F1 CD, Coord F1 CE, Coord F2 CD, Coord F2 CE]

-- Lista de coordenadas validas al inicio del juego para las blancas
posicionesValidasBlancasLibres :: Tablero -> [Coordenada]
posicionesValidasBlancasLibres t = [c | c <- posicionesValidasBlancas, estaLibre t c]

posicionesValidasBlancas :: [Coordenada]
posicionesValidasBlancas = [Coord f c | f <- [F6 .. F8],  c <- [CA .. CH], not (elem (Coord f c) posicionesReyBlanco)]

posicionesReyBlanco :: [Coordenada]
posicionesReyBlanco = [Coord F7 CD, Coord F7 CE, Coord F8 CD, Coord F8 CE]

-- Utilidades
obtenerElementoRandomico :: [a] -> IO a
obtenerElementoRandomico xs = do
  n <- randomRIO (0, (length xs) - 1)
  return $ xs !! n

-- showIOBoard (posicionarFichaRandomico (Rey, Blanca) []) >>= putStr
-- showIOBoard (posicionarFichaRandomico (Hacha, Blanca) []) >>= putStr
posicionarFichaRandomico :: Ficha -> Tablero -> IO Tablero
posicionarFichaRandomico (Rey, Blanca) t = 
  do
    posicion <- obtenerElementoRandomico posicionesReyBlanco
    return ((Rey, Blanca, posicion) : t)

posicionarFichaRandomico (Rey, Negra) t = 
  do
    posicion <- obtenerElementoRandomico posicionesReyNegro
    return ((Rey, Negra, posicion) : t)

posicionarFichaRandomico (u, Blanca) t = 
  do
    posicion <- obtenerElementoRandomico (posicionesValidasBlancasLibres t)
    return ((u, Blanca, posicion) : t)

posicionarFichaRandomico (u, Negra) t = 
  do
    posicion <- obtenerElementoRandomico (posicionesValidasNegrasLibres t)
    return ((u, Negra, posicion) : t)

todasLasUnidades :: [Ficha]
todasLasUnidades = [
  (Hacha,Blanca), (Hacha,Blanca), (Espada,Blanca), (Espada,Blanca), (Pica,Blanca), (Pica,Blanca), (Caballo,Blanca), (Caballo,Blanca), (Arco,Blanca), (Arco,Blanca), (Bufo,Blanca), (Bufo,Blanca), (Mago,Blanca), (Mago,Blanca), (Rey,Blanca),
  (Hacha,Negra), (Hacha,Negra), (Espada,Negra), (Espada,Negra), (Pica,Negra), (Pica,Negra), (Caballo,Negra), (Caballo,Negra), (Arco,Negra), (Arco,Negra), (Bufo,Negra), (Bufo,Negra), (Mago,Negra), (Mago,Negra), (Rey,Negra)
  ]

-- showIOBoard (crearTableroCompleto todasLasUnidades []) >>= putStr
crearTableroCompleto :: [Ficha] -> Tablero -> IO Tablero
crearTableroCompleto [] t = return t
crearTableroCompleto (ficha:xs) t =
  do
    tablero <- posicionarFichaRandomico ficha t
    crearTableroCompleto xs tablero
