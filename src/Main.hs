module Main where

import Data.List (intercalate)
import Data.Maybe
import Control.Arrow ((&&&))
import System.Random

data Player = PlayerWhite | PlayerBlack                                      deriving (Eq, Show, Enum)
data GameStatus = Turn Player | Roll Player | Finished                       deriving (Eq, Show)
-- Tablero, NroTurno,JugadorActivo, dado1,dado2
data GameState = GameState Tablero Int Player (Int,Int)                      deriving (Eq) --TODO
data GameAction = DiceRoll Int Int | Move FichaTablero Coordenada | Pasar    deriving (Eq) --TODO

data Fil = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8                             deriving (Eq,Ord,Enum, Show)
data Col = CA | CB | CC | CD | CE | CF | CG | CH                             deriving (Eq,Ord,Enum, Show)
data Color = Blanca | Negra                                                  deriving (Eq, Show)
data Unidad = Rey| Mago | Bufo | Arco | Caballo | Pica | Espada | Hacha      deriving (Eq, Show)
data Coordenada = Coord Fil Col                                              deriving (Eq, Show)

--type GameState = (Tablero,Int,Player)
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
posicionesValidasNegras = [Coord f c | f <- [F1 .. F3], c<- [CA .. CH],  Coord f c `notElem` posicionesRey Negra]

posicionesRey :: Color -> [Coordenada]
posicionesRey Negra  = [Coord F1 CD, Coord F1 CE, Coord F2 CD, Coord F2 CE]
posicionesRey Blanca = [Coord F7 CD, Coord F7 CE, Coord F8 CD, Coord F8 CE]

-- posicionesReyNegro :: [Coordenada]
-- posicionesReyNegro = [Coord F1 CD, Coord F1 CE, Coord F2 CD, Coord F2 CE]

-- Lista de coordenadas validas al inicio del juego para las blancas
posicionesValidasBlancasLibres :: Tablero -> [Coordenada]
posicionesValidasBlancasLibres t = [c | c <- posicionesValidasBlancas, estaLibre t c]

posicionesValidasBlancas :: [Coordenada]
posicionesValidasBlancas = [Coord f c | f <- [F6 .. F8],  c <- [CA .. CH], Coord f c `notElem` posicionesRey Blanca]

-- Utilidades
obtenerElementoRandomico :: [a] -> IO a
obtenerElementoRandomico xs = do
  n <- randomRIO (0, length xs - 1)
  return $ xs !! n

posicionarFichaRandomico :: Ficha -> Tablero -> IO Tablero
posicionarFichaRandomico (Rey, c) t =
  do
    posicion <- obtenerElementoRandomico (posicionesRey c)
    return ((Rey, c, posicion) : t)

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


-- nuestas logicas ------------------------------------------------------------------------------------
estoyEncarga :: (Int,Int) -> Bool
estoyEncarga (a,b) = a==b

-- data Fil = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8                             deriving (Eq,Ord,Enum)
-- data Col = CA | CB | CC | CD | CE | CF | CG | CH                             deriving (Eq,Ord,Enum)

-- show (movimientoDiagonal (Rey, Blanca, Coord F1 CA))
movimientoDiagonal :: FichaTablero -> [FichaTablero]
movimientoDiagonal (uni, color, Coord f c) =
  izqAba ++ izqArr ++ derAba ++ derArr
  where
    izqAba = [(uni, color, Coord (pred f) (pred c)) | f > F1, c > CA]
    izqArr = [(uni, color, Coord (pred f) (succ c)) | f > F1, c < CH]
    derAba = [(uni, color, Coord (succ f) (pred c)) | f < F8, c > CA]
    derArr = [(uni, color, Coord (succ f) (succ c)) | f < F8, c < CH]


-- show (movimientoCruz (Rey, Blanca, Coord F1 CA))
movimientoCruz :: FichaTablero -> [FichaTablero]
movimientoCruz (uni, color, Coord f c) =
  izq ++ der ++ aba ++ arr
  where
    izq = [(uni, color, Coord (f) (pred c)) | c > CA]
    der = [(uni, color, Coord (f) (succ c)) | c < CH]
    aba = [(uni, color, Coord (pred f) (c)) | f > F1]
    arr = [(uni, color, Coord (succ f) (c)) | f < F8]

-- ----------------------------------------------------------------------------------

startState :: (Int, Int) -> GameState
startState _ = error "startState has not been implemented!" --TODO

status :: GameState -> GameStatus
status gs = if isFinished gs then Finished else error "no sabemos bien que va aca y no tengo tildes" --TODO

activePlayer :: GameState -> Maybe Player
activePlayer gs = case status gs of
  Turn player -> Just player
  Roll player -> Just player
  Finished -> Nothing

  -- data GameState = GameState Tablero Int Player (Int,Int)                      deriving (Eq) --TODO
  -- data GameAction = DiceRoll Int Int | Move FichaTablero Coordenada | Pasar    deriving (Eq) --TODO

actions :: GameState -> Player -> [GameAction]
actions (GameState tablero _ _ _) player = error "actions has not been implemented!" --TODO
  -- let fichas = [(uni,col,coord) | (uni,col,coord) <- tablero, col==color]
      -- map (\f -> movimiento f  ) fichas

-- ficha, primer dado, segundo dado, tablero
-- estoy en una carga o no?

-- movimiento :: FichaTablero -> (Int,Int) -> Tablero -> [GameAction]
-- movimiento (uni, col, coord) dados tablero = case estoyEncarga dados of
  -- True ->
    -- let puntos = (fst dados) * 2
  -- False -> []


nextState :: GameState -> Player -> GameAction -> GameState
nextState _ _ _ = error "nextState has not been implemented!" --TODO

isFinished :: GameState -> Bool
isFinished (GameState _ 30 _ _ ) = True
isFinished (GameState tablero _ _ _ ) = length [u | (u,col,coor)<-tablero,u==Rey] /= 2 --Si no están los dos reyes el juego terminó

score :: GameState -> Player -> Maybe Int
score _ _ = error "score has not been implemented!" --TODO

-- Presentation ------------------------------------------------------------------------------------

instance Show GameState where
   show _ = error "(Show GameState) has not been implemented!" --TODO

instance Show GameAction where
   show _ = error "(Show GameAction) has not been implemented!" --TODO

instance Read GameAction where
   readsPrec _ = error "(Read GameAction) has not been implemented!" --TODO

-- Match controller --------------------------------------------------------------------------------
type Agent = GameState -> IO GameAction

consoleAgent :: Player -> Agent
consoleAgent _ _ = error "consoleAgent has not been implemented!"

randomAgent :: Player -> Agent
randomAgent _ _ = error "consoleAgent has not been implemented!"

runMatch :: RandomGen r => (Agent, Agent) -> GameState -> r -> IO (Int, Int)
runMatch ags@(agWhite, agBlack) g r = do
   putStrLn (show g)
   case (status g) of
      Finished -> return (fromJust (score g PlayerWhite), fromJust (score g PlayerBlack))
      (Roll p) -> do
         let ((d1, d2), r2) = roll2Dice r
         runMatch ags (nextState g p (DiceRoll d1 d2)) r2
      (Turn p) -> do
         move <- (if p == PlayerWhite then agWhite else agBlack) g
         runMatch ags (nextState g p move) r

runOnConsole :: IO (Int, Int)
runOnConsole = do
   r <- newStdGen
   let (dice, r2) = roll2Dice r
   runMatch (consoleAgent PlayerWhite, consoleAgent PlayerBlack) (startState dice) r2

-- Utility -----------------------------------------------------------------------------------------

roll2Dice :: (RandomGen g) => g -> ((Int, Int), g)
roll2Dice g = ((v1, v2), g2)
   where (v1, g1) = randomR (1, 6) g
         (v2, g2) = randomR (1, 6) g1
