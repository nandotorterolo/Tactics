module Main where

import Data.List (intercalate)
import Data.Maybe
import Data.Char
import Control.Arrow ((&&&))
import System.Random

data Player = PlayerWhite | PlayerBlack                                      deriving (Eq, Show, Enum)
data GameStatus = Turn Player | Roll Player | Finished                       deriving (Eq, Show)
-- Tablero, NroTurno,JugadorActivo, sumaDados, cargaHabilitada
data GameState = GameState Tablero Int Player Int Bool                       deriving (Eq)
data GameAction = DiceRoll Int Int | Move FichaTablero Coordenada | Skip     deriving (Eq)

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

colorGana :: Ficha -> [Ficha]
colorGana (unidad, Negra) =  map (\u -> (u, Blanca)) (gana unidad)
colorGana (unidad, Blanca) = map (\u ->(u, Negra)) (gana unidad)

colorPierde :: Ficha -> [Ficha]
colorPierde (unidad, Negra) = map (\u -> (u, Blanca)) (pierde unidad)
colorPierde (unidad, Blanca) = map (\u ->(u, Negra)) (pierde unidad)

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

strAFicha :: Char -> Ficha
strAFicha 'H' = (Hacha, Blanca)
strAFicha 'h' = (Hacha, Negra)
strAFicha 'E' = (Espada,Blanca)
strAFicha 'e' = (Espada,Negra)
strAFicha 'P' = (Pica,Blanca)
strAFicha 'p' = (Pica,Negra)
strAFicha 'C' = (Caballo,Blanca)
strAFicha 'c' = (Caballo,Negra)
strAFicha 'A' = (Arco,Blanca)
strAFicha 'a' = (Arco,Negra)
strAFicha 'B' = (Bufo,Blanca)
strAFicha 'b' = (Bufo,Negra)
strAFicha 'M' = (Mago,Blanca)
strAFicha 'm' = (Mago,Negra)
strAFicha 'R' = (Rey,Blanca)
strAFicha 'r' = (Rey,Negra)


fila :: Char -> Fil
fila x = if (x > '0') && ( x < '9') then
  [F1 , F2 , F3 , F4 , F5 , F6 , F7 , F8]!!(ord x -49) else
    error "fila no válida "


columna :: Char -> Col
columna x = if x > '@' && x<'I' then
  [CA , CB , CC , CD , CE , CF , CG , CH]!!(ord x -65) else
    error "columna no válida"


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

puedeIr :: Tablero -> Ficha -> Coordenada -> Bool
puedeIr t ficha c = resultado
  where
    resultado = case fichaCoordenada t c of
      Nothing -> True
      Just f@(uni,col) -> f `elem` colorPierde ficha

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

-- la que voy a mover, a donde voy a mover, tableroViejo
moverFicha :: FichaTablero -> Coordenada -> Tablero -> Tablero
moverFicha ft@(uni,color,coord) destino t = (uni,color,destino) : [ficha | ficha@(uniEnT,colEnT,coordEnT) <- t, ficha /= ft, coordEnT/=destino]

-- nuestas logicas ------------------------------------------------------------------------------------

-- show (movimientoDiagonal (Rey, Blanca, Coord F1 CA))
movimientoDiagonal :: FichaTablero -> [GameAction]
movimientoDiagonal ft@(uni, color, Coord f c) =
  izqAba ++ izqArr ++ derAba ++ derArr
  where
    izqArr = [Move ft (Coord (pred f) (pred c)) | f > F1, c > CA]
    izqAba = [Move ft (Coord (pred f) (succ c)) | f > F1, c < CH]
    derArr = [Move ft (Coord (succ f) (pred c)) | f < F8, c > CA]
    derAba = [Move ft (Coord (succ f) (succ c)) | f < F8, c < CH]

-- show (movimientoOrtogonal (Rey, Blanca, Coord F1 CA))
movimientoOrtogonal :: FichaTablero -> [GameAction]
movimientoOrtogonal ft@(uni, color, Coord f c) =
  izq ++ der ++ aba ++ arr
  where
    izq = [Move ft (Coord f (pred c)) | c > CA]
    der = [Move ft (Coord f (succ c)) | c < CH]
    arr = [Move ft (Coord (pred f) c) | f > F1]
    aba = [Move ft (Coord (succ f) c) | f < F8]

-- show (movimientoCarga (Rey, Blanca, Coord F1 CA))
movimientoCarga :: FichaTablero -> [GameAction]
movimientoCarga ft@(uni, Negra, Coord f c) = [Move ft (Coord (succ (succ f)) c) | f <= F6]
movimientoCarga ft@(uni, Blanca, Coord f c) = [Move ft (Coord (pred (pred f)) c) | f >= F2]

movimientosPosibles :: GameState -> FichaTablero -> [GameAction]
movimientosPosibles (GameState tablero _ _ puntos carga) ft@(uni, color, coord) =
  ortogonales ++ diagonales ++ cargas
  where
    ortogonales = [mov | mov@(Move _ coord) <- movimientoOrtogonal ft, puntos >= 2, puedeIr tablero (uni,color) coord]
    diagonales  = [mov | mov@(Move _ coord) <- movimientoDiagonal ft,  puntos >= 3, puedeIr tablero (uni,color) coord]
    cargas = [mov | mov@(Move _ coord) <- movimientoCarga ft, carga, puedeIr tablero (uni,color) coord]

-- dado un tablero y un jugador, devuelve todas sus fichas
fichasDelJugador :: Tablero -> Player -> [FichaTablero]
fichasDelJugador tablero p = case p of
  PlayerWhite -> [fichas | fichas@(unidad,color,coord) <- tablero, color==Blanca]
  PlayerBlack -> [fichas | fichas@(unidad,color,coord) <- tablero, color==Negra]
------------------------------------------------------------------------------------

startState :: (Int, Int) -> IO GameState
startState (d1,d2) = do
  let carga = d1 == d2
  let suma = if (carga) then (d1+d2)*2 else (d1+d2)
  tablero <- crearTableroCompleto todasLasUnidades []
  return (GameState tablero 0 PlayerWhite suma carga)

-- verificar el costo minimo de las actions
status :: GameState -> GameStatus
status gs@(GameState t numTurno p puntos carga ) =
  if isFinished gs then Finished
  else if (puntos > 2) then Turn p
    else Roll (otroPlayer p)

otroPlayer :: Player -> Player
otroPlayer p = if p == PlayerWhite then PlayerBlack else PlayerWhite


costoMovimiento :: GameAction -> Int
costoMovimiento (Move ft@(uni,col,Coord f c) (Coord fdest cdest)) =
  if (f/=fdest && c/=cdest) then 3
    else if (abs(fromEnum fdest - fromEnum f) == 2 ) then 4
      else 2
costoMovimiento _ = 0

-- porque GameState
activePlayer :: GameState -> Maybe Player
activePlayer gs = case status gs of
  Turn player -> Just player
  Roll player -> Just player
  Finished -> Nothing

-- El que esta adentro de GameState es el activo
-- el que me pasan es para el que estan preguntando las actions
actions :: GameState -> Player -> [GameAction]
actions gs@(GameState tablero _ p _ _) player = if (p == player) then concatMap (movimientosPosibles gs) (fichasDelJugador tablero player) else []


nextState :: GameState -> Player -> GameAction -> GameState
nextState (GameState t i p puntos carga) player action = case action of
  Skip ->
    GameState t (i+1) p 0 carga
  (DiceRoll d1 d2) ->
    let esCarga = d1 == d2 in
      let suma = if (esCarga) then (d1+d2)*2 else (d1+d2) in
        GameState t (i+1) player suma esCarga

  (Move ft coord) ->  -- bajar puntos, quitar del tablero la ficha ft, ponerla en coord
    let tableroNuevo = moverFicha ft coord t in
      let puntosNuevo = puntos - costoMovimiento action in
        GameState tableroNuevo (i+1) p puntosNuevo carga

isFinished :: GameState -> Bool
isFinished (GameState _ 30 _ _ _) = True
isFinished (GameState tablero _ _ _ _) = length [u | (u,col,coor)<-tablero,u==Rey] /= 2 --Si no están los dos reyes el juego terminó

score :: GameState -> Player -> Maybe Int
score _ _ = Just 0  -- TODO

-- Presentation ------------------------------------------------------------------------------------

instance Show GameState where
   show (GameState t turno p puntos carga) = "Turno: "++ show turno ++ "\n"
                                          ++ "Jugador: "++ show p ++ "\n"
                                          ++ "Puntos: "++ show puntos ++"\n"
                                          ++ "Carga: "++ show carga ++ "\n"
                                          ++ showBoard t



instance Show GameAction where
  show (DiceRoll d1 d2) = "DiceRoll: " ++ show d1 ++ show d2
  show (Move ft c) = "Move: " ++ show ft ++ show c
  show (Skip) = "Skip"

readAction :: String -> Maybe GameAction
readAction txt@[u,x,y,z,w] = do
  let desde = Coord (fila x) (columna y)
  let hacia = Coord (fila z) (columna w)
  let (unidad, color) = strAFicha u
  let ft = (unidad,color,desde)
  Just (Move ft hacia)
readAction "" = Just Skip
readAction _ = Nothing

-- Match controller --------------------------------------------------------------------------------
type Agent = GameState -> IO GameAction

-- controlar que acciones puede hacer, llamar recursivo
consoleAgent :: Player -> Agent
consoleAgent p = \gs -> do
  linea <- getLine
  case (readAction linea) of
    Just act@(Move ft@(_,c,_) _) ->
      if ((c == Blanca && p == PlayerWhite ) || (c == Negra && p == PlayerBlack)) then
        return act
      else consoleAgent p gs
    Just Skip -> return Skip
    Nothing -> consoleAgent p gs

--
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

-- pasar
runOnConsole :: IO (Int, Int)
runOnConsole = do
   r <- newStdGen
   let (dice, r2) = roll2Dice r
   estado <- startState dice
   runMatch (consoleAgent PlayerWhite, consoleAgent PlayerBlack) estado r2

-- Utility -----------------------------------------------------------------------------------------

roll2Dice :: (RandomGen g) => g -> ((Int, Int), g)
roll2Dice g = ((v1, v2), g2)
   where (v1, g1) = randomR (1, 6) g
         (v2, g2) = randomR (1, 6) g1
