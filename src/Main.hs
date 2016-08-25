module Main where

import Data.List (intercalate)
import Data.Array

data Fil = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8                             deriving (Show, Eq, Ord)
data Col = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8                             deriving (Show, Eq, Ord)
data Color = Blanca | Negra                                                  deriving (Show, Eq, Ord)
data Unidad = Rey| Mago | Bufo | Arco | Caballo | Pica | Espada | Hacha      deriving (Show, Eq, Ord)

data Coord = Coord { fila :: Fil
                   , columna :: Col
                   }      deriving (Show, Eq, Ord)

data FichasOld = Fichas { color :: Color
                     , enjuego :: [(Unidad,Coord)]
                     , afuera :: [Unidad]
                     }

main :: IO ()
main = putStr (showBoard fichasEstado1)

-- Unidades del Juego - 15 fichas
unidades :: [Unidad]
unidades = [Hacha, Espada, Pica, Caballo, Arco, Bufo, Mago, Rey]

--Devuelve los enemigos de una unidad, verificar el color opuesto
gana :: Unidad -> [Unidad]
gana uni
 | uni == Hacha = [Arco, Bufo, Mago]
 | uni == Espada = [Hacha, Bufo, Mago]
 | uni == Pica = [Hacha, Espada, Caballo]
 | uni == Caballo = [Hacha, Espada, Arco]
 | uni == Arco = [Espada, Pica, Bufo]
 | uni == Bufo = [Pica, Caballo, Mago]
 | uni == Mago = [Pica, Caballo, Arco]
 | otherwise = unidades --rey

pierde :: Unidad -> [Unidad]
pierde uni
 | uni == Hacha = [Espada, Pica, Caballo]
 | uni == Espada = [Pica, Caballo, Arco]
 | uni == Pica = [Arco, Bufo, Mago]
 | uni == Caballo = [Pica, Bufo, Mago]
 | uni == Arco = [Hacha,Caballo, Mago]
 | uni == Bufo = [Hacha,Espada, Arco]
 | uni == Mago = [Hacha, Espada, Arco]
 | otherwise = unidades --rey

colorGana :: (Color, Unidad) -> (Color,[Unidad])
colorGana (Negra, uni) = (Blanca, gana uni)
colorGana (Blanca, uni) = (Negra, gana uni)

colorPierde :: (Color,Unidad) -> (Color,[Unidad])
colorPierde (Negra, uni) = (Blanca, pierde uni)
colorPierde (Blanca, uni) = (Negra, pierde uni)

extract3 :: (a, b, c) -> c
extract3 (_,_,c) = c

-- Busca una Unidad en posicion dada, deberia ser una lista de 1, o una [], TODO devolver un Maybe
buscarPosicion :: [(Unidad,Color,Coord)] -> Coord -> [(Unidad,Color)]
buscarPosicion fichas Coord{fila=f ,columna=c} =
  map (\(u,c,_) -> (u,c)) (filter (\ficha -> extract3 ficha == Coord { fila = f, columna = c}) fichas)

fichaStrPocision :: [(Unidad,Color,Coord)] -> Coord -> Char
fichaStrPocision fichas coord
  | length lista  == 1 = fichaStr (head lista)
  | null lista = ' '
  where lista = buscarPosicion fichas coord

--Blancas son mayusculas, negras minusculas
fichaStr ::(Unidad , Color) -> Char
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

--putStr (showBoard fichasEstado1)
showBoard :: [(Unidad,Color,Coord)] -> String
showBoard fichas =
 emptyRow ++
 "|    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |\n" ++
 emptyRow ++
 row "1" (row2 F1 fichas) ++
 row "2" (row2 F2 fichas) ++
 row "3" (row2 F3 fichas) ++
 row "4" (row2 F4 fichas) ++
 row "5" (row2 F5 fichas) ++
 row "6" (row2 F6 fichas) ++
 row "7" (row2 F7 fichas) ++
 row "8" (row2 F8 fichas)
 where
  emptyRow = "+----+---+---+---+---+---+---+---+---+\n"
  row n x = "| " ++ n ++ "  | " ++ intercalate " | " x ++ " |\n" ++ emptyRow


row2 :: Fil -> [(Unidad,Color,Coord)] -> [[Char]]
row2  f fichas = [[fichaStrPocision fichas Coord{ fila = f, columna = C1}],
         [fichaStrPocision fichas Coord{ fila = f, columna = C2}],
         [fichaStrPocision fichas Coord{ fila = f, columna = C3}],
         [fichaStrPocision fichas Coord{ fila = f, columna = C4}],
         [fichaStrPocision fichas Coord{ fila = f, columna = C5}],
         [fichaStrPocision fichas Coord{ fila = f, columna = C6}],
         [fichaStrPocision fichas Coord{ fila = f, columna = C7}],
         [fichaStrPocision fichas Coord{ fila = f, columna = C8}]]


-- 15 unidades blancas iniciales, hacer un algoritmo de como poner las fichas
-- llamar de esta forma, putStr (showBoard fichasEstado1)
-- type Fichas = [(Unidad,Color,Coord)]
fichasEstado1 :: [(Unidad,Color,Coord)]
fichasEstado1 = [
   (Hacha, Blanca, Coord{ fila = F2, columna = C1})
 , (Hacha, Negra, Coord{ fila = F1, columna = C2})
 , (Espada, Negra, Coord{ fila = F3, columna = C8})
 , (Espada, Blanca, Coord{ fila = F4, columna = C2})
 , (Rey, Negra, Coord{ fila = F5, columna = C7})
 , (Rey, Negra, Coord{ fila = F6, columna = C2})
 , (Bufo, Blanca, Coord{ fila = F7, columna = C4})
 , (Mago, Negra, Coord{ fila = F8, columna = C5})
 ]
