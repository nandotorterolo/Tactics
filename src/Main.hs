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

                    --  blancasIniciales =
                    --    Fichas
                    --    { color = Blanca
                    --    , enjuego = [ (Hacha, Coord { fila = F2, columna = C1})
                    --                , (Hacha, Coord { fila = F3, columna = C2})
                    --                , (Espada, Coord { fila = F2, columna = C3})
                    --                , (Espada, Coord { fila = F3, columna = C4})
                    --                , (Pica, Coord { fila = F1, columna = C5})
                    --                , (Pica, Coord { fila = F2, columna = C6})
                    --                , (Caballo, Coord { fila = F3, columna = C7})
                    --                , (Caballo, Coord { fila = F2, columna = C1})
                    --                , (Arco, Coord { fila = F2, columna = C3})
                    --                , (Arco, Coord { fila = F3, columna = C4})
                    --                , (Bufo, Coord { fila = F1, columna = C1})
                    --                , (Bufo, Coord { fila = F2, columna = C5})
                    --                , (Mago, Coord { fila = F1, columna = C4})
                    --                , (Mago, Coord { fila = F3, columna = C8})
                    --                , (Rey, Coord { fila = F2, columna = C1})
                    --                ]
                    --    , afuera = []
                    --    }

main :: IO ()
main = putStr (showGame "abcdefghi")

-- Unidades del Juego - 15 fichas
unidades :: [Unidad]
unidades = [Hacha, Hacha, Espada, Espada, Pica, Pica, Caballo, Caballo, Arco, Arco, Bufo, Bufo, Mago, Mago, Rey]

-- 15 unidades blancas iniciales, hacer un algoritmo de como poner las fichas
-- type Fichas = [(Unidad,Color,Coord)]
fichasEstado1 :: [(Unidad,Color,Coord)]
fichasEstado1 = [
   (Hacha, Blanca, Coord{ fila = F2, columna = C1})
 , (Hacha, Negra, Coord{ fila = F1, columna = C2})
 ]

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

-- format a game board for on-screen printing
-- La informacion que se le pase a esta funcion es la impresa en el tablero
showGame :: String -> String
showGame [a,b,c,d,e,f,g,h,i] =
 emptyRow ++
 "|    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |\n" ++
 emptyRow ++
 row "1" [[a],[b],[c]] ++
 row "2" [[d],[e],[f]] ++
 row "3" [[g],[h],[i]] ++
 row "4" [[a],[b],[c]] ++
 row "5" [[d],[e],[f]] ++
 row "6" [[g],[h],[i]] ++
 row "7" [[d],[e],[f]] ++
 row "8" [[g],[h],[i]]
 where
  emptyRow = "+----+---+---+---+\n"
  row n x = "| " ++ n ++ "  | " ++ intercalate " | " x ++ " |\n" ++ emptyRow

-- showGame :: [Unidad] -> [Unidad] -> String
