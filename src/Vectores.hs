module Vectores where

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.Array

-- ---------------------------------------------------------------------
-- Tipos de los vectores y de las matrices                            --
-- ---------------------------------------------------------------------

-- Los vectores son tablas cuyos índices son números naturales.
type Vector a = Array Int a

-- Las matrices son tablas cuyos índices son pares de números naturales
type Matriz a = Array (Int,Int) a

-- ---------------------------------------------------------------------
-- (listaVector xs) es el vector correspondiente a la lista xs.
--  ghci> listaVector [3,2,5]
--  array (1,3) [(1,3),(2,2),(3,5)]
-- ---------------------------------------------------------------------
listaVector :: Num a => [a] -> Vector a
listaVector xs = listArray (1,n) xs
    where n = length xs

-- ---------------------------------------------------------------------
-- (listaMatriz xss) es la matriz cuyas filas son los elementos de xss.
--  ghci> listaMatriz [[1,3,5],[2,4,7]]
--  array ((1,1),(2,3))
--        [((1,1),1),((1,2),3),((1,3),5),((2,1),2),((2,2),4),((2,3),7)]
-- ---------------------------------------------------------------------

listaMatriz :: Num a => [[a]] -> Matriz a
listaMatriz xss = listArray ((1,1),(m,n)) (concat xss)
    where m = length xss
          n = length (head xss)
