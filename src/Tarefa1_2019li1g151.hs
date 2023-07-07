{- |

= Introdução



= Objetivos


= Discussao e conclusao



-}




-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g151 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(4,20,2),(6,25,5),(10,20,3),(3,10,1),(3,10,4),(3,10,6),(3,10,9),(3,10,12),(4,30,10),
            (4,40,14),(4,40,19),(4,35,18),(4,40,18),(4,45,21)]

-- * Funções pré-definidas da Tarefa 1.

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

-- | Gera "c-1" pares de numeros aleatorios para uma pista com um determinado comprimento (c).
gApista :: Int -> [Int] -> [(Int,Int)]
gApista _ [] = []
gApista 1 _ = []
gApista c (h:hs:hss) = (h,hs):(gApista (c-1) hss)

-- | Gera "p" listas de "c-1" pares de numeros aleatorios para um mapa com um "p" pistas.
gAmapa :: Int -> Int -> [Int] -> [[(Int,Int)]]
gAmapa 0 _ _ = []
gAmapa p c l = gApista c l : gAmapa (p-1) c (drop ((2*c)-2) l)

-- | Gera um piso de acordo com a gama (numero atribuido aleatoriamente pelas funcoes "gApista" e "gAmapa")
geraPiso :: Int -> Piso -> Piso
geraPiso g1 pa | elem g1 [0,1] = Terra
               | elem g1 [2,3] = Relva
               | elem g1 [4] = Lama
               | elem g1 [5] = Boost
               | otherwise = pa

-- | Gera uma peca de acordo com a gama representada pelo par de numeros (atruibuido aleatoriamente),
-- | a altura da peca anterior (aa) e o tipo de piso anterior (pa), recorrendo a funcao "geraPiso".
geraPeca :: (Int,Int) -> Int -> Piso -> Peca
geraPeca (g1,g2) aa pa | elem g2 [0,1] = Rampa (geraPiso g1 pa) (aa) (aa+g2+1)
                       | elem g2 [2..5] && aa == 0 = Recta (geraPiso g1 pa) (0)
                       | elem g2 [2..5] && aa /= 0 = Rampa (geraPiso g1 pa) (aa) (alturaFinal aa g2)
                       | otherwise = Recta (geraPiso g1 pa) (aa)
 where
  alturaFinal aa g2 | aa - (g2 - 1) < 0 = 0
                    | otherwise = aa - (g2 - 1)

-- | Gera uma pista de acordo com uma lista de pares de numeros (gama) e a altura (aa) e piso (pa) anterior de cada peca,
-- | recorrendo a funcao "geraPista". As incognitas "aa1" e "pa1" atualizam a altura e o piso anterior de cada peca.
geraPista :: [(Int,Int)] -> Int -> Piso -> Pista
geraPista [] _ _ = []
geraPista (p:xs) aa pa = peca : (geraPista (xs) (aa1 (peca)) (pa1 (peca)))
 where
  peca = geraPeca p aa pa
  aa1 (Rampa _ x y) = y
  aa1 (Recta _ z) = z
  pa1 (Rampa x _ _) = x
  pa1 (Recta y _) = y

-- | Gera uma lista de pistas (Mapa) recorendo a funcao "geraPista" e tendo em conta que cada pista comeca com a peca "Recta Terra 0"
geraPistas :: [[(Int,Int)]] -> Mapa
geraPistas [] = []
geraPistas (p:ps) = (Recta Terra 0 : p1) : geraPistas (ps)
 where
  p1 = geraPista p 0 Terra

-- | Gera um Mapa de acordo com o numero de pistas, o comprimento de cada uma e uma seed que recorrendo a funcao "geraAleatorios"
-- | gera uma lista com n elementos aleatorios.
gera :: Int -> Int -> Int -> Mapa
gera p c s = geraPistas (gAmapa (p) (c) (l))
 where
  l = geraAleatorios n s
    where
     n = p*(2*(c - 1))