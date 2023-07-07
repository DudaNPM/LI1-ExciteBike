{- |

= Introdução



= Objetivos


= Discussao e conclusao


-}


-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g151 where

import LI11920
import Tarefa0_2019li1g151
import Tarefa1_2019li1g151

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [(gera 2 5 1),(gera 2 20 3),(gera 4 20 2),(gera 4 20 4),(gera 2 10 5)]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.


-- | Junçao das 'Instruçoes' dadas aos bulldozers agrupando-as segundo padroes horizontais.
desconstroi :: Mapa -> Instrucoes
desconstroi [] = []
desconstroi m = repeteH (auxrH (desconstroi1 0 m))

-- | Delvolve as 'Instrucoes' de desconstruçao de uma pista de cada vez.
desconstroi1 :: Int -> Mapa -> Instrucoes
desconstroi1 _ [] = []
desconstroi1 p (a:b) = desconstroiPista p a ++ desconstroi1 (p + 1) b

-- | Devolve uma instruçao de acordo com a peça e a pista onde esta se encontra.
desconstroiPeca :: Int -> Peca -> Instrucao
desconstroiPeca p (Rampa z x y) | x < y = Sobe [p] z (y - x)
                                | x > y = Desce [p] z (x - y)
desconstroiPeca p (Recta z _) = Anda [p] z

-- | Devolve uma lista de 'Instrucao' de acordo com as caracteristicas de cada peça e o nº da pista.
desconstroiPista :: Int -> Pista -> Instrucoes
desconstroiPista _ [] = []
desconstroiPista _ [a] = []
desconstroiPista p (a:b:c) = desconstroiPeca p b : desconstroiPista p (a:c)

-- | Agrupa elementos iguais e consecutivos de uma lista.
group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = insere h (group t)

insere :: Eq a => a -> [[a]] -> [[a]]
insere h [] = [[h]]
insere h (y:ys) = if elem h y
                  then (h:y):ys
                  else [h]:(y:ys)

-- | Agrupa 'Instrucao' iguais e consecutivas de uma lista.
auxrH :: Instrucoes -> [Instrucoes]
auxrH i = group i

-- | Altera as listas com mais de uma 'Instruçao' igual para uma 'Instruçao' do tipo Repete _ _.
repeteH :: [Instrucoes] -> Instrucoes
repeteH [] = []
repeteH (a:b) | length a > 1 = Repete (length a) [head a] : repeteH b
              | otherwise = [head a] ++ repeteH b




--comp :: Mapa -> Int
--comp [] = 0
--comp m = length (head m)
--
--auxrV1 :: Int -> Mapa -> Instrucoes -> [Instrucoes]
--auxrV1 _ [] _ = []
--auxrV1 _ _ [] = []
--auxrV1 i m l | i == np - 1 = []
--             | otherwise = ( [encontraIndiceLista i l , encontraIndiceLista (np - 1 + i) l] : auxrV1 i m (drop (2*np - 2) l) ) ++ auxrV1 (i + 1) m l
-- where
--  np = comp m     -- > Numero de peças de cada pista
--
--auxrV2 :: Instrucao -> Instrucao -> Bool
--auxrV2 (Anda _ p1) (Anda _ p2) = p1 == p2
--auxrV2 (Sobe _ p1 a1) (Sobe _ p2 a2) = p1 == p2 && a1 == a2
--auxrV2 (Desce _ p1 a1) (Desce _ p2 a2) = p1 == p2 && a1 == a2
----auxrV2 (Repete n Instrucoes) (Repete n Instrucoes) =
--auxrV2 _ _ = False
--
--repeteVertical :: [Instrucoes] -> [Instrucoes]
--repeteVertical [] = []
--repeteVertical (i:is) | ip1 == ip2 = Repete 