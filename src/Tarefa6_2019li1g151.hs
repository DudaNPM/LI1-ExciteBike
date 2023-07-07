{- |

= Introdução


= Objetivos


= Discussao e conclusao


-}




-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g151 where

import LI11920
import Tarefa2_2019li1g151
import Tarefa4_2019li1g151
import Data.List

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot ib (e@(Estado m js)) | ar   = jogadaAr ib e
                         | chao = jogadaChao ib e
 where
 ar   = not (checkChao ej)
 chao = checkChao ej
 ej   = eJogador ib js





-- | Funcoes executadas quando o bot se encontra no Chao.


-- | Verifica se um bot se encontra no Chao ou nao.
checkChao :: Jogador -> Bool
checkChao (Jogador _ _ _ _ (Chao _)) = True
checkChao _ = False

-- | Devolve a melhor Jogada, dependendo da posicao do bot.
jogadaChao :: Int -> Estado -> Maybe Jogada
jogadaChao ib (e@(Estado m js)) | x < 0.9   = atritoDif ib e d
                                | otherwise = atritoDif ib e (d+1)

 where
 x  = d - (fromIntegral (truncate d)) -- ^ Distancia percorrida pelo bot dentro da peca.
 ej = eJogador ib js                  -- ^ Estado do bot em questao.
 d  = returnDist ej                   -- ^ Distancia que o bot ja percorreu.

 -- | Devolve a melhor jogada, dependendo dos atritos das pecas em questao.
atritoDif :: Int -> Estado -> Double -> Maybe Jogada
atritoDif ib (e@(Estado m js)) d1 | (not daB) && (not daC) = Just Acelera
                                  | (not daB) && a2 < a1   = Just Acelera
                                  | (not daB)              = Just (Movimenta C)
                                  | (not daC) && a2 < a3   = Just Acelera
                                  | (not daC)              = Just (Movimenta B)
                                  |   (b1p)   && p /= 0    = Just (Movimenta C)
                                  |   (bup)   && p /= up   = Just (Movimenta B)
                                  |  a2 <= a1 && a2 <= a3  = Just Acelera
                                  |  a3 <= a1              = Just (Movimenta B)
                                  |  a1 <= a3              = Just (Movimenta C)

 where
 ej  = eJogador ib js                     -- ^ Estado do bot em questao.
 p   = returnPista ej                     -- ^ Pista em que o bot se encontra.
 daB = (checkDifAlturas ib B e)           -- ^ Diferenca de alturas entre a pista atual e a de baixo.
 daC = (checkDifAlturas ib C e)           -- ^ Diferenca de alturas entre a pista atual e a de cima.
 p1  = findPeca ((p-1) , (truncate d1)) m -- ^ Peca acima do bot.
 p2  = findPeca (  p   , (truncate d1)) m -- ^ Peca atual do bot.
 p3  = findPeca ((p+1) , (truncate d1)) m -- ^ Peca abaixo do bot.
 a1  = atrito (ptp p1)                    -- ^ Atrito de p1.
 a2  = atrito (ptp p2)                    -- ^ Atrito de p2.
 a3  = atrito (ptp p3)                    -- ^ Atrito de p3.
 up  = (length m) - 1                     -- ^ Indicador da ultima pista.
 ptp = pecaToPiso                         -- ^ Transforma uma peca no piso correspondente.
 b1p = (ptp (findPeca (0, (truncate d1)) m))  == (Boost) -- ^ Verifica se a primeira pista tem um boost.
 bup = (ptp (findPeca (up, (truncate d1)) m)) == (Boost) -- ^ Verifica se a ultima pista tem um boost.





-- | Funcoes executadas quando o bot se encontra no Ar.


-- | Devolve um indicador (letra), de acordo com as inclinacoes da peca e do jogador.
incBotPeca :: Jogador -> Mapa -> Char
incBotPeca (Jogador p d _ _ (Ar _ i _)) m | i == ip = 'n' -- ^ Nada deve ser feito.
                                          | i  > ip = 'd' -- ^ Inclinar a mota para a frente (Mov..D)
                                          | i  < ip = 'e' -- ^ Inclinar a mota para a tras (Mov..E)
 where
 pa = findPeca (p , truncate d) m -- ^ Peca atual.
 dp = declive d pa                -- ^ Declive da peca atual.
 ip = atan (dp) * (180 / pi)      -- ^ Inclinaçao da peca atual.

-- | De acordo com um indicador, devolve a movimentacao (Esq/Dir) que o bot deve assumir.
jogadaAr :: Int -> Estado -> Maybe Jogada
jogadaAr ib (Estado m js) | x == 'n' = Nothing
                          | x == 'd' = Just (Movimenta D)
                          | x == 'e' = Just (Movimenta E)
 where
 x  = incBotPeca ej m -- ^ Indicador resultante da funcao incBotPeca.
 ej = eJogador ib js  -- ^ Estado do bot em questao.





-- | Funcoes auxiliares.


-- | Devolve o estado de um bot em funcao do bot que o representa.
eJogador :: Int -> [Jogador] -> Jogador
eJogador ib js = (!!) js (ib)

-- | Verifica se a altura entre a peca atual e a peca para a qual o bot se vai movimentar é menor que 0.2.
checkDifAlturas :: Int -> Direcao -> Estado -> Bool
checkDifAlturas ib d (Estado m js) | (d == C)  = difAltC ej m <= 0.2
                                   | otherwise = difAltB ej m <= 0.2
    where 
    ej = eJogador ib js                                          -- ^ Estado do bot em questao.

-- | Devolve a pista onde o bot se encontra.
returnPista :: Jogador -> Int
returnPista (Jogador p _ _ _ _) = p

-- | Devolve a distancia que o bot ja percorreu.
returnDist :: Jogador -> Double
returnDist (Jogador _ d _ _ _) = d

-- | Retorna o piso da respetiva peca.
pecaToPiso :: Peca -> Piso
pecaToPiso (Recta p _)   = p
pecaToPiso (Rampa p _ _) = p

-- | Transforma uma lista de pecas numa lista de pisos.
pistaToPiso :: [Peca] -> [Piso]
pistaToPiso []    = []
pistaToPiso (h:t) = pecaToPiso h : pistaToPiso t