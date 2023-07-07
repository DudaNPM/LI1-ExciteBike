{- |

= Introdução
esta é a tarefa onde é criada toda a interfase grafica 
por completo e os menus assim como defenir as jogadas atravez das tarefas 2 e 4.
A tarefa foi cumprida, exceto a implementação das tarefas 2 e 4.


= Objetivos
O objetivo principal da tarefa 5 é executar o jogo, para tal temos de criar uma interfase gráfica, 
para isso utilizei um programa á parte, o photoshop.
Tendo concluído a criação da interfase gráfica, criei uma funcao (reduzReta)
que ajusta o tamanho de cada imagem ao tamanho que eu quero que ela tenha, 
assim como uma função para desenhar mapa e os jogadores (desenhaestado)
para desenhar o mapa (desenhaMapa/desenhaLinha/desenhaBloco) que recebe a matriz (mapa) 
e a divide ate chegar a cada elemento em específico, 
analizando-o e devolvendo a imagem que corresponde a cada peça.
Para desenhar os jogadores (desenhaJogadores/desenhaJogador) 
que analiza se o jogador esta ou nao no estado Morto e assim devolve a imagem correspondente
nao podendo esquecer da função (reageTempo) que mostra como é que todo o jogo reage a passagem do tempo
Por último temos a função principal (main) que executa das as funções da tarefa.


= Discussao e conclusao
Embora nao tenha conseguido implementar as tarefas 2 e 4 na minha tarefa 5,
nem executar o meu jogo devido a um erro ao executar a função main que fazia com que a janela se fechasse automaticamente,
posso dizer que gostei parcialmente do resultado final da tarefa, principalmente
da interfase grafica criada


-}



-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Tarefa2_2019li1g151
import Tarefa4_2019li1g151
import Tarefa0_2019li1g151
import LI11920

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

comp:: Float --comprimento de cada peça
comp = 80

larg:: Float -- largura de cada peça
larg = 60


type EstadoG = (Estado,[Picture])

estadoInicial :: [Picture] -> EstadoG
estadoInicial li = ((Estado ([[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]]) ([Jogador 0 10 1 5 (Chao True) ])),li)

reduzReta :: Picture -> Picture  -- ajustar o tamanho das texturas do jogo
reduzReta x = scale 0.4 0.4 x


desenhaEstado :: Estado -> [Picture] -> [Picture] 
desenhaEstado (Estado m ((Jogador x d y z w):u)) li = desenhaMapa m ((-700) - d1) ((-200) + d1) texturas ++ desenhaJogadores jogadores ij
                               where
                                jogadores = take 2 ((Jogador x d y z w):u)
                                ij = ((encontraIndiceLista 6 li) : (encontraIndiceLista 7 li) : [])
                                d1 = realToFrac d
                                texturas = li

desenhaJogadores :: [Jogador] -> [Picture] -> [Picture] -- desenha os jogadores pertencentes ao jogo
desenhaJogadores (h:t) li = (desenhaJogador h li) : (desenhaJogadores t li)
desenhaJogadores _ _ = []

desenhaJogador :: Jogador -> [Picture] -> Picture -- escolhe a imagen atribuida ao jogador de acordo com o seu estado
desenhaJogador (Jogador x y _ _ (Morto _)) (mota:morto:[]) = Translate ((-700) + (realToFrac y)) ((-200) - ((realToFrac x)*60)) morto
desenhaJogador (Jogador x y _ _ _) (mota:morto:[]) = Translate ( (-700) + (realToFrac y) ) ( (-200) - ((realToFrac x)*60) ) mota

desenhaMapa :: Mapa -> Float -> Float -> [Picture] -> [Picture] -- monta o mapa
desenhaMapa [] _ _ _ = []
desenhaMapa (h:t) x y texturas = (desenhaLinha h x y texturas) ++ (desenhaMapa t x (y-larg) texturas)

desenhaLinha:: [Peca] -> Float -> Float -> [Picture] -> [Picture] -- monta uma linha
desenhaLinha [] _ _ _ = []
desenhaLinha ((Rampa a b c):t) x y texturas| (c-b) == 1 = (desenhaBloco (Rampa a b c) x y texturas) : (desenhaLinha t (x+comp) (y+1) texturas)
                                           | (c-b) == -1 = (desenhaBloco (Rampa a b c) x y texturas) : (desenhaLinha t (x+comp) (y-1) texturas)
                                           | (c-b) == 2 = (desenhaBloco (Rampa a b c) x y texturas) : (desenhaLinha t (x+comp) (y+2) texturas)
                                           | (c-b) == -2 = (desenhaBloco (Rampa a b c) x y texturas) : (desenhaLinha t (x+comp) (y-2) texturas)
desenhaLinha (h:t) x y texturas = (desenhaBloco h x y texturas) : (desenhaLinha t x y texturas)

desenhaBloco:: Peca -> Float -> Float -> [Picture] -> Picture -- atribui a imagem certa a cada peça do mapa
desenhaBloco (Recta a b) xInicial y (retaT:retaB:retaC:retaL:retaR:mota:morto:sobe1T:sobe2T:desce1T:desce2T:sobe1B:sobe2B:desce1B:desce2B:sobe1C:sobe2C:desce1C:desce2C:sobe1L:sobe2L:desce1L:desce2L:sobe1R:sobe2R:desce1R:desce2R:[]) | a == Terra = Translate x y (reduzReta retaT)
                                     | a == Relva = Translate x y (reduzReta retaR)
                                     | a == Boost = Translate x y (reduzReta retaB)
                                     | a == Cola = Translate x y (reduzReta retaC)
                                     | a == Lama = Translate x y (reduzReta retaL)
desenhaBloco (Rampa a b c) x y (retaT:retaB:retaC:retaL:retaR:mota:morto:sobe1T:sobe2T:desce1T:desce2T:sobe1B:sobe2B:desce1B:desce2B:sobe1C:sobe2C:desce1C:desce2C:sobe1L:sobe2L:desce1L:desce2L:sobe1R:sobe2R:desce1R:desce2R:[]) | (c-b) == 1 && a == Terra = Translate x y sobe1T
                                        | (c-b) == 1 && a == Boost = Translate x y (reduzReta sobe1B)
                                        | (c-b) == 1 && a == Relva = Translate x y (reduzReta sobe1R)
                                        | (c-b) == 1 && a == Cola = Translate x y (reduzReta sobe1C)
                                        | (c-b) == 1 && a == Lama = Translate x y (reduzReta sobe1L)
                                        | (c-b) == -1 && a == Terra = Translate x y (reduzReta desce1T)
                                        | (c-b) == -1 && a == Boost = Translate x y (reduzReta desce1B)
                                        | (c-b) == -1 && a == Relva = Translate x y (reduzReta desce1R)
                                        | (c-b) == -1 && a == Cola = Translate x y (reduzReta desce1C)
                                        | (c-b) == -1 && a == Lama = Translate x y (reduzReta desce1L)
                                        | (c-b) == 2 && a == Terra = Translate x y (reduzReta sobe2T)
                                        | (c-b) == 2 && a == Boost = Translate x y (reduzReta sobe2B)
                                        | (c-b) == 2 && a == Relva = Translate x y (reduzReta sobe2R)
                                        | (c-b) == 2 && a == Cola = Translate x y  (reduzReta sobe2C)
                                        | (c-b) == 2 && a == Lama = Translate x y  (reduzReta sobe2L)
                                        | (c-b) == -2 && a == Terra = Translate x y (reduzReta desce2T)
                                        | (c-b) == -2 && a == Boost = Translate x y (reduzReta desce2B)
                                        | (c-b) == -2 && a == Relva = Translate x y (reduzReta desce2R)
                                        | (c-b) == -2 && a == Cola = Translate x y (reduzReta desce2C)
                                        | (c-b) == -2 && a == Lama = Translate x y (reduzReta desce2L)



reageTempo :: Float -> Estado -> Estado
reageTempo n (Estado m ((Jogador a b c d e) : t)) = (Estado m ((Jogador a (b+2) c d e) : t))

reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s -- ignora qualquer outro evento

reageEventoG :: Event -> EstadoG -> EstadoG
reageEventoG ev (e,li) = (reageEvento ev e, li) -- ignora qualquer outro evento

reageTempoG :: Float -> EstadoG -> EstadoG
reageTempoG t (e,li) = (reageTempo t e, li)

desenhaEstadoG :: EstadoG -> Picture
desenhaEstadoG (e,li) = Pictures[fundo,estado]
                         where estado = Pictures(desenhaEstado e stateImages)
                               stateImages = tail li
                               fundo = head li




fr :: Int -- frames por segundo
fr = 50

dm :: Display -- tamanho da janela
dm = FullScreen
--InWindow "Novo Jogo" (800, 800) (0, 0)



main :: IO () -- executar o jogo
main = do 
    Just fundo <- loadJuicy "imagens/fundo.jpg"
    Just retaT <- loadJuicy "imagens/retaT.jpg"
    Just retaB <- loadJuicy "imagens/retaB.jpg"
    Just retaC <- loadJuicy "imagens/retaC.jpg"
    Just retaL <- loadJuicy "imagens/retaL.jpg"
    Just retaR <- loadJuicy "imagens/retaR.jpg"  
    Just mota <- loadJuicy "imagens/dilacerator.png"
    Just morto <- loadJuicy "imagens/morto.png"
    Just sobe1T <- loadJuicy "imagens/sobe1T.png"
    Just sobe2T <- loadJuicy "imagens/sobe2T.png"
    Just desce1T <- loadJuicy "imagens/desce1T.png"
    Just desce2T <- loadJuicy "imagens/desce2T.png"
    Just sobe1C <- loadJuicy "imagens/sobe1C.png"
    Just sobe2C <- loadJuicy "imagens/sobe2C.png"  
    Just desce1C <- loadJuicy "imagens/desce1C.png"  
    Just desce2C <- loadJuicy "imagens/desce2C.png"
    Just sobe1R <- loadJuicy "imagens/sobe1R.png"  
    Just sobe2R <- loadJuicy "imagens/sobe2R.png"  
    Just desce1R <- loadJuicy "imagens/desce1R.png"  
    Just desce2R <- loadJuicy "imagens/desce2R.png"  
    Just sobe1B <- loadJuicy "imagens/sobe1B.png"
    Just sobe2B <- loadJuicy "imagens/sobe2B.png"  
    Just desce1B <- loadJuicy "imagens/desce1B.png"  
    Just desce2B <- loadJuicy "imagens/desce2B.png" 
    Just sobe1L <- loadJuicy "imagens/sobe1L.png"  
    Just sobe2L <- loadJuicy "imagens/sobe2L.png"  
    Just desce1L <- loadJuicy "imagens/desce1L.png"  
    Just desce2L <- loadJuicy "imagens/desce2L.png"  
    play dm              -- janela onde irá correr o jogo
        (greyN 0.5)     -- côr do fundo da janela
        fr              -- frame rate
        (estadoInicial [fundo,mota,retaT,retaB,retaC,retaL,retaR,mota,morto,sobe1T,sobe2T,desce1T,desce2T,sobe1B,sobe2B,desce1B,desce2B,sobe1C,sobe2C,desce1C,desce2C,sobe1L,sobe2L,desce1L,desce2L,sobe1R,sobe2R,desce1R,desce2R])  -- estado inicial
        desenhaEstadoG   -- desenha o estado do jogo
        reageEventoG     -- reage a um evento
        reageTempoG      -- reage ao passar do tempo






