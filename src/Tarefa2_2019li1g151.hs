{- |

= Introdução



= Objetivos



= Discussao e conclusao



-}


-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g151 where

import LI11920
import Tarefa0_2019li1g151
import Tarefa1_2019li1g151

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,Acelera,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Acelera,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Chao False),Jogador 1 10 1 5 (Chao True)]))),
            (0,Acelera,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Morto 1.0),Jogador 1 10 1 5 (Chao True)]))),
            (0,Desacelera,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Desacelera,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Chao False),Jogador 1 10 1 5 (Chao True)]))),
            (0,Desacelera,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Morto 1.0),Jogador 1 10 1 5 (Chao True)]))),
            (0,Dispara,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Dispara,(Estado (gera 2 20 3) ([Jogador 0 7 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Dispara,(Estado (gera 2 20 3) ([Jogador 0 0.5 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Dispara,(Estado (gera 2 20 3) ([Jogador 0 10 1 0 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Dispara,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Morto 1.0),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta B,(Estado (gera 2 20 3) ([Jogador 0 10 1 5 (Morto 1.0),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta D,(Estado (gera 2 20 3) ([Jogador 0 13.5 1 5 (Ar 0.5 45 0),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta D,(Estado (gera 2 20 3) ([Jogador 0 13.7 1 5 (Ar 0.3 (-80) 0),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta E,(Estado (gera 2 20 3) ([Jogador 0 13.5 1 5 (Ar 0.5 45 0),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta E,(Estado (gera 2 20 3) ([Jogador 0 13.7 1 5 (Ar 0.3 80 0),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta B,(Estado (gera 2 20 3) ([Jogador 1 5.5 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta B,(Estado (gera 2 20 3) ([Jogador 0 5.5 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta B,(Estado (gera 2 20 3) ([Jogador 0 16.5 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta B,(Estado (gera 2 20 3) ([Jogador 0 13.05 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta B,(Estado (gera 2 20 3) ([Jogador 0 7.95 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),            
            (0,Movimenta C,(Estado (gera 2 20 3) ([Jogador 0 5.5 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta C,(Estado (gera 2 20 3) ([Jogador 1 16.5 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta C,(Estado (gera 2 20 3) ([Jogador 1 13.5 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta C,(Estado (gera 2 20 3) ([Jogador 1 13.05 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)]))),
            (0,Movimenta C,(Estado (gera 2 20 3) ([Jogador 1 7.95 1 5 (Chao True),Jogador 1 10 1 5 (Chao True)])))]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

jogada n (Movimenta x) (Estado m js) = Estado m js'
 where js' = atualizaIndiceLista n (mov x (encontraIndiceLista n js) m) js

jogada n Acelera (Estado m js) = Estado m js'
 where js' = atualizaIndiceLista n (aceleraj (encontraIndiceLista n js)) js

jogada n Desacelera (Estado m js) = Estado m js'
 where js' = atualizaIndiceLista n (desaceleraj (encontraIndiceLista n js)) js

jogada n Dispara (Estado m js) = fire n (encontraIndiceLista n js) (Estado m js)



-- | Modifica o estado de um jogador em funcao da sua jogada 
-- |
-- | mov B >> Altera um jogador da pista 'p' para a pista 'p+1' se : este nao estiver na ultima pista; estiver no Chao; e se este nao estiver com uma
-- |          diferença de alturas superior a 0.2 para a peça da pista de baixo.
-- | 
-- | mov C >> Altera um jogador da pista 'p' para a pista 'p+1' se : este nao estiver na primeira pista; ... "condições iguais à mov B".
-- |
-- | mov E >> Altera a inclinaçao do jogador para 'i+15º' ate a um max de 90º se este estiver no ar.
-- |
-- | mov D >> Altera a inclinaçao do jogador para 'i-15º' ate a um max de -90º se este estiver no ar.
-- |
-- | mov _ >> Caso as condições nao sejam respeitadas a jogada nao provoca alteracoes no jogador.

mov :: Direcao -> Jogador -> Mapa -> Jogador
mov B (Jogador p d v c (Chao acc)) m | p /= up && dif1 <= 0.2 = Jogador (p + 1) d v c (Chao acc)
                                     | p /= up && aj > af = Jogador (p + 1) d v c (Ar aj i 0)
                                     | p /= up = Jogador p d 0 c (Morto 1.0)
                                     | otherwise = Jogador p d v c (Chao acc)
 where
  up = (length m) - 1                                        -- > Ultima pista
  dif1 = difAltB (Jogador p d v c (Chao acc)) m              -- > Diferença de alturas
  p1 = findPeca (p , truncate d) m                           -- > Peca onde se encontra
  p2 = findPeca (p + 1 , truncate d) m                       -- > Peca para onde transita
  aj = altJog p1 p d m                                       -- > Altura do jogador na pista atual 
  i = atan (declive d p1) * (180 / pi)                       -- > Inclinaçao da peça onde se encontra
  af = altJog p2 (p + 1) d m                                 -- > Altura do jogador na pista para a qual transita (Chao)

mov C (Jogador p d v c (Chao acc)) m | p /= 0 && dif2 <= 0.2 = Jogador (p - 1) d v c (Chao acc)
                                     | p /= 0 && aj > af = Jogador (p - 1) d v c (Ar aj i 0)
                                     | p /= 0 = Jogador p d 0 c (Morto 1.0)
                                     | otherwise = Jogador p d v c (Chao acc)
 where
  dif2 = difAltC (Jogador p d v c (Chao acc)) m              -- > Diferença de alturas
  p1 = findPeca (p , truncate d) m                           -- > Peca onde se encontra
  p2 = findPeca (p - 1 , truncate d) m                       -- > Peca para onde transita
  aj = altJog p1 p d m                                       -- > Altura do jogador na pista atual 
  i = atan (declive d p1) * (180 / pi)                       -- > Inclinaçao da peça onde se encontra
  af = altJog p2 p d m                                       -- > Altura do jogador na pista para a qual transita (Chao)

mov E (Jogador p d v c (Ar a i g)) m = Jogador p d v c e'
                                        where e' | i >= 75 = Ar a 90 g
                                                 | otherwise = Ar a (i + 15) g

mov D (Jogador p d v c (Ar a i g)) m = Jogador p d v c e' 
                                        where e' | i <= (-75) = Ar a (-90) g
                                                 | otherwise = Ar a (i - 15) g

mov _ (Jogador p d v c e) _ = Jogador p d v c e


-- | Calcula o declive de uma reta de uma determinada peça(peca) de acordo com a distancia percorrida(d)
declive :: Double -> Peca -> Double
declive d peca = (y2 - y1) / (x2 - x1)
 where
  x1 = fromIntegral (truncate d)
  y1 = fromIntegral (fst (alturasPeca peca))
  x2 = fromIntegral (truncate (d + 1))
  y2 = fromIntegral (snd (alturasPeca peca))

-- | Devolve uma Peca de uma mapa
findPeca :: PosicaoMatriz -> Mapa -> Peca
findPeca (l,c) m = encontraPosicaoMatriz (l,c) m

-- | Devolve um par de inteiros com a altura inicial e final de uma peca.
alturasPeca :: Peca -> (Int,Int)
alturasPeca (Recta _ n) = (n,n)
alturasPeca (Rampa _ n1 n2) = (n1,n2)

-- | Devolve a altura de um jogador, no chao, com base no declive da peca e a sua distancia percorrida.
altJog :: Peca -> Int -> Double -> Mapa -> Double
altJog (Recta _ a) p _ m = fromIntegral a
altJog peca p d m | ai < af = (d1 * m1) + fromIntegral ai                          -- > Rampa que sobe
                  | ai > af = (d2 * tan (pi - atan (m1))) + fromIntegral (fst pp)  -- > Rampa que desce
 where 
  ai = fst (alturasPeca peca)
  af = snd (alturasPeca peca)
  m1 = declive d peca
  d1 = d - fromIntegral (truncate d)                    -- > Distancia percorrida dentro da peça
  d2 = fromIntegral (truncate (d + 1)) - d              -- > Distancia percorrida dentro da peça
  pp = alturasPeca (findPeca (p , truncate d + 1) m)    -- > Altura(s) peca posterior

-- | Diferenca entre a altura em que o jogador se encontra (aaj) e a altura que este se encontrará (afj) caso se mova para a pista de cima.
difAltC :: Jogador -> Mapa -> Double
difAltC (Jogador 0 _ _ _ _) m = 1
difAltC (Jogador p d v c e) m = abs (aaj - afj)
 where
  aaj = altJog (findPeca ( p   , truncate d ) m) p d m
  afj = altJog (findPeca ( p-1 , truncate d ) m) p d m

-- | Diferenca entre a altura em que o jogador se encontra (aaj) e a altura que este se encontrará (afj) caso se mova para a pista de baixo.
difAltB :: Jogador -> Mapa -> Double
difAltB (Jogador p d v c e) m = if p == x then 1 else abs (aaj - afj)
 where
  aaj = altJog (findPeca ( p   , truncate d ) m) p d m
  afj = altJog (findPeca ( p+1 , truncate d ) m) p d m
  x   = (length m) - 1

-- | Devolve uma peca depois de ter sido sujeita a um disparo de cola.
auxfire :: Peca -> Peca
auxfire (Recta _ x) = Recta Cola x
auxfire (Rampa _ x y) = Rampa Cola x y

-- | Atualiza a capacidade de colas e o piso de uma certa peca para Cola se : o jogador tiver 1 ou mais colas e se nao se encontrar no ponto de partida.
-- | Para tal acontecer "encontra se" a peca anterior à que o jogador se encontra, altera se o seu piso, e atualiza se no mapa a mesma.
fire :: Int -> Jogador -> Estado -> Estado
fire n (Jogador p d v c (Chao acc)) (Estado m js) | c >= 1 && d >= 1 = Estado m' js'
                                                  | otherwise = (Estado m js)
                                                     where m' = atualizaPosicaoMatriz (p,(truncate d) - 1) (auxfire (encontraPosicaoMatriz (p,(truncate d) - 1) m)) m
                                                           js' = atualizaIndiceLista n (Jogador p d v (c - 1) (Chao acc)) js
fire _ _ (Estado m js) = (Estado m js)

-- | Modifica o estado aceleraJogador para True caso este se encontre no Chao e o seu estado aceleraJogador = False.
-- | Caso contrario a Jogada nao provoca alteracoes.
aceleraj :: Jogador -> Jogador
aceleraj (Jogador p d v c e) | e == Chao True = Jogador p d v c (Chao True)
                             | e == Chao False = Jogador p d v c (Chao True)
                             | otherwise = Jogador p d v c e


-- | Modifica o estado aceleraJogador para False caso este se encontre no Chao e o seu estado aceleraJogador = True.
-- | Caso contrario a Jogada nao provoca alteracoes.
desaceleraj :: Jogador -> Jogador
desaceleraj (Jogador p d v c e) | e == Chao True = Jogador p d v c (Chao False)
                                | e == Chao False = Jogador p d v c (Chao False)
                                | otherwise = Jogador p d v c e