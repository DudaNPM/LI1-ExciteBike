{- |

= Introdução



= Objetivos


= Discussao e conclusao


-}


-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g151 where

import LI11920
import Tarefa0_2019li1g151
import Tarefa1_2019li1g151
import Tarefa2_2019li1g151

ra = 0.125                            -- > Resistencia do ar
ag = 1                                -- > Aceleraçao oferecida pela gravidade

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(2.5,(gera 1 5 1),(Jogador 0 0 0 5 (Chao True))),
            (2.5,(gera 1 5 1),(Jogador 0 0 0 5 (Chao False))),
            (1.5,(gera 1 5 1),(Jogador 0 0.9 1 5 (Chao False))),
            (3.5,(gera 1 5 1),(Jogador 0 1.5 5 5 (Chao False))),
            (0.2,(gera 1 5 1),(Jogador 0 4.5 0.5 5 (Chao True))),
            (15,(gera 1 10 4),(Jogador 0 4.5 5 5 (Chao False))),
            (0.1,[[Recta Terra 0,Rampa Cola 0 1]],(Jogador 0 1 5 5 (Chao False))),
            (2.5,(gera 1 5 1),(Jogador 0 0 0 5 (Morto 1.0))),
            (0.5,(gera 1 5 1),(Jogador 0 0 0 5 (Morto 1.0))),
            (10,(gera 1 10 4),(Jogador 0 6.9 5 5 (Chao True))),
            (10,(gera 1 10 4),(Jogador 0 2.9 5 5 (Chao True))),
            (10,(gera 1 10 4),(Jogador 0 7.9 5 5 (Chao True))),
            (4.5,(gera 1 10 4),(Jogador 0 0.5 10 5 (Ar 1.8 20 2))),
            (2.5,(gera 1 10 4),(Jogador 0 0.5 10 5 (Ar 0.4 20 2))),
            (0.5,(gera 1 10 4),(Jogador 0 0.5 10 5 (Ar 1 20 2))),
            (0.5,(gera 1 10 4),(Jogador 0 0.5 10 5 (Ar 0.4 20 2))),
            (5,(gera 1 10 4),(Jogador 0 4.1 10 5 (Ar 2.1 (-50) 1))),
            (0.5,(gera 1 10 4),(Jogador 0 5.1 1 5 (Ar 4 0 2))),
            (0.5,(gera 1 10 4),(Jogador 0 2.1 1 5 (Ar 2 0 2)))]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j = velocidade t m j

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j = repjogador j t m






-- | Devolve o piso de uma determinada peca.
piso :: Peca -> Piso
piso (Recta p _)   = p
piso (Rampa p _ _) = p

-- | Devolve o atrito de uma peça em funçao do piso em que o jogador se encontra.
atrito :: Piso -> Double
atrito Terra = 0.25
atrito Relva = 0.75
atrito Lama  = 1.50
atrito Boost = (-0.50)
atrito Cola  = 3

-- | Atualiza o timeout de um jogador apos um periodo de tempo.
atuaTimeout :: Double -> Jogador -> Jogador
atuaTimeout t (Jogador p d v c (Morto x)) | x - t > 0 = Jogador p d v c (Morto (x - t))
                                          | otherwise = Jogador p d 0 c (Chao False)

-- | Devolve True/False dependendo do campo 'aceleraJogador' que se obtem atraves do EstadoJogador.
accelJogador :: EstadoJogador -> Bool
accelJogador (Chao acc) = acc

-- | Delvove os valores 1 ou 0 dependendo da velocidade inicial e o EstadoJogador.
accelMota :: Double -> EstadoJogador -> Int
accelMota v e | v < 2 && accelJogador e = 1
              | otherwise               = 0

-- | Devolve a velocidade do jogador quando esta no Chao de acordo com a velocidade inicial (v), o EstadoJogador (e), o atrito (a),
-- | e o periodo de tempo (t) em questao. 
velChao :: Double -> EstadoJogador -> Double -> Double -> Double
velChao v e a t = max 0 vj
 where
  vj = v + ( ( fromIntegral (accelMota v e) - (a * v) ) * t )

-- | Devolve a velocidade do jogador quando esta no Ar de acordo com a velocidade inicial (v) e o periodo de tempo (t) em questao.
velAr :: Double -> Double -> Double
velAr v t = max 0 v'
 where
  v' = v - (ra * v * t)

-- | Devolve a gravidade do jogador quando esta no Ar de acordo com a gravidade inicial (g) e o periodo de tempo (t) em questao.
grav :: Double -> Double -> Double
grav g t = max 0 g'
 where
  g' = g + (ag * t)

-- | Altera a velocidade e/ou gravidade de um jogador dependendo do estado em que ele se encontre Chao/Ar/Morto.
velocidade :: Double -> Mapa -> Jogador -> Jogador
velocidade t m (Jogador p d v c (Chao acc)) = Jogador p d v' c (Chao acc)
 where
  v' = velChao v (Chao acc) a' t
  a' = atrito (piso pa)
  pa = findPeca (p , truncate d) m

velocidade t m (Jogador p d v c (Ar a i g)) = Jogador p d v' c (Ar a i g')
 where
  v' = velAr v t
  g' = grav g t

velocidade _ _ j = j

-- | Verifica se a inclinaçao da peça posterior é maior que a da peça atual, atraves do declive.
checkinc :: Peca -> Peca -> Double -> Bool
checkinc p1 p2 d = declive d p2 >= declive d p1

 -- | Calcula a distancia que o jogador percorre num determinado periodo de tempo em linha reta, no chao.
dist :: Double -> Double -> Double
dist t v = t * v

-- | Verifica se a distancia percorrida, no chao, por um jogador num determinado periodo de tempo ultrapassa ou nao o limite de uma peça.
checkdistChao :: Peca -> Int -> Double -> Double -> Double -> Mapa -> Bool
checkdistChao (Recta _ _) p d t v m = d1 + d2 > 1
 where
  d1 = dist t v                      -- ^ Distançia percorrida apos o periodo de tempo e velocidades considerados (reta).
  d2 = d - fromIntegral (truncate d) -- ^ Distançia percorrida na peça (eixo x).

checkdistChao peca p d t v m = d4 + d2 > d5
 where
  d1 = dist t v                                                  -- ^ Distançia percorrida apos o periodo de tempo e velocidades considerados (reta).
  d2 = d1 / (cos (atan (declive d peca)))                        -- ^ Distançia percorrida apos o periodo de tempo e velocidades considerados (rampa).
  d3 = d - fromIntegral (truncate d)                             -- ^ Distançia percorrida na peça (eixo x).
  d4 = sqrt ( d3^2 + ((altJog peca p d m) - fromIntegral ai)^2 ) -- ^ Distançia percorrida na rampa.
  d5 = sqrt ( fromIntegral (1 + (af - ai)^2) )                   -- ^ Comprimento da rampa.
  ai = fst (alturasPeca peca)                                    -- ^ Altura inical da peça.
  af = snd (alturasPeca peca)                                    -- ^ Altura final da peça.

-- | Verifica se a distancia percorrida, no ar, por um jogador ultrapassa ou nao o limite de uma peça, em funçao da sua posicao final.
checkdistAr :: Double -> Vetor -> Bool
checkdistAr d posf = xf >= fromIntegral (truncate d + 1)
 where
  xf = fst (auxPonto posf) -- ^ Abcissa da posicao final do jogador.

-- | Verifica se um jogador embate ou nao no chao com base na sua posicao final (aplicacao da soma de vetores). 
checkEmbate :: Double -> Peca -> Double -> Vetor -> Bool
checkEmbate _ (Recta _ a) aj (Cartesiano _ y) = y <= fromIntegral a
checkEmbate d p aj posf1 = intersetam r1 r2
 where
  ai  = fromIntegral (fst (alturasPeca p))                                                      -- ^ Altura inicial da rampa.
  af  = fromIntegral (snd (alturasPeca p))                                                      -- ^ Altura final da rampa.
  r1  = (Cartesiano (fromIntegral (truncate d)) ai,Cartesiano (fromIntegral (truncate d+1)) af) -- ^ Reta da peca.
  r2  = (Cartesiano d aj,posf1)  -- ^ reta definida pela posicao atual do jogador e a posicao final (aplicacao da soma de vetores).

-- | Verifica se um jogador morre ou nao ao embater no chao.
checkEmbate1 :: Double -> Double -> Bool
checkEmbate1 dc i = abs ( i - ((atan dc)*(180/pi)) ) >= 45

-- | Calcula o ponto de intersecao da reta definida pela posicao atual do jogador e a posicao final (aplicacao da soma de vetores)
-- | e a reta da peca em que se encontra.
intersetaChao :: Peca -> Reta -> Double -> Ponto
intersetaChao (Recta _ h) (a,b) d = intersecao (a,b) (x,y)
 where
  (x,y) = (Cartesiano (fromIntegral (truncate d)) (fromIntegral h),Cartesiano (fromIntegral (truncate d + 1)) (fromIntegral h))

intersetaChao (Rampa _ ai af) (a,b) d = intersecao (a,b) (x,y)
 where
  (x,y) = (Cartesiano (fromIntegral (truncate d)) (fromIntegral ai),Cartesiano (fromIntegral (truncate d + 1)) (fromIntegral (af)))

-- | Calcula o ponto de intersecao da reta definida pela posicao atual do jogador e a posicao final (aplicacao da soma de vetores)
-- | e a reta x=truncate (d+1).
intersetaLimitePeca :: Peca -> Reta -> Double -> Ponto
intersetaLimitePeca peca (a,b) d = intersecao (a,b) (x,y)
 where
  (x,y) = (Cartesiano (fromIntegral (truncate d+1)) (fromIntegral afp),Cartesiano (fromIntegral (truncate d + 1)) 100)
  afp   = snd (alturasPeca peca)
-- | Escolhe a primeira intersecao realizada pelo jogador.
escolheIntersecao :: Double -> Ponto -> Ponto -> Ponto
escolheIntersecao d p1 p2 | x1 < fromIntegral (truncate (d+1)) && x1 > d = p1
                          | otherwise                                    = p2
 where
  x1 = fst (auxPonto p1)

-- | Calcula a posiçao final (aplicacao da soma de vetores) de um jogador no estado Ar.
posFinalAr1 :: Jogador -> Double -> Vetor
posFinalAr1 (Jogador _ d v _ (Ar a i g)) t = somaVetores pj vgt
 where
  pj  = Cartesiano d a                                              -- ^ Posicao atual do jogador.
  vgt = multiplicaVetor t (somaVetores (Polar v i) (Polar g (-90))) -- ^ Posicao final do jogador (aplicacao da soma de vetores).

-- | Calcula a posicao final do jogador.
posFinalAr :: Mapa -> Double -> Jogador -> Ponto
posFinalAr  m t (Jogador p d v c (Ar aj i g)) | not ce && not cda = posf1
                                              | otherwise         = escolheIntersecao d p1 p2
 where
  ej    = (Jogador p d v c (Ar aj i g)) -- ^ Estado do jogador.
  pa    = findPeca (p , truncate (d)) m -- ^ Peca atual do jogador.
  posi  = Cartesiano d aj               -- ^ Posicao atual do jogador.
  posf1 = posFinalAr1 ej t              -- ^ Posicao final do jogador (aplicacao da soma de vetores).
  cda   = checkdistAr d posf1
  ce    = checkEmbate d pa aj posf1
  p1    = intersetaChao pa (posi,posf1) d
  p2    = intersetaLimitePeca pa (posi,posf1) d

-- | Devolve um par de inteiros correspondentes as coordenadas de um Ponto.
auxPonto :: Ponto -> (Double,Double)
auxPonto (Cartesiano x y) = (x,y)

-- | Reposiciona um jogador dependendo da distancia que percorreu.
repjogador :: Jogador -> Double -> Mapa -> Jogador
repjogador (Jogador p d 0 c (Chao acc)) _ _ = (Jogador p d 0 c (Chao acc))

repjogador (Jogador p d v c (Chao acc)) t m = (Jogador p d' v c e')
 where
  pf = findPeca (p , truncate (d+1)) m -- ^ Peça "futura".
  pa = findPeca (p , truncate d) m     -- ^ Peça atual do jogador.
  d' | checkdistChao pa p d t v' m = fromIntegral (truncate (d + 1))
     | otherwise                   = d + (dist t v')
   where
    v' = abs (v + ( ( fromIntegral (accelMota v (Chao acc)) - (a * v) ) * t ))
    a  = atrito (piso pa)
  
  e' | checkinc pa pf d = (Chao acc)
     | otherwise        = (Ar (fromIntegral (snd (alturasPeca pa))) i 0)
   where
    i = (atan (declive d pa)) * (180 / pi)

repjogador (Jogador p d v c (Ar a i g)) t m = (Jogador p d' v' c e')
 where
  d' = fst (auxPonto posf)
   where
    posf = posFinalAr m t ej            -- ^ Posicao final do jogador.
    ej   = (Jogador p d v c (Ar a i g)) -- ^ Estado do jogador.
             
            
  v' | not cda && ce && not ce1 = v1 -- ^ Nao ultrapassa o limite da peca, bate no chao e nao morre.
     | not cda && ce && ce1     = 0  -- ^ Nao ultrapassa o limite da peca, bate no chao e morre.
     | otherwise                = v           
   where           
    v1    = max 0 (v*(cos ((pi/180)*(i - ip)))) -- ^ Velocidade do jogador apos embater no chao.
    posf  = posFinalAr m t ej                   -- ^ Posicao final do jogador.
    posf1 = posFinalAr1 ej t                    -- ^ Posicao final do jogador (aplicacao da soma de vetores).
    ip    = (atan (declive d pa)) * (180 / pi)  -- ^ Inclinacao da peca.
    pa    = findPeca (p , truncate d) m         -- ^ Peca atual.
    ej    = (Jogador p d v c (Ar a i g))        -- ^ Estado do jogador.
    cda   = checkdistAr d posf
    ce    = checkEmbate d pa a posf1
    ce1   = checkEmbate1 (declive d pa) i

 
 
  e' | not cda && ce && ce1 = Morto 1.0  -- ^ Nao ultrapassa o limite da peca, bate no chao e morre.
     | not cda && ce        = Chao False -- ^ Nao ultrapassa o limite da peca, bate no chao e nao morre.
     | otherwise            = Ar (snd (auxPonto posf)) i g
   where
    posf  = posFinalAr m t ej            -- ^ Posicao final do jogador.
    posf1 = posFinalAr1 ej t             -- ^ Posicao final do jogador (aplicacao da soma de vetores).
    pa    = findPeca (p , truncate d) m  -- ^ Peca atual.
    ej    = (Jogador p d v c (Ar a i g)) -- ^ Estado do jogador.
    cda   = checkdistAr d posf
    ce    = checkEmbate d pa a posf1
    ce1   = checkEmbate1 (declive d pa) i
    
repjogador j t _ = atuaTimeout t j