module Tarefa0_2019li1g151 where

data Ponto = Cartesiano Double Double | Polar Double Angulo deriving (Show)
type Angulo = Double
type Vetor = Ponto
type Reta = (Ponto,Ponto)
type DimensaoMatriz = (Int,Int)
type PosicaoMatriz = (Int,Int)
type Matriz a = [[a]]


convertPolar :: Ponto -> Ponto
convertPolar (Cartesiano x y) = Cartesiano x y
convertPolar (Polar dist ang) = Cartesiano (dist * cos (ang * (pi / 180))) (dist * sin (ang * (pi / 180)))


-- | Soma dois Vetores.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1 + x2) (y1 + y2)
somaVetores p1 p2 = somaVetores (convertPolar p1) (convertPolar p2)


-- | Subtrai dois Vetores.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1 - x2) (y1 - y2)
subtraiVetores p1 p2 = subtraiVetores (convertPolar p1) (convertPolar p2)


-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor n (Cartesiano x y) = Cartesiano (x * n) (y * n)
multiplicaVetor n p1 = multiplicaVetor n (convertPolar p1)


-- | Testar se dois segmentos de reta se intersetam.
intersetam :: Reta -> Reta -> Bool
intersetam (Cartesiano x1 y1,Cartesiano x2 y2) (Cartesiano x3 y3,Cartesiano x4 y4) = if t1 >= 0 && t1 <= 1 && t2 >= 0 && t2 <= 1 
                                                                                     then True 
                                                                                     else False
    
    where t1 = (((y3 - y4) * (x1 - x3)) + ((x4 - x3) * (y1 - y3))) / (((x4 - x3) * (y1 - y2)) - ((x1 - x2) * (y4 - y3)))
          t2 = (((y1 - y2) * (x1 - x3)) + ((x2 - x1) * (y1 - y3))) / (((x4 - x3) * (y1 - y2)) - ((x1 - x2) * (y4 - y3)))

intersetam (p1,p2) (p3,p4) = intersetam (convertPolar p1,convertPolar p2) (convertPolar p3,convertPolar p4)


-- | Calcular o ponto de intersecao entre dois segmentos de reta.
intersecao :: Reta -> Reta -> Ponto
intersecao (p1@(Cartesiano x1 y1),p2@(Cartesiano x2 y2)) (p3@(Cartesiano x3 y3),p4@(Cartesiano x4 y4)) = pip
  where d = (((x4 - x3) * (y1 - y2)) - (x1-x2) * (y4-y3))
        ta = ((y3 - y4) * (x1 - x3) + (x4 - x3) * (y1 - y3)) / d
        p2mp1 = subtraiVetores p2 p1
        tatpip = multiplicaVetor ta p2mp1
        pip = somaVetores p1 tatpip

intersecao (p1r1,p2r1) (p1r2,p2r2) = resultado
          where 
          aCart = convertPolar p1r1 
          bCart = convertPolar p2r1
          cCart = convertPolar p1r2
          dCart = convertPolar p2r2
          resultado = intersecao (aCart, bCart) (cCart, dCart)

-- | Verifica se o indice pertence à lista
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x y = x >= 0 && x < l
    where l = length y


-- | Calcula a dimensão de uma matriz.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz m = if length (head m) > 0
                   then (length m, length (head m))
                   else (0,0)


-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida _ [] = False
ePosicaoMatrizValida (l,c) (h:s)  | l >= 0 && l <= (m - 1) && c >= 0 && c <= (n - 1) = True
                                  | otherwise = False

    where m = length (h:s)
          n = length h


-- | Normaliza um ângulo na gama [0..360).
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo x
               | x >= 0 && x < 360 = x
               | otherwise = abs ( ( ( abs (x) / 360)  - 1 ) ) * 360


-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista x l = last (take (x + 1) l )


-- | Modifica um elemento num dado índice.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista x y l = if x < length l 
                            then (take x l) ++ (y:(drop (x+1) l))
                            else l


-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (l,c) m = encontraIndiceLista c x1

 where
  encontraListaMatriz :: Int -> [[a]] -> [a]
  encontraListaMatriz x l = last (take (x + 1) l )
  
  x1 = encontraListaMatriz l m
                                

-- | Modifica um elemento numa dada 'Posicao'
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) y z = if ePosicaoMatrizValida (l,c) z == True
                                  then atualizaIndiceListaMatriz l x2 z
                                  else z 

 where
  x1 = last (take (l+1) z)
  x2 = atualizaIndiceLista c y x1

  atualizaIndiceListaMatriz :: Int -> [a] -> [[a]] -> [[a]]
  atualizaIndiceListaMatriz l x2 z = (take l z) ++ (x2:(drop (l+1) z))