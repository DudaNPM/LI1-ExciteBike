-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.


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


-- | Devolve uma Peca de uma mapa


-- | Devolve a altura maxima de uma peca


-- | Devolve a altura de um jogador com base na altura da peca e a sua distancia percorrida.
-- | "(d - fromIntegral (truncate d))" -> representa a distancia percorrida na peca em que se encontra.


-- | Diferenca entre a altura em que o jogador se encontra (aaj) e a altura que este se encontrará (afj) caso se mova para a pista de cima.


-- | Diferenca entre a altura em que o jogador se encontra (aaj) e a altura que este se encontrará (afj) caso se mova para a pista de baixo.


-- | Devolve uma peca depois de ter sido sujeita a um disparo de cola.


-- | Atualiza a capacidade de colas e o piso de uma certa peca para Cola se : o jogador tiver 1 ou mais colas e se nao se encontrar no ponto de partida.
-- | Para tal acontecer "encontra se" a peca anterior à que o jogador se encontra, altera se o seu piso, e atualiza se no mapa a mesma.


-- | Modifica o estado aceleraJogador para True caso este se encontre no Chao e o seu estado aceleraJogador = False.
-- | Caso contrario a Jogada nao provoca alteracoes.


-- | Modifica o estado aceleraJogador para False caso este se encontre no Chao e o seu estado aceleraJogador = True.
-- | Caso contrario a Jogada nao provoca alteracoes.