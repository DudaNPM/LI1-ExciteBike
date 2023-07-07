-- * Funções principais da Tarefa 4.

-- | Devolve o piso de uma determinada peca.

-- | Devolve o atrito de uma peça em funçao do piso em que o jogador se encontra.

-- | Atualiza o timeout de um jogador apos um periodo de tempo.

-- | Devolve True/False dependendo do campo 'aceleraJogador' que se obtem atraves do EstadoJogador.

-- | Delvove os valores 1 ou 0 dependendo da velocidade inicial e o EstadoJogador.

-- | Devolve a velocidade do jogador quando esta no Chao de acordo com a velocidade inicial (v), o EstadoJogador (e), o atrito (a),
-- | e o periodo de tempo (t) em questao. 

-- | Devolve a velocidade do jogador quando esta no Ar de acordo com a velocidade inicial (v) e o periodo de tempo (t) em questao.

-- | Devolve a gravidade do jogador quando esta no Ar de acordo com a gravidade inicial (g) e o periodo de tempo (t) em questao.

-- | Altera a velocidade e/ou gravidade de um jogador dependendo do estado em que ele se encontre Chao/Ar/Morto.

-- | Verifica se a inclinaçao da peça posterior é maior que a da peça atual, atraves do declive.

-- | Calcula a distancia que o jogador percorre num determinado periodo de tempo em linha reta, no chao.

-- | Verifica se a distancia percorrida, no chao, por um jogador num determinado periodo de tempo ultrapassa ou nao o limite de uma peça.

-- | Verifica se a distancia percorrida, no ar, por um jogador ultrapassa ou nao o limite de uma peça, em funçao da sua posicao final.

-- | Verifica se um jogador embate ou nao no chao com base na sua posicao final (aplicacao da soma de vetores). 

-- | Verifica se um jogador morre ou nao ao embater no chao.

-- | Calcula o ponto de intersecao da reta definida pela posicao atual do jogador e a posicao final (aplicacao da soma de vetores)
-- | e a reta da peca em que se encontra.

-- | Calcula o ponto de intersecao da reta definida pela posicao atual do jogador e a posicao final (aplicacao da soma de vetores)

-- | e a reta x=truncate (d+1).

-- | Escolhe a primeira intersecao realizada pelo jogador.

-- | Calcula a posiçao final (aplicacao da soma de vetores) de um jogador no estado Ar.

-- | Calcula a posicao final do jogador.

-- | Devolve um par de inteiros correspondentes as coordenadas de um Ponto.

-- | Reposiciona um jogador dependendo da distancia que percorreu.
