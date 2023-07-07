-- * Funções principais da Tarefa 1.


-- | Gera "c-1" pares de numeros aleatorios para uma pista com um determinado comprimento (c).


-- | Gera "p" listas de "c-1" pares de numeros aleatorios para um mapa com um "p" pistas.


-- | Gera um piso de acordo com a gama (numero atribuido aleatoriamente pelas funcoes "gApista" e "gAmapa")


-- | Gera uma peca de acordo com a gama representada pelo par de numeros (atruibuido aleatoriamente),
-- | a altura da peca anterior (aa) e o tipo de piso anterior (pa), recorrendo a funcao "geraPiso".


-- | Gera uma pista de acordo com uma lista de pares de numeros (gama) e a altura (aa) e piso (pa) anterior de cada peca,
-- | recorrendo a funcao "geraPista". As incognitas "aa1" e "pa1" atualizam a altura e o piso anterior de cada peca.


-- | Gera uma lista de pistas (Mapa) recorendo a funcao "geraPista" e tendo em conta que cada pista comeca com a peca "Recta Terra 0"


-- | Gera um Mapa de acordo com o numero de pistas, o comprimento de cada uma e uma seed que recorrendo a funcao "geraAleatorios"
-- | gera uma lista com n elementos aleatorios.