module CentroidClassifier (roda_classificador_centroide, centroide_classifica, centroide_treino) where
import DataHandler
import Utils

{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável por 
      rodar o classificador centroide.
-}

-- roda o classificador, realizando o treino e o teste, devolvendo: ( classificador, [( (atributos do registro, classificação dada), classificação real ) ] )
roda_classificador_centroide :: [Registro] -> [Registro] -> ([Registro], [(Registro, [Char])])
roda_classificador_centroide base_treino base_teste = (classificador, [((fst registro, centroide_classifica ( fst registro ) classificador), snd registro) | registro <- base_teste])
                                                        where
                                                            classificador = centroide_treino base_treino

-- classifica um registro através do classificador treinado pelo método centroide
centroide_classifica :: [Double] -> [Registro] -> [Char]
centroide_classifica registro classificador = snd menor_dist
                                                where
                                                    menor_dist = foldr1 menor distancias
                                                    menor a b = if (fst a) < (fst b) then a else b
                                                    distancias = [ (distancia_euclidiana registro $ fst centroide_classe, snd centroide_classe ) | centroide_classe <- classificador ]

-- Realiza o treinamento do classificador centroide, descobrindo os pontos médios de cada classe na base de treinamento
centroide_treino :: [Registro] -> [Registro]
centroide_treino base_treino = [ (ponto_medio_classe $ map fst x, snd $ x!!0) | x <- separa_em_classes base_treino ]

-- Calcula o ponto médio de uma classe
ponto_medio_classe :: [[Double]] -> [Double]
ponto_medio_classe registros_classe = map (/tamanho) somatoria
                                        where
                                            tamanho = fromIntegral $ length registros_classe
                                            somatoria = foldr1 soma_listas registros_classe