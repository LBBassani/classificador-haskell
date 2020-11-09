module NearestNeighbor (roda_classificador_nn, nn_classifica) where
import DataHandler
import Utils

{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável por 
      rodar o classificador vizinho mais próximo.
-}

-- roda o classificador, realizando o treino e o teste, devolvendo: ( classificador, [( atributos do registro, classificação dada, classificação real ) ] )
roda_classificador_nn :: [Registro] -> [Registro] -> ([Registro], [(Registro, [Char])])
roda_classificador_nn base_treino base_teste = (base_treino, [((fst registro, nn_classifica ( fst registro ) base_treino), snd registro) | registro <- base_teste])

-- classifica um registro através do classificador treinado pelo método NearestNeighbor
nn_classifica :: [Double] -> [Registro] -> [Char]
nn_classifica registro base_treino = snd menor_dist
                                                where
                                                    menor_dist = foldr1 menor distancias
                                                    menor a b = if (fst a) < (fst b) then a else b
                                                    distancias = [ (distancia_euclidiana registro $ fst registro_base, snd registro_base ) | registro_base <- base_treino ] 