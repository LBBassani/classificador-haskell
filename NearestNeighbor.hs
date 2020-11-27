module NearestNeighbor (roda_classificador_nn, nn_classifica, roda_classificador_knn, knn_classifica) where
import DataHandler
import Utils

{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável por 
      rodar o classificador vizinho mais próximo.
-}

{-- roda o classificador Nearest Neighbor, realizando o treino e o teste, devolvendo: 
( classificador, [( (atributos do registro, classificação dada), classificação real ) ] ) --}
roda_classificador_nn :: [Registro] -> [Registro] -> ([Registro], [(Registro, [Char])])
roda_classificador_nn base_treino base_teste = (base_treino, [((fst registro, nn_classifica ( fst registro ) base_treino), snd registro) | registro <- base_teste])

-- classifica um registro através do classificador treinado pelo método Nearest Neighbor
nn_classifica :: [Double] -> [Registro] -> [Char]
nn_classifica registro base_treino = snd menor_dist
                                                where
                                                    menor_dist = foldr1 menor distancias
                                                    menor a b = if (fst a) < (fst b) then a else b
                                                    distancias = [ (distancia_euclidiana registro $ fst registro_base, snd registro_base ) | registro_base <- base_treino ] 

{-- roda o classificador K Nearest Neighbor, realizando o treino e o teste, devolvendo: 
( classificador, [( (atributos do registro, classificação dada), classificação real ) ] ) --} 
roda_classificador_knn :: Int -> [Registro] -> [Registro] -> ([Registro], [(Registro, [Char])])
roda_classificador_knn k base_treino base_teste = (base_treino, [((fst registro, knn_classifica k ( fst registro ) base_treino), snd registro) | registro <- base_teste])

-- classifica um registro através do classificador treinado pelo método K Nearest Neighbor
knn_classifica :: Int -> [Double] -> [Registro] -> [Char]
knn_classifica k registro base_treino = snd menor_media
                                                where
                                                    menor_media = foldr1 (\x y -> if (fst $ fst x) <= (fst $ fst y) then x else y) vizinhos_mais_prox
                                                    vizinhos_mais_prox = filter (\y -> (snd $ fst y) == maior_tamanho) quantidade_e_medias
                                                    maior_tamanho = snd $ fst $ foldr1 (\x y -> if (snd $ fst x) >= (snd $ fst y) then x else y ) quantidade_e_medias
                                                    quantidade_e_medias :: [((Double, Int), [Char])]
                                                    quantidade_e_medias = [ (((somatoria $ map fst x)/(tamanho $ map fst x), tamanho $ map fst x), snd $ x!!0 ) | x <- por_classes ]
                                                    tamanho x = fromIntegral $ length x
                                                    somatoria x = foldr1 (+) x 
                                                    por_classes = separa_em_classes k_menores_dist
                                                    k_menores_dist = take k $ qsort distancias
                                                    distancias = [ (distancia_euclidiana registro $ fst registro_base, snd registro_base ) | registro_base <- base_treino ]