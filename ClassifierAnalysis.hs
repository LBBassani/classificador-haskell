module ClassifierAnalysis (analisa_acuracia, monta_matriz_confusao, matriz_confusao_unificada) where
import Utils
import DataHandler(Registro)

{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável por 
      rodar o classificador vizinho mais próximo.
-}

-- Analisa a acurácia de um classificador a partir de seus resultados
analisa_acuracia :: [(Registro, [Char])] -> Double
analisa_acuracia resultados = 100*(fromIntegral corretos)/(fromIntegral $ length resultados)
                                where
                                    corretos = length $ filter (\((_,x),y) -> x == y) resultados

-- Monta a matriz de confusão de um classificador a partir de seus resultados
monta_matriz_confusao :: [[Char]] -> [(Registro, [Char])] -> [[Int]]
monta_matriz_confusao classes resultados = [linha_matriz_confusao x classes resultados | x <- classes] 

-- Monta uma linha da matriz de confusão de um classificador
linha_matriz_confusao :: [Char] -> [[Char]] -> [(Registro, [Char])] -> [Int]
linha_matriz_confusao classe classes resultados = [ celula_matriz_confusao x preditos | x <- classes]
                                                where 
                                                    preditos = filter (\((_,x),_) -> x == classe) resultados

-- Calcula uma celula da matriz de confusão de um classificador
celula_matriz_confusao :: [Char] -> [(Registro, [Char])] -> Int
celula_matriz_confusao classe preditos = length $ filter (\((_,_),y) -> y == classe) preditos

-- Monta da matriz de confusão unificada da utilização do k_folds_cross_validation
matriz_confusao_unificada :: [[[Int]]] -> [[Int]]
matriz_confusao_unificada matrizes_confusao = [linha_matriz_confusao_unificada i matrizes_confusao | i <- [0.. length (matrizes_confusao!!0) - 1 ]]

-- Monta uma linha da matriz de confusão unificada da utilização do k_folds_cross_validation
linha_matriz_confusao_unificada :: Int -> [[[Int]]] -> [Int]
linha_matriz_confusao_unificada i matrizes_confusao = [ celula_matriz_confusao_unificada i j matrizes_confusao | j <- [0..length ((matrizes_confusao!!0)!!0) - 1] ]

-- Calcula uma celula da matriz de confusão unificada da utilização do k_folds_cross_validation
celula_matriz_confusao_unificada :: Int -> Int -> [[[Int]]] -> Int
celula_matriz_confusao_unificada i j matrizes_confusao = foldr1 (+) [ (x!!i)!!j | x <- matrizes_confusao]