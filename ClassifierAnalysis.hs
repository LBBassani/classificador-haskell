module ClassifierAnalysis (analisa_acuracia, monta_matriz_confusao) where
import Utils

{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável por 
      rodar o classificador vizinho mais próximo.
-}

-- Analisa a acurácia de um classificador a partir de seus resultados
analisa_acuracia :: [([Double], [Char], [Char])] -> Double
analisa_acuracia resultados = 100*(fromIntegral corretos )/(fromIntegral $ length resultados)
                                where
                                    corretos = length $ filter (\(_,x,y) -> x == y) resultados

-- Monta a matriz de confusão de um classificador a partir de seus resultados
monta_matriz_confusao :: [[Char]] -> [([Double], [Char], [Char])] -> [[Int]]
monta_matriz_confusao classes resultados = [linha_matriz_confusao x classes resultados | x <- classes] 

-- Monta uma linha da matriz de confusão de um classificador
linha_matriz_confusao :: [Char] -> [[Char]] -> [([Double], [Char], [Char])] -> [Int]
linha_matriz_confusao classe classes resultados = [ celula_matriz_confusao x preditos | x <- classes]
                                                where 
                                                    preditos = filter (\(_,x,_) -> x == classe) resultados

-- Calcula uma celula da matriz de confusão de um classificador
celula_matriz_confusao :: [Char] -> [([Double], [Char], [Char])] -> Int
celula_matriz_confusao classe preditos = length $ filter (\(_,_,y) -> y == classe) preditos