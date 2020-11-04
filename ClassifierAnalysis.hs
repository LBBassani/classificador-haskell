module ClassifierAnalysis (analisa_acuracia, monta_matriz_confusao) where
import Utils

analisa_acuracia :: [([Double], [Char], [Char])] -> Double
analisa_acuracia resultados = 100*(fromIntegral corretos )/(fromIntegral $ length resultados)
                                where
                                    corretos = length $ filter (\(_,x,y) -> x == y) resultados

monta_matriz_confusao :: [[Char]] -> [([Double], [Char], [Char])] -> [[Int]]
monta_matriz_confusao classes resultados = [linha_matriz_confusao x classes resultados | x <- classes] 

linha_matriz_confusao :: [Char] -> [[Char]] -> [([Double], [Char], [Char])] -> [Int]
linha_matriz_confusao classe classes resultados = [ celula_matriz_confusao x preditos | x <- classes]
                                                where 
                                                    preditos = filter (\(_,x,_) -> x == classe) resultados

celula_matriz_confusao :: [Char] -> [([Double], [Char], [Char])] -> Int
celula_matriz_confusao classe preditos = length $ filter (\(_,_,y) -> y == classe) preditos