module ClassifierAnalysis where
import Utils

analisa_acuracia :: [([Double], [Char], [Char])] -> Double
analisa_acuracia resultados = 100*(fromIntegral corretos )/(fromIntegral $ length resultados)
                                where
                                    corretos = length $ filter (\(_,x,y) -> x == y) resultados

-- monta_matriz_confusao resultados