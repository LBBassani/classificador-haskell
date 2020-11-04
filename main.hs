import Utils
import DataHandler
import CentroidClassifier
import NearestNeighbor
import ClassifierAnalysis
import Text.Printf

{-
main :: IO ()
main = do   putStr("Forneca o nome do arquivo de entrada: ")
            entrada <- getLine
            putStr("Forneca o nome do arquivo de saida: ")
            saida <- getLine
            putStr("Forneca o percentual de exemplos de teste: ")
            percent <- le_num
            putStr("Forneca o valor da semente para geracao randomizada: ")
            semente <- le_num
            putStrLn(entrada ++ " " ++ saida ++ " " ++ show percent ++ " " ++ show semente)
-}

main :: IO ()
main = do   dados <- readFile "iris.csv"
            let base_treino = fst $ base_treino_teste dados 30 42
            let base_teste = snd $ base_treino_teste dados 30 42
            let resultados = roda_classificador_nn base_treino base_teste
            putStr("Acuracia do NN: " )
            printf "%.2f\n" (analisa_acuracia $ snd resultados)
            let resultados = roda_classificador_centroide base_treino base_teste
            putStr("Acuracia do Centroide: " )
            printf "%.2f\n" (analisa_acuracia $ snd resultados)