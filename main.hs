import Utils
import DataHandler
import CentroidClassifier
import NearestNeighbor
import ClassifierAnalysis
import Text.Printf
import System.IO


{-
    Primeiro trabalho computacional de Programação Funcional em Haskell. Realiza o treinamento de dois classificadores
        (Centroide e Vizinho mais próximo) e analisa as acurácias e matriz de confusão da aplicação destes nas bases de
        teste.
-}

main :: IO ()
main = trabalho_2

trabalho_2 :: IO ()
trabalho_2 = do   
            -- Impedir buffering para escrever todas as mensagens na tela
            hSetBuffering stdout NoBuffering

            --leitura dos parametros de entrada
            putStr("Forneca o nome do arquivo de entrada: ")
            entrada <- getLine
            putStr("Forneca o nome do arquivo de saida: ")
            saida <- getLine
            putStr("Forneca o número de folds: ")
            num_folds <- le_num
            putStr("Forneca o número de vizinhos: ")
            num_knn <- le_num
            putStr("Forneca o valor da semente para geracao randomizada: ")
            semente <- le_num

            -- Preparo das bases de treino e teste
            dados <- readFile entrada
            let base = registros dados
            let folds = map padroniza_bases $ k_folds_treino_teste base num_folds semente
            let classes = descobre_classes base
            
            -- Resultados do classificador Nearest Neighbor
            let resultados = [ roda_classificador_nn (fst fold) (snd fold) | fold <- folds ]
            let acuracias = map (analisa_acuracia . snd) resultados
            putStr("Acuracia(vizinho): " )
            printf "%.2f" (media acuracias)
            putStrLn("%")
            putStr("Desvio-Padrao(vizinho): ")
            printf "%.2f" (desvio_padrao acuracias)
            putStrLn("%")
            let matriz_confusao = matriz_confusao_media $ map (monta_matriz_confusao classes . snd) resultados
            let escrever_saida_vizinho = "vizinho mais próximo:\n" ++ matriz_to_string matriz_confusao ++ "\n"
            
            -- Resultados do classificador Centroide
            let resultados = [ roda_classificador_centroide (fst fold) (snd fold) | fold <- folds ]
            let acuracias = map (analisa_acuracia . snd) resultados
            putStr("Acuracia(centroide): " )
            printf "%.2f" (media acuracias)
            putStrLn("%")
            putStr("Desvio-Padrao(centroide): ")
            printf "%.2f" (desvio_padrao acuracias)
            putStrLn("%")
            let matriz_confusao = matriz_confusao_media $ map (monta_matriz_confusao classes . snd) resultados
            let escrever_saida_centroide = "centroides:\n" ++ matriz_to_string matriz_confusao ++ "\n"
            
            -- Resultados do classificador knn
            let resultados = [ roda_classificador_knn num_knn (fst fold) (snd fold) | fold <- folds ]
            let acuracias = map (analisa_acuracia . snd) resultados
            putStr("Acuracia(k-vizinhos): " )
            printf "%.2f" (media acuracias)
            putStrLn("%")
            putStr("Desvio-Padrao(k-vizinhos): ")
            printf "%.2f" (desvio_padrao acuracias)
            putStrLn("%")
            let matriz_confusao = matriz_confusao_media $ map (monta_matriz_confusao classes . snd) resultados
            let escrever_saida_knn = "k-vizinhos mais próximos:\n" ++ matriz_to_string matriz_confusao ++ "\n"
            
            -- Escrevendo no arquivo de saída
            writeFile saida $ escrever_saida_vizinho ++ escrever_saida_centroide ++ escrever_saida_knn
--}

trabalho_1 :: IO ()
trabalho_1 = do   
            -- Impedir buffering para escrever todas as mensagens na tela
            hSetBuffering stdout NoBuffering
            
            --leitura dos parametros de entrada
            putStr("Forneca o nome do arquivo de entrada: ")
            entrada <- getLine
            putStr("Forneca o nome do arquivo de saida: ")
            saida <- getLine
            putStr("Forneca o percentual de exemplos de teste: ")
            percent <- le_num
            putStr("Forneca o valor da semente para geracao randomizada: ")
            semente <- le_num
            
            -- Preparo das bases de treino e teste
            dados <- readFile entrada
            let base = registros dados
            let base_treino = fst $ base_treino_teste base percent semente
            let base_teste = snd $ base_treino_teste base percent semente
            let classes = descobre_classes base
            
            -- Resultados do classificador Nearest Neighbor
            let resultados = roda_classificador_nn base_treino base_teste
            putStr("Acuracia(vizinho): " )
            printf "%.2f" (analisa_acuracia $ snd resultados)
            putStrLn("%")
            let matriz_confusao = monta_matriz_confusao classes $ snd resultados
            let escrever_saida_vizinho = "vizinho mais próximo:\n" ++ matriz_to_string matriz_confusao ++ "\n"
            
            -- Resultados do classificador Centroide
            let resultados = roda_classificador_centroide base_treino base_teste
            putStr("Acuracia(centroide): " )
            printf "%.2f" (analisa_acuracia $ snd resultados)
            putStrLn("%")
            let matriz_confusao = monta_matriz_confusao classes $ snd resultados
            let escrever_saida_centroide = "centroides:\n" ++ matriz_to_string matriz_confusao ++ "\n"
            
            -- Escrevendo no arquivo de saída
            writeFile saida $ escrever_saida_vizinho ++ escrever_saida_centroide