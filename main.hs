import Random
import Utils

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