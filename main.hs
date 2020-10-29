import Random
import ES

main :: IO ()
main = do   putStr("Forneca o nome do arquivo de entrada: ")
            entrada <- getLine
            putStr("Forneca o nome do arquivo de saida: ")
            saida <- getLine
            putStr("Forneca o percentual de exemplos de teste: ")
            percent <- leNum
            putStr("Forneca o valor da semente para geracao randomizada: ")
            semente <- leNum
            putStrLn(entrada ++ " " ++ saida ++ " " ++ show percent ++ " " ++ show semente)