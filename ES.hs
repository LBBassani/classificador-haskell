module ES (leNum) where
{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável pelo tratamento de
        entradas e escrita dos arquivos de saída. 
-}

leNum :: IO Int
leNum = do  num <- getLine
            return (read num :: Int)