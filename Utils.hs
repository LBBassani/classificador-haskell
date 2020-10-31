module Utils (le_num, soma_listas, distancia_euclidiana) where
{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável pelo tratamento de
        entradas e escrita dos arquivos de saída. 
-}

le_num :: IO Int
le_num = do num <- getLine
            return (read num :: Int)

-- Soma duas listas indice por indice
soma_listas :: Num a => [a] -> [a] -> [a]
soma_listas lst1 lst2 = [ lst1!!i + lst2!!i | i <- [0.. length lst1 - 1] ]

distancia_euclidiana:: [Double] -> [Double] -> Double
distancia_euclidiana lst1 lst2 = sqrt $ foldr (+) 0 [ (lst1!!i - lst2!!i)**2 | i <- [0.. length lst1 - 1] ]