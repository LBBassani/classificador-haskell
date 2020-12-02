module Utils (le_num, soma_listas, subtrai_listas, divide_listas, distancia_euclidiana, matriz_to_string, qsort, ponto_medio_lista) where
import Text.Printf

{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável pelo tratamento de
        entradas e escrita dos arquivos de saída e outras funções utilitárias. 
-}

le_num :: IO Int
le_num = do num <- getLine
            return (read num :: Int)

-- Soma duas listas indice por indice
soma_listas :: Num a => [a] -> [a] -> [a]
soma_listas lst1 lst2 = [ lst1!!i + lst2!!i | i <- [0.. length lst1 - 1] ]

-- Diminui duas listas indice por indice
subtrai_listas :: Num a => [a] -> [a] -> [a]
subtrai_listas lst1 lst2 = [ lst1!!i - lst2!!i | i <- [0.. length lst1 - 1] ]

-- Divide duas listas indice por indice
divide_listas :: (Fractional a) => [a] -> [a] -> [a]
divide_listas lst1 lst2 = [ lst1!!i / (lst2!!i) | i <- [0.. length lst1 - 1] ]

-- Calcula distancia euclidiana entre dois pontos de n dimensões R
distancia_euclidiana:: [Double] -> [Double] -> Double
distancia_euclidiana lst1 lst2 = sqrt $ foldr (+) 0 [ (lst1!!i - lst2!!i)**2 | i <- [0.. length lst1 - 1] ]

-- Dá uma matriz em forma de string
matriz_to_string :: [[Int]] -> [Char]
matriz_to_string matriz = foldr1 (++) linhas_matriz
                        where
                            linhas_matriz = [ linha_matriz x | x <- matriz ]
                            linha_matriz [x] = printf "%3d\n" x
                            linha_matriz (x:xs) = printf "%3d, " x ++ linha_matriz xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [ a | a <- xs, a <= x ] ++ [x] ++ qsort [ a | a <- xs, a > x]    

ponto_medio_lista :: [[Double]] -> [Double]
ponto_medio_lista lista = map (/tamanho) somatoria
                                        where
                                            tamanho = fromIntegral $ length lista
                                            somatoria = foldr1 soma_listas lista