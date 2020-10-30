module DataHandler (registros) where
import Random
{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável por 
      tratar da entrada do arquivo csv de entrada e criar os conjuntos de treinamento e teste.
-}

-- Separa string do arquivo csv lido em registros do tipo ([Double], String) onde a lista de Double são os atributos
-- e a String é a classe do registro
registros :: String -> [([Double], [Char])]
registros str = [ (map (read::String->Double) $ init $ split ',' linha, last $ split ',' linha) | linha <- lines str ]

-- Separa uma string de acordo com um elemento d
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

base_treino_teste :: [Char] -> Int -> Int -> ([([Double], [Char])], [([Double], [Char])])
base_treino_teste str percent seed = (base_treino, base_teste)
                                    where
                                        base_treino = [ regs!!x | x <- drop n aleatorios]
                                        base_teste = [ regs!!x | x <- take n aleatorios]
                                        regs = registros str
                                        aleatorios = randomList tamanho seed tamanho
                                        n = truncate $ fromIntegral(tamanho*percent)/100.0
                                        tamanho = length regs