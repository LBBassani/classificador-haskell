module DataHandler (registros, base_treino_teste, separa_em_classes, descobre_classes, Registro) where
import Random
import Data.List
{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável por 
      tratar da entrada do arquivo csv de entrada e criar os conjuntos de treinamento e teste.
-}

type Registro = ([Double], [Char])

-- Separa string do arquivo csv lido em registros do tipo ([Double], String) onde a lista de Double são os atributos
-- e a String é a classe do registro
registros :: [Char] -> [Registro]
registros str = [ (map (read::String->Double) $ init $ split ',' linha, last $ split ',' linha) | linha <- lines str ]

-- Separa uma string de acordo com um elemento d
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- Separa os dados vindo do csv em base de treino e teste
base_treino_teste :: [Registro] -> Int -> Int -> ([Registro], [Registro])
base_treino_teste regs percent seed = (base_treino, base_teste)
                                    where
                                        base_treino = [ regs!!x | x <- take n aleatorios]
                                        base_teste = [ regs!!x | x <- drop n aleatorios]
                                        aleatorios = randomList tamanho seed (tamanho -1)
                                        n = tamanho - ( truncate $ fromIntegral(tamanho*percent)/100.0)
                                        tamanho = length regs

-- Retorna a base separada por classes
separa_em_classes :: Eq a1 => [(a2, a1)] -> [[(a2, a1)]]
separa_em_classes base = [ registros_da_classe classe base | classe <- descobre_classes base ]

-- Devolve todos os registros de uma dada classe na base
registros_da_classe :: Eq a1 => a1 -> [(a2, a1)] -> [(a2, a1)]
registros_da_classe classe base = [ x | x <- base, snd x == classe]

-- Descobre as classes da base
descobre_classes :: Eq a1 => [(a2, a1)] -> [a1]
descobre_classes base = nub $ map snd base