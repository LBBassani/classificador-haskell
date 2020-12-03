module DataHandler (registros, base_treino_teste, k_folds_treino_teste, separa_em_classes, descobre_classes, padroniza_bases, Registro) where
import Utils
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
                                        base_treino = [ regs!!x | x <- drop n aleatorios]
                                        base_teste = [ regs!!x | x <- take n aleatorios]
                                        aleatorios = randomList tamanho seed (tamanho - 1)
                                        n =  truncate $ fromIntegral(tamanho*percent)/100.0
                                        tamanho = length regs

-- Separa os dados vindo do csv em k bases de treino e teste a partir da separação em k_folds
k_folds_treino_teste :: [Registro] -> Int -> Int -> [([Registro], [Registro])]
k_folds_treino_teste regs k seed = [ ( folds_treino i, folds_teste i ) | i <- [0..k - 1] ]
                                    where
                                        folds_treino i = [regs!!x | x <- foldr1 (++) $ (take i folds ++ (drop (i+1) folds) ) ]
                                        folds_teste i = [regs!!x | x <- (folds!!i) ]
                                        folds = monta_folds 0 separa_folds
                                        monta_folds i lista_folds = if null $ last lista_folds
                                                                        then monta_folds i $ init lista_folds
                                                                        else if length lista_folds == k
                                                                                then lista_folds
                                                                                else monta_folds (i+1) ( take i lista_folds ++ [(lista_folds!!i ++ (take 1 $ last lista_folds) )] ++ (init $ drop (i+1) lista_folds ) ++ [(drop 1 $ last lista_folds)] )
                                        separa_folds = [ take n $ drop (n*i) aleatorios | i <- [0..k - 1] ] ++ [drop (k*n) aleatorios]
                                        aleatorios = randomList tamanho seed (tamanho - 1)
                                        n = div tamanho k
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

-- Padronização das bases através de z-score
padroniza_bases :: ([Registro], [Registro]) -> ([Registro], [Registro])
padroniza_bases (base_treino, base_teste) = (map padroniza base_treino, map padroniza base_teste)
                                            where
                                                padroniza x = (divide_listas ( subtrai_listas (fst x) media ) desvio, snd x)
                                                media = ponto_medio_lista atributos_treino
                                                desvio = desvio_padrao_listas media atributos_treino
                                                atributos_treino = map fst base_treino