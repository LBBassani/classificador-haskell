module Random (randomList) where
import System.Random

{-
    Modulo do primeiro trabalho computacional de Programação Funcional em Haskell responsável pela 
      geração de lista randomica.
-}

tem :: Eq a => a -> [a] -> Bool
tem _ [] = False
tem y (x:xs)
   | x == y = True
   | otherwise = tem y xs

removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
     removeD [] _ = []
     removeD (x:xs) ls
        | tem x ls = removeD xs ls
        | otherwise = x: removeD xs (x:ls)

randomList:: Int -> Int -> Int -> [Int]
randomList n s limite = take n $ removeDup  $ randomRs (1,limite) g :: [Int]
                        where
                          g = mkStdGen s