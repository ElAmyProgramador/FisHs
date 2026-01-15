module FisHs.Core where

sumatoria :: [Double] -> Double
sumatoria = foldr (+) 0

longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs
