module FisHs.Core where
import FisHs.Types

sumatoria :: [Double] -> Double
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- Valor

sumaValores :: Valor -> Valor -> Valor
sumaValores (Valor a) (Valor b) = Valor (a + b)

restaValores :: Valor -> Valor -> Valor
restaValores (Valor a) (Valor b) = Valor (a - b)

escalar :: Double -> Valor -> Valor
escalar 0 (Valor a) = Valor 0
escalar lambda (Valor a) = Valor (lambda * a)
