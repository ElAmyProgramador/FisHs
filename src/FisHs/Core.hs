module FisHs.Core where
import FisHs.Types

sumatoria :: [Double] -> Double
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- Artimetica

sumaValores :: Valor -> Valor -> Valor
sumaValores (Valor a) (Valor b) = Valor (a + b)

restaValores :: Valor -> Valor -> Valor
restaValores (Valor a) (Valor b) = Valor (a - b)

multV :: Valor -> Valor -> Valor
multV (Valor a) (Valor b) = Valor (a * b)

escalar :: Double -> Valor -> Valor
escalar 0 (Valor _) = Valor 0
escalar lambda (Valor a) = Valor (lambda * a)

divValores :: Valor -> Valor -> Maybe Valor
divValores (Valor _) (Valor 0) = Nothing
divValores (Valor a) (Valor b) = Just $ Valor (a / b)

listaValor :: [Double] -> [Valor]
listaValor [] = []
listaValor l = map (\x -> Valor x) l

sumatoriaV :: [Valor] -> Valor
sumatoriaV [] = Valor 0
sumatoriaV (x:xs) = sumaValores x (sumatoriaV xs)

cuadradoV :: Valor -> Valor
cuadradoV (Valor x) = Valor (x * x)

sqrtV :: Valor -> Valor
sqrtV (Valor a) = Valor (sqrt a)

absV :: Valor -> Valor
absV (Valor a) = Valor $ abs a

-- Comparación

maxV :: Valor -> Valor -> Valor
maxV (Valor a) (Valor b)
    | a > b = Valor a
    | otherwise = Valor b

minV :: Valor -> Valor -> Valor
minV (Valor a) (Valor b)
    | a < b = Valor a
    | otherwise = Valor b
