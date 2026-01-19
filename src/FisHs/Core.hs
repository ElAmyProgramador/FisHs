module FisHs.Core where
import FisHs.Types

sumatoria :: [Double] -> Double
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- Artimetica

sumaV :: Valor -> Valor -> Valor
sumaV (Valor a) (Valor b) = Valor (a + b)

restaV :: Valor -> Valor -> Valor
restaV (Valor a) (Valor b) = Valor (a - b)

multV :: Valor -> Valor -> Valor
multV (Valor a) (Valor b) = Valor (a * b)

escalar :: Double -> Valor -> Valor
escalar 0 (Valor _) = Valor 0
escalar lambda (Valor a) = Valor (lambda * a)

divV :: Valor -> Valor -> Maybe Valor
divV (Valor _) (Valor 0) = Nothing
divV (Valor a) (Valor b) = Just $ Valor (a / b)

listaV :: [Double] -> [Valor]
listaV [] = []
listaV l = map (\x -> Valor x) l

listaM :: [Double] -> Muestra
listaM [] = Muestra []
listaM l = Muestra $ listaV l

sumatoriaV :: [Valor] -> Valor
sumatoriaV [] = Valor 0
sumatoriaV (x:xs) = sumaV x (sumatoriaV xs)

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
